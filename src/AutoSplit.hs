{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module AutoSplit
  ( plugin
  ) where

import           Control.Exception
import           Data.Functor
import           Data.IORef
import           Data.String (IsString, fromString)
import qualified Data.Typeable as Typeable
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.GHC.ExactPrint.Utils as EP

import qualified AutoSplit.Fields as Fields
import qualified AutoSplit.GhcFacade as Ghc
import qualified AutoSplit.Split as Split

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = \_ hscEnv -> pure $ addDsHook hscEnv
  , Ghc.parsedResultAction = \_ _ result -> pure $ addImport result
  , Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.typeCheckResultAction = \_ _ env ->
      removeUnusedImportWarn >> pure env
  }

-- | Incomplete patterns warning is emitted by the desugarer. Some shenanigans
-- are needed to intercept these warnings: a custom error is added to the
-- frontend result which causes the desugarer to throw a 'SourceError'
-- exception containing all diagnostics after running. This exception is then
-- caught (and later rethrown) by the plugin.
addDsHook :: Ghc.HscEnv -> Ghc.HscEnv
addDsHook hscEnv = hscEnv
  { Ghc.hsc_hooks =
      let hooks = Ghc.hsc_hooks hscEnv
       in hooks
          { Ghc.runPhaseHook = Just $ phaseHook (Ghc.runPhaseHook hooks) }
  }
  where
    phaseHook mExistingHook = Ghc.PhaseHook $ \tPhase -> do
      let isMissingPatWarn msgEnv =
            case Ghc.errMsgDiagnostic msgEnv of
              Ghc.GhcDsMessage (Ghc.DsNonExhaustivePatterns _ _ _ patIds nablas)
                | Just srcSpan <- Ghc.srcSpanToRealSrcSpan (Ghc.errMsgSpan msgEnv) ->
                    Left Split.NonExhaustivePatterns
                      { Split.patternIds = patIds
                      , Split.patternNablas = nablas
                      , Split.srcCodeLoc = srcSpan
                      }
              _ -> Right msgEnv
          isMissingFieldsErr msgEnv =
            case Ghc.errMsgDiagnostic msgEnv of
              Ghc.GhcTcRnMessage (Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnMissingFields conLike _fields)))
                | Just srcSpan <- Ghc.srcSpanToRealSrcSpan (Ghc.errMsgSpan msgEnv) ->
                    Left Fields.MissingFields
                      { Fields.conLike = conLike
                      , Fields.srcCodeLoc = srcSpan
                      }
              Ghc.GhcTcRnMessage (Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnMissingStrictFields conLike _fields)))
                | Just srcSpan <- Ghc.srcSpanToRealSrcSpan (Ghc.errMsgSpan msgEnv) ->
                    Left Fields.MissingFields
                      { Fields.conLike = conLike
                      , Fields.srcCodeLoc = srcSpan
                      }
              _ -> Right msgEnv
          isAutoSplitError msgEnv =
            case Ghc.errMsgDiagnostic msgEnv of
              Ghc.GhcUnknownMessage (Ghc.UnknownDiagnostic' a)
                | Just PatternSplitDiag <- Typeable.cast a -> True
              _ -> False
          parseFailedErr filePath =
            Ghc.mkPlainErrorMsgEnvelope
              (Ghc.mkGeneralSrcSpan $ fromString filePath)
              (Ghc.ghcUnknownMessage ParseFailed)
          patSplitErr filePath =
            Ghc.mkPlainErrorMsgEnvelope
              (Ghc.mkGeneralSrcSpan $ fromString filePath)
              (Ghc.ghcUnknownMessage PatternSplitDiag)
          improperUseErr filePath =
            Ghc.mkPlainErrorMsgEnvelope
              (Ghc.mkGeneralSrcSpan $ fromString filePath)
              (Ghc.ghcUnknownMessage ImproperUse)
          runPhaseOrExistingHook :: Ghc.TPhase res -> IO res
          runPhaseOrExistingHook = maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h) mExistingHook
      case tPhase of
        Ghc.T_HscPostTc env modSum tcResult@(Ghc.FrontendTypecheck gblEnv) warns mOldHash -> do
          usedGres <- readIORef $ Ghc.tcg_used_gres gblEnv
          let usesSplit = any ((== Split.splitName) . Ghc.occNameFS . Ghc.occName . Ghc.gre_name) usedGres
              mFilePath = Ghc.ml_hs_file (Ghc.ms_location modSum)
          case mFilePath of
            Just filePath | usesSplit -> do
              let noMissingPatErr =
                    Ghc.mkPlainErrorMsgEnvelope
                      (Ghc.mkGeneralSrcSpan $ fromString filePath)
                      (Ghc.ghcUnknownMessage NoMissingPat)
                  warnsWithError = Ghc.addMessage (patSplitErr filePath) warns
                  updEnv = env
                    -- Plugin only functions if incomplete patterns warning is enabled, so we force it on.
                    -- If this is instead done as driver plugin, ghci sessions won't pick it up.
                    { Ghc.hsc_dflags = (Ghc.hsc_dflags env `Ghc.wopt_set` Ghc.Opt_WarnIncompletePatterns)
                      -- Need to override the number of uncovered patterns reported.
                      { Ghc.maxUncoveredPatterns = maxBound - 1 }
                    }
              catch
                (runPhaseOrExistingHook $ Ghc.T_HscPostTc updEnv modSum tcResult warnsWithError mOldHash)
                (\(Ghc.SourceError msgs) -> do
                  let (missingPatWarns, otherDiags) = Ghc.partitionBagWith isMissingPatWarn (Ghc.getMessages msgs)
                      -- Need to parse the module because GHC removes
                      -- ParsedResult from ModSummary after the frontend finishes.
                      -- Use the options from compilation to parse the module, otherwise certain
                      -- syntax extensions won't parse correctly.
                      dynFlags = Ghc.ms_hspp_opts modSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
                  eResult <- parseModule env dynFlags filePath
                  case eResult of
                    (Right parsedMod, usesCpp)
                      | not (null missingPatWarns) -> do
                          let gblRdrEnv = Ghc.tcg_rdr_env gblEnv
                          updated <- Split.modifyModule gblRdrEnv parsedMod usesCpp missingPatWarns filePath
                          if updated
                             then throw . Ghc.SourceError $ Ghc.mkMessages otherDiags
                             else throw . Ghc.SourceError . Ghc.mkMessages
                                   . Ghc.consBag (improperUseErr filePath)
                                   $ Ghc.filterBag (not . isAutoSplitError) otherDiags
                      | otherwise -> throw . Ghc.SourceError . Ghc.mkMessages
                                   . Ghc.consBag noMissingPatErr
                                   $ Ghc.filterBag (not . isAutoSplitError) otherDiags
                    (Left _, _) ->
                      throw . Ghc.SourceError . Ghc.mkMessages
                            . Ghc.consBag (parseFailedErr filePath)
                            $ Ghc.filterBag (not . isAutoSplitError) otherDiags
                )
            _ -> runPhaseOrExistingHook tPhase -- no SPLIT or no file path

        -- Missing fields
        Ghc.T_Hsc env modSum -> do
          gblEnvRef <- newIORef Nothing :: IO (IORef (Maybe Ghc.TcGblEnv))
          let rnPlugin =
                Ghc.defaultPlugin
                { Ghc.renamedResultAction = \_ gblEnv group -> do
                    Ghc.liftIO $ writeIORef gblEnvRef (Just gblEnv)
                    pure (gblEnv, group)
                }
              staticPlugin = Ghc.StaticPlugin
                { Ghc.spPlugin = Ghc.PluginWithArgs rnPlugin []
#if MIN_VERSION_ghc(9,12,0)
                , Ghc.spInitialised = True
#endif
                }
              envWithPlugin = env
                { Ghc.hsc_plugins = let ps = Ghc.hsc_plugins env in ps
                  { Ghc.staticPlugins = staticPlugin : Ghc.staticPlugins ps }
                }
          eTcRes <- try . runPhaseOrExistingHook $ Ghc.T_Hsc envWithPlugin modSum
          let msgs = case eTcRes of
                Right (_res, m) -> m
                Left (Ghc.SourceError m) -> m
              result = case eTcRes of
                Right res -> pure res
                Left err -> throw err
          mGblEnv <- readIORef gblEnvRef
          case mGblEnv of
            Nothing -> result -- renaming failed
            Just gblEnv -> do
              usedGres <- readIORef $ Ghc.tcg_used_gres gblEnv
              let usesFields = any ((== Fields.fieldsName) . Ghc.occNameFS . Ghc.occName . Ghc.gre_name) usedGres
                  mFilePath = Ghc.ml_hs_file (Ghc.ms_location modSum)
                  (missingFields, otherDiags) = Ghc.partitionBagWith isMissingFieldsErr (Ghc.getMessages msgs)
              case mFilePath of
                Just filePath | usesFields, not (null missingFields) -> do
                  -- Parse module
                  let dynFlags = Ghc.ms_hspp_opts modSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
                  eParseRes <- parseModule env dynFlags filePath
                  case eParseRes of
                    (Right parsedMod, usesCpp) -> do
                      let gblRdrEnv = Ghc.tcg_rdr_env gblEnv
                      updated <- Fields.modifyModule gblRdrEnv parsedMod usesCpp missingFields filePath
                      if updated
                         then throw . Ghc.SourceError . Ghc.mkMessages
                                $ Ghc.consBag (patSplitErr filePath) otherDiags
                         else throw . Ghc.SourceError . Ghc.mkMessages
                                $ Ghc.consBag (improperUseErr filePath) otherDiags
                    (Left _, _) ->
                      throw . Ghc.SourceError . Ghc.mkMessages
                            $ Ghc.consBag (parseFailedErr filePath) otherDiags
                _ -> result -- no FIELDS or no file path

        _ -> runPhaseOrExistingHook tPhase

-- | Parse the given module file. Accounts for CPP comments
parseModule
  :: Ghc.HscEnv
  -> Ghc.DynFlags
  -> String
  -> IO (EP.ParseResult Ghc.ParsedSource, Bool)
parseModule env dynFlags filePath = EP.ghcWrapper Paths.libdir $ do
  Ghc.setSession env { Ghc.hsc_dflags = dynFlags }
  res <- EP.parseModuleEpAnnsWithCppInternal EP.defaultCppOptions dynFlags filePath
  let eCppComments = fmap (\(c, _, _) -> c) res
      hasCpp = case eCppComments of
                 Right cs -> not $ null cs
                 _ -> False
  pure
    ( liftA2 EP.insertCppComments
        (EP.postParseTransform res)
        eCppComments
    , hasCpp
    )

-- | Diagnostic thrown when case splitting should be attempted.
data PatternSplitDiag = PatternSplitDiag

instance Ghc.Diagnostic PatternSplitDiag where
  type DiagnosticOpts PatternSplitDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "Module updated by auto-split, compilation aborted"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

-- | Diagnostic thrown when SPLIT is used but there are no resulting warnings
data NoMissingPat = NoMissingPat

instance Ghc.Diagnostic NoMissingPat where
  type DiagnosticOpts NoMissingPat = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "Module was not updated because all cases are already covered where SPLIT occurs"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

-- | Diagnostic thrown when SPLIT is used but there are no resulting warnings
data ImproperUse = ImproperUse

instance Ghc.Diagnostic ImproperUse where
  type DiagnosticOpts ImproperUse = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "It appears that an auto-split pattern was used improperly. The module has not been updated."
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

-- | Diagnostic thrown if the module fails to parse
data ParseFailed = ParseFailed

instance Ghc.Diagnostic ParseFailed where
  type DiagnosticOpts ParseFailed = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "auto-split failed to parse the module"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

-- | Adds the import for SPLIT to the module being compiled. Otherwise users
-- would have to manually add this import everytime they want to do pattern splitting.
addImport :: Ghc.ParsedResult -> Ghc.ParsedResult
addImport result = result
  { Ghc.parsedResultModule =
    let resMod = Ghc.parsedResultModule result
     in resMod
        { Ghc.hpm_module = Ghc.hpm_module resMod <&> \pMod ->
            pMod
              { Ghc.hsmodImports =
                  patternImport : Ghc.hsmodImports pMod
              }
        }
  }
  where
    patternImport = Ghc.noLocA . Ghc.simpleImportDecl $ Ghc.mkModuleName patternModName

-- | The automatically added import gets flagged as unused even if it is used.
-- The solution here is to simply suppress the warning.
removeUnusedImportWarn :: Ghc.TcM ()
removeUnusedImportWarn = do
  errsVar <- Ghc.getErrsVar
#if MIN_VERSION_ghc(9,8,0)
  let isAutoSplitImportWarn msgEnv =
        case Ghc.errMsgDiagnostic msgEnv of
          Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnUnusedImport decl _)) ->
            Ghc.unLoc (Ghc.ideclName decl) == Ghc.mkModuleName patternModName
          _ -> False
  Ghc.liftIO . modifyIORef errsVar $
    Ghc.mkMessages . Ghc.filterBag (not . isAutoSplitImportWarn) . Ghc.getMessages
#else
  -- 9.6 lacks the specific diagnostic
  let isAutoSplitImportWarn msgEnv =
        case Ghc.errMsgDiagnostic msgEnv of
          Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnUnknownMessage (Ghc.UnknownDiagnostic diag)))
            | Ghc.WarningWithFlag Ghc.Opt_WarnUnusedImports <- Ghc.diagnosticReason diag
            -> True
          _ -> False
  Ghc.liftIO . modifyIORef errsVar $ \msgs ->
    -- the target import warning always shows up as the last occurrence
    case break isAutoSplitImportWarn . reverse . Ghc.bagToList $ Ghc.getMessages msgs of
      (before, _ : after) -> Ghc.mkMessages . Ghc.listToBag . reverse $ before ++ after
      _ -> msgs
#endif

patternModName :: IsString a => a
patternModName = "AutoSplit.Pattern"
