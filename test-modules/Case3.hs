module Case3 where

main :: IO ()
main = pure ()

test :: Maybe Bool -> Bool -> Bool
test x y = case x of
  Nothing ->
    case y of
         False -> False
         True -> False
  Just _ -> True
