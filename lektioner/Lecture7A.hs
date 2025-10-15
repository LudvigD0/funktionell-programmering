import Prelude hiding (lookup)
import TableList


type Name = String
type Number = Int

phoneBook :: Table Name Number
phoneBook = insert "lise" 23423 $ insert "sofie" 322 $ empty

main :: IO ()
main = do
    putStrLn "Welcome to Telia \n"
    putStr "Who are you looking for? \n"
    name <-


lookup :: Ord k => k -> Table k v -> Maybe v
lookup key t = case t of
    Empty    -> Nothing
    Node l k v r
      | key == k -> Just v