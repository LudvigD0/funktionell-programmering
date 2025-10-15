module Main where

import Prelude hiding (lookup)
-- import TableList
import TableTree

type Name   = String
type Number = Int

phoneBook :: Table Name Number
phoneBook = insert "lise" 2351236
          $ insert "sofie" 342
          $ insert "mats" 1234
          $ empty

main :: IO ()
main = do
  putStrLn "Welcome to Telia!\n"
  putStr "Who are you looking for?\n> "
  name <- getLine
  putStrLn $ case lookup name phoneBook of
    Just number -> name ++ " has number " ++ show number
    _           -> name ++ " is not in the phone book :-("
