module Main where

import EvilTest
import Data.Char

main :: IO ()
main = do
    file <- readFile "testo.txt"
    putStrLn . censura $ file

censura :: String -> String
censura = unwords . (fmap cambiaparola) . words

cambiaparola :: String -> String
cambiaparola parola = if elem (rimuoviNonAlpfanumerici parola) paroleVietate then fmap sostituisciX parola else parola

rimuoviNonAlpfanumerici :: String -> String
rimuoviNonAlpfanumerici parola = filter (\c -> isAlpha c) parola

sostituisciX :: Char -> Char
sostituisciX carattere = if isAlpha carattere then 'X' else carattere

paroleVietate :: [String]
paroleVietate = [ "ciao", "bye" ]
