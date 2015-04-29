module Spew where

import Data.Array

type FastModel = Array Int (String, [(Int, Int)])

parseModel :: String -> FastModel
parseModel str = array size $ zipWith parseState [1..] states where
    states = lines str
    size = (1, length states)

    parseState :: Int -> String -> (Int, (String, [(Int, Int)]))
    parseState i str = undefined 

spew :: IO ()
spew = do
    putStrLn "hello"
