{-# LANGUAGE ViewPatterns #-}

module Spew where

import Data.Array
import System.Random
import Control.Monad.Trans.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Applicative ((<$>))

type Id = Int
type Rand = State StdGen
type FastModel = Array Int (String, [(Int, Int)])

parseModel :: String -> FastModel
parseModel str = array size $ zipWith parseState [1..] states where
    states = lines str
    size = (1, length states)
    parseState i str = (i, read str)

-- Pick a value at random from a list
select :: [a] -> Rand a
select xs = fmap (xs !!) . state . randomR $ (0, length xs - 1)

-- Pick a value from a list of (frequency, value) pairs
frequency :: [(Int, b)] -> Rand b
frequency xs = 
    let frequencies = fst . unzip $ xs
        ids = concatMap (\(freq, id) -> replicate freq id) xs
    in do
        rand <- state $ randomR (0, sum frequencies - 1)
        return $ ids !! rand

-- Can we reasonably end on this token?
isTerminal :: String -> Bool
isTerminal s = case s of
    "." -> True
    _   -> False

genText :: Int -> FastModel -> WriterT String Rand () 
genText len model = iter len =<< start where
    start = (fst . randomR (bounds model)) <$> lift get 
    iter len ((!) model -> (word, successors))  
        | len < 0 && isTerminal word = tell word 
        | otherwise = do
            tell word -- emit a word
            tell " "
        
            case successors of
                [] -> iter (len - 1)  =<< lift (select $ indices model)
                _  -> iter (len - 1) =<< lift (frequency successors)

spew :: IO ()
spew = do
    model <- parseModel <$> readFile "sokal.model"
    gen <- getStdGen

    writeFile "sokal.output" 
        $ (flip evalState $ gen) . execWriterT . genText 1000
        $ model 
  

