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

select :: [a] -> Rand a
select xs = fmap (xs !!) . state . randomR $ (0, length xs - 1)

frequency :: [(Int, b)] -> Rand b
frequency xs = 
    let frequencies = fst . unzip $ xs
        ids = concatMap (\(freq, id) -> replicate freq id) xs
    in do
        rand <- state $ randomR (0, sum frequencies - 1)
        return $ ids !! rand

genText :: Id -> FastModel -> WriterT String Rand Int 
genText start model = iter start where
    iter state = do
        let (word, successors) = model ! state
        tell word -- emit a word
    
        case successors of
            [] -> iter =<< lift (select $ indices model)
            _  -> iter =<< lift (frequency successors)

spew :: IO ()
spew = do
    model <- parseModel <$> readFile "sokal.model"
    gen <- getStdGen
    let start = fst $ randomR (bounds model) gen 

    putStrLn "hello"
    writeFile "sokal.output" 
        $ (flip evalState $ gen) . execWriterT . genText start $ model  
