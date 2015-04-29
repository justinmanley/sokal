module Spew where

import Data.Array
import System.Random
import Control.Monad.Trans.State.Lazy
import Control.Monad.Writer.Lazy

type Id = Int
type Rand = State StdGen
type FastModel = Array Int (String, [(Int, Int)])

parseModel :: String -> FastModel
parseModel str = array size $ zipWith parseState [1..] states where
    states = lines str
    size = (1, length states)
    parseState i str = (i, read str)

select :: [(Int, b)] -> Rand b
select xs = 
    let frequencies = fst . unzip $ xs
        size = (1, sum frequencies)
        ids = concatMap (\(freq, id) -> replicate freq id) xs
    in do
        rand <- state $ randomR size
        return $ (listArray size ids) ! rand



genText :: Id -> FastModel -> WriterT String Rand Int 
genText start model = iter start where
    iter state = undefined --do
--        let (word, successors) = model ! word
--        succ <- select successors
        
--        tell word

        

             
spew :: IO ()
spew = do
    putStrLn "hello"
