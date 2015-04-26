module Suck where

import Control.Applicative ((<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Map (Map, (!))
import qualified Data.Map as Map

type PrimitiveModel = Map (String, String) [String]
type ProcessedModel = [(String, [(Int, Int)])]

{-mkPrimitiveModel :: [String] -> PrimitiveModel-}
{-mkPrimitiveModel pages = do-}
    {-map (readString [withParseHTML yes]) pages-}

suck :: IO ()
suck = do
    results <- mapM (runX . readDocument [withParseHTML yes, withHTTP []]) =<< 
        lines <$> readFile "urls.txt"
    putStrLn "hello"
