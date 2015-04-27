module Suck (primitiveModel, suck) where

import Control.Applicative ((<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.HandsomeSoup (css)

type PrimitiveModel = Map (String, String) [String]
type ProcessedModel = [(String, [(Int, Int)])]

{-mkPrimitiveModel :: [String] -> PrimitiveModel-}
{-mkPrimitiveModel pages = do-}
    {-map (readString [withParseHTML yes]) pages-}

primitiveModel page = page 
    >>> css "#body"
    //> getText

suck :: IO ()
suck = do
    results <- mapM (runX . primitiveModel . readDocument [withParseHTML yes, withHTTP [], withWarnings no, withRedirect yes]) =<< 
        lines <$> readFile "urls.txt"
    putStrLn "hello"
