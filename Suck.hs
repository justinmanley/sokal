module Suck (getArticleBody, suck, hxtConfig) where

import Control.Applicative ((<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Map (Map, (!))
import qualified Data.Map as Map hiding (foldr)
import Text.HandsomeSoup (css)

type PrimitiveModel = Map (String, String) [String]
type ProcessedModel = [(String, [(Int, Int)])]

hxtConfig :: [SysConfig]
hxtConfig = 
    [ withParseHTML yes
    , withHTTP []
    , withWarnings no
    , withRedirect yes ]

getArticleBody page = page 
    >>> css "#body"
    //> getText

primitiveModel :: PrimitiveModel -> [String] -> PrimitiveModel
primitiveModel model [] = model
primitiveModel model [x] = model
primitiveModel model [x,y] = model
primitiveModel model (x1:x2:x3:xs) = primitiveModel newModel (x2:x3:xs) where
    newModel = Map.insertWith (++) (x1, x2) [x3] model    

--processModel :: PrimitiveModel -> ProcessedModel
--processModel prim =  

suck :: IO ()
suck = do
    results <- mapM ((fmap $ concatMap words) . runX . getArticleBody . readDocument hxtConfig) 
        =<< lines <$> readFile "urls.txt"
    let modelPrim = foldr (Map.unionWith (++)) Map.empty (map (primitiveModel Map.empty) results)
    putStrLn "hello"
