module Suck (getArticleBody, suck, hxtConfig) where

import Control.Applicative ((<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Map (Map, (!))
import qualified Data.Map as Map hiding (foldr)
import Text.HandsomeSoup (css)
import Data.List (group, sort)

type PrimitiveModel = Map (String, String) [String]
newtype ProcessedModel = ProcessedModel [(String, [(Int, Int)])]

instance Show ProcessedModel where
    show (ProcessedModel states) = concatMap (\(str, xs) 
        -> str ++ concatMap show xs ++ "\n") states

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

-- Need to create a map from (String, String) states to Int ids.
-- Alternatively - what happens to states that are not keys in the dictionary?
processModel :: PrimitiveModel -> ProcessedModel
processModel prim = ProcessedModel $ map processState (Map.toList prim) where
    processState :: ((String, String), [String]) -> (String, [(Int, Int)]) 
    processState ((_, x), xs) = (x, zip (map length . group . sort $ xs) [1..])

suck :: IO ()
suck = do
    results <- mapM ((fmap $ concatMap words) . runX . getArticleBody . readDocument hxtConfig) 
        =<< lines <$> readFile "urls.txt"
    let modelPrim = foldr (Map.unionWith (++)) Map.empty (map (primitiveModel Map.empty) results)

    writeFile "sokal.model"
        $ show . processModel 
        $ modelPrim
