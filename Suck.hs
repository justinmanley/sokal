module Suck (getArticleBody, suck, hxtConfig) where

import Control.Applicative ((<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Map (Map, (!))
import qualified Data.Map as Map hiding (foldr)
import Text.HandsomeSoup (css)
import Data.List (group, sort, intersperse)
import Data.Maybe

type Id = Int

type Frequency = Int

type PrimitiveModel = Map (String, String) [String]
type ProcessedModel = [(String, [(Frequency, Id)])]

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

processModel :: PrimitiveModel -> ProcessedModel 
processModel prim = map processState (Map.toList prim) where
    processState :: ((String, String), [String]) -> (String, [(Frequency, Id)])
    processState ((s1, s2), xs) = 
        ( s2
        , map swap . Map.toList . (Map.mapKeys $ stamp s2) 
            $ foldr frequency Map.empty xs )

    -- All terminal states are mapped to the same integer id
    stamp current str = fromMaybe terminalId $ Map.lookup (current, str) ids

    frequency s freqs = Map.insertWith' (+) s 1 freqs 
    ids = Map.fromList $ zip (Map.keys prim) [1..]
    terminalId = length $ Map.keys prim

suck :: IO ()
suck = do
    results <- mapM ((fmap $ concatMap words) . runX . getArticleBody . readDocument hxtConfig) 
        =<< lines <$> readFile "urls.txt"
    let modelPrim = foldr (Map.unionWith (++)) Map.empty (map (primitiveModel Map.empty) results)

    writeFile "sokal.model"
        $ concat . intersperse "\n"
        $ map show . processModel 
        $ modelPrim

