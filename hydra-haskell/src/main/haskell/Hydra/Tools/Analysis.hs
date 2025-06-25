-- | A module for static analysis of Hydra graphs and programs

{-
:m
import Hydra.Kernel
import Hydra.Tools.Analysis
import qualified Data.List as L
import Hydra.Sources.Tier0.Core
import Hydra.Tools.Monads
import Hydra.Codegen
import Hydra.Sources.Tier1.All
import Hydra.Sources.Tier2.All

-- Create an exhaustive list of Hydra kernel elements and primitives for code generation
fromFlowIo hydraCoreGraph (graphSummary True $ modulesToGraph kernelModules) >>= putStrLn
-}
module Hydra.Tools.Analysis where

import Hydra.Kernel
import Hydra.Codegen
import Hydra.Sources.Tier2.All
import Hydra.Sources.Libraries
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Describe.Core as DescribeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Show.Core as ShowCore

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Maybe as Y


elementSummary :: Bool -> Element -> Flow Graph String
elementSummary withTypes el = do
    mt <- findTypeInfo
    return $ (unName $ elementName el) ++ Y.fromMaybe "" mt
  where
    findTypeInfo = if withTypes
      then case elementType el of
        Nothing -> return Nothing
        Just ts -> Just <$> if EncodeCore.isType (stripType $ typeSchemeType ts)
          then do
            typ <- stripType <$> (DecodeCore.type_ $ elementTerm el)
            return $ " = " ++ ShowCore.type_ typ
          else pure $ " : " ++ ShowCore.typeScheme ts
      else pure Nothing

elementsInModules :: [Module] -> [Element]
elementsInModules mods = L.sortBy (O.comparing elementName) $ L.concat $ fmap moduleElements mods

elementsInKernelModules :: [Element]
elementsInKernelModules = elementsInModules kernelModules

fieldSummary :: Field -> String
fieldSummary = unName . fieldName

fieldTypeSummary :: FieldType -> String
fieldTypeSummary (FieldType fname ftype) = unName fname ++ " : " ++ ShowCore.type_ ftype

graphSummary :: Bool -> Graph -> Flow Graph String
graphSummary withTypes g = do
  gi <- if withTypes then inferGraphTypes g else pure g
  let els = L.sortBy (O.comparing elementName) $ M.elems $ graphElements gi
  let prims = L.sortBy (O.comparing primitiveName) $ M.elems $ graphPrimitives gi
  elSummaries <- CM.mapM (elementSummary withTypes) els
  let primSummaries = fmap (primitiveSummary withTypes) prims
  return $ "Elements:\n" ++ showSummaries elSummaries ++ "\nPrimitives:\n" ++ showSummaries primSummaries

primitiveSummary :: Bool -> Primitive -> String
primitiveSummary withType prim = unName (primitiveName prim)
  ++ if withType
     then (" : " ++ ShowCore.typeScheme (primitiveType prim))
     else ""

primitivesInLibraries :: [Library] -> [Primitive]
primitivesInLibraries libs = L.sortBy (O.comparing primitiveName) $ L.concat $ fmap libraryPrimitives libs

primitivesInStandardLibraries :: [Primitive]
primitivesInStandardLibraries = primitivesInLibraries standardLibraries

showSummaries sums = L.unlines $ fmap (\s -> "  " ++ s) $ L.concat $ fmap L.lines sums
