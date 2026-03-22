-- Temporary debug: test isComplexBinding for overrideAnnotation
module Main where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Lexical as Lexical
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  -- Simulate what overrideAnnotation's binding looks like
  let name = Core.Name "hydra.ext.java.utils.overrideAnnotation"
  -- A simple application term (not a literal, not a variable)
  let term = Core.TermApplication (Core.Application
        (Core.TermVariable (Core.Name "hydra.ext.java.syntax.AnnotationMarker"))
        (Core.TermVariable (Core.Name "someArg")))
  let binding = Core.Binding name term Nothing

  -- Create a minimal graph
  let graph = Graph.Graph
        { Graph.graphBoundTerms = M.singleton name term
        , Graph.graphBoundTypes = M.empty
        , Graph.graphClassConstraints = M.empty
        , Graph.graphLambdaVariables = S.empty
        , Graph.graphMetadata = M.empty
        , Graph.graphPrimitives = M.empty
        , Graph.graphSchemaTypes = M.empty
        , Graph.graphTypeVariables = S.empty
        }

  putStrLn $ "isComplexBinding: " ++ show (CoderUtils.isComplexBinding graph binding)
  putStrLn $ "isTrivialTerm: " ++ show (CoderUtils.isTrivialTerm term)

  -- Check lookupElement
  case Lexical.lookupElement graph name of
    Nothing -> putStrLn "lookupElement: Nothing"
    Just el -> do
      putStrLn $ "lookupElement: Found"
      putStrLn $ "  el.type: " ++ show (Core.bindingType el)
