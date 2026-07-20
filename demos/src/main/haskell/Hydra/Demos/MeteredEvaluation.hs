-- | A small demo of "metered" Hydra evaluation. In this example, the evaluator keeps track of the number of times it
--   calls each primitive function (as a proxy for API calls, which can consume time and resources).

{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Demos.MeteredEvaluation (demoMeteredEvaluation) where

import Hydra.Kernel
import qualified Hydra.Print.Errors as PrintError
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types as Types
import Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import Hydra.ExtGeneration
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Lib.Strings as Strings

import System.IO
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Prelude hiding ((++))


testNs = ModuleName "hydra.demos.meteredEvaluation"

testModule :: Module
testModule = Module {
               moduleName = testNs,
               moduleDefinitions = definitions,
               moduleDependencies = [] :: [ModuleDependency],
               moduleMetadata = Nothing}
  where
    test local tterm = TypedTermDefinition (unqualifyName $ QualifiedName (Just testNs) local) tterm
    definitions = [
        toDefinition $ test "catStrings" (string "foo" ++ string "bar" ++ string "quux" ++ (Literals.showInt32 $ int32 42)),
        toDefinition $ test "describeType" $ PrintCore.type_ @@ (TypedTerm $ EncodeCore.type_ $ Types.list $ Types.int32)]

demoMeteredEvaluation :: IO ()
demoMeteredEvaluation = do
    case result of
      Left err -> putStrLn $ "error: " <> PrintError.error err
      Right reduced -> putStrLn $ "result: " <> show reduced
  where
    graph = modulesToGraph [testModule] [testModule]
    cx = emptyInferenceContext
    result = do
      original <- bindingTerm <$> requireBinding graph (unqualifyName $ QualifiedName (Just testNs) "catStrings")
      reduceTerm cx graph False original
