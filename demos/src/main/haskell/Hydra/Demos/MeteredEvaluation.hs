-- | A small demo of "metered" Hydra evaluation. In this example, the evaluator keeps track of the number of times it
--   calls each primitive function (as a proxy for API calls, which can consume time and resources).

module Hydra.Demos.MeteredEvaluation (demoMeteredEvaluation) where

import Hydra.Kernel
import qualified Hydra.Show.Errors as ShowError
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.Meta.Lib.Lists as Lists
import Hydra.Dsl.Meta.Lib.Strings as Strings
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import Hydra.ExtGeneration
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings

import System.IO
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Prelude hiding ((++))


testNs = Namespace "hydra.demos.meteredEvaluation"

testModule :: Module
testModule = Module {
               moduleNamespace = testNs,
               moduleDefinitions = definitions,
               moduleTermDependencies = [],
               moduleTypeDependencies = [],
               moduleDescription = Nothing}
  where
    test local tterm = TTermDefinition (unqualifyName $ QualifiedName (Just testNs) local) tterm
    definitions = [
        toDefinition $ test "catStrings" (string "foo" ++ string "bar" ++ string "quux" ++ (Literals.showInt32 $ int32 42)),
        toDefinition $ test "describeType" $ ShowCore.type_ @@ (TTerm $ EncodeCore.type_ $ Types.list $ Types.int32)]

demoMeteredEvaluation :: IO ()
demoMeteredEvaluation = do
    case result of
      Left err -> putStrLn $ "error: " <> ShowError.error err
      Right reduced -> putStrLn $ "result: " <> show reduced
  where
    graph = modulesToGraph [testModule] [testModule]
    cx = emptyContext
    result = do
      original <- bindingTerm <$> requireBinding graph (unqualifyName $ QualifiedName (Just testNs) "catStrings")
      reduceTerm cx graph False original
