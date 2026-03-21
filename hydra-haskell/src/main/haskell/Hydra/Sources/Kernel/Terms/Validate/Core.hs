
module Hydra.Sources.Kernel.Terms.Validate.Core where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  binding, elimination, field, fields, fieldType, floatType, floatValue, function, injection, integerType,
  integerValue, lambda, literal, literalType, term, type_, typeScheme)
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import qualified Hydra.Dsl.Errors            as Error
import qualified Hydra.Dsl.Error.Core       as ErrorsCore
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting


ns :: Namespace
ns = Namespace "hydra.validate.core"

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns]
    kernelTypesNamespaces $
    Just "Validation functions for core terms"
  where
   elements = [
     toBinding checkDuplicateBindings,
     toBinding checkDuplicateFields,
     toBinding checkTerm,
     toBinding findDuplicate,
     toBinding term]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- | Validate a term, checking for duplicate binding names and duplicate field names.
-- Returns Nothing if the term is valid, or Just an InvalidTermError describing the first error found.
-- | A Nothing of type Maybe InvalidTermError
noError :: TTerm (Maybe InvalidTermError)
noError = TTerm $ TermMaybe Nothing

-- | A Just of type InvalidTermError -> Maybe InvalidTermError
justError :: TTerm InvalidTermError -> TTerm (Maybe InvalidTermError)
justError (TTerm t) = TTerm $ TermMaybe $ Just t

term :: TBinding (Graph -> Term -> Maybe InvalidTermError)
term = define "term" $
  doc "Validate a term, returning the first error found or nothing if valid" $
  "g" ~> "t" ~>
  Rewriting.foldTermWithGraphAndPath
    @@ ("recurse" ~> "path" ~> "cx" ~> "acc" ~> "trm" ~>
      -- If we already found an error, short-circuit
      Maybes.cases (var "acc")
        -- No error yet: check the current term, then recurse into subterms
        ("checkResult" <~ (checkTerm @@ (wrap _AccessorPath $ var "path") @@ var "trm") $
          Maybes.cases (var "checkResult")
            -- No error at this term: let the framework recurse into subterms
            (var "recurse" @@ noError @@ var "trm")
            -- Found an error: return it
            ("err" ~> justError (var "err")))
        -- Already have an error: propagate it
        ("_" ~> var "acc"))
    @@ var "g"
    @@ noError
    @@ var "t"

-- | Check a single term node for validation errors (without recursing into subterms)
checkTerm :: TBinding (AccessorPath -> Term -> Maybe InvalidTermError)
checkTerm = define "checkTerm" $
  doc "Check a single term node for duplicate bindings or fields" $
  "path" ~> "term" ~>
  cases _Term (var "term") (Just noError) [
    _Term_let>>: "lt" ~>
      checkDuplicateBindings @@ var "path" @@ Core.letBindings (var "lt"),
    _Term_record>>: "rec" ~>
      checkDuplicateFields @@ var "path" @@ (Lists.map
        (unaryFunction Core.fieldName)
        (Core.recordFields $ var "rec"))]

-- | Check a list of bindings for duplicate names
checkDuplicateBindings :: TBinding (AccessorPath -> [Binding] -> Maybe InvalidTermError)
checkDuplicateBindings = define "checkDuplicateBindings" $
  doc "Check for duplicate binding names in a list of bindings" $
  "path" ~> "bindings" ~>
  "names" <~ Lists.map (unaryFunction Core.bindingName) (var "bindings") $
  "dup" <~ findDuplicate @@ var "names" $
  Maybes.map ("name" ~>
    inject _InvalidTermError _InvalidTermError_duplicateBinding $
      record _DuplicateBindingError [
        _DuplicateBindingError_location>>: var "path",
        _DuplicateBindingError_name>>: var "name"])
    (var "dup")

-- | Check a list of field names for duplicates
checkDuplicateFields :: TBinding (AccessorPath -> [Name] -> Maybe InvalidTermError)
checkDuplicateFields = define "checkDuplicateFields" $
  doc "Check for duplicate field names in a list of fields" $
  "path" ~> "names" ~>
  "dup" <~ findDuplicate @@ var "names" $
  Maybes.map ("name" ~>
    inject _InvalidTermError _InvalidTermError_duplicateField $
      record _DuplicateFieldError [
        _DuplicateFieldError_location>>: var "path",
        _DuplicateFieldError_name>>: var "name"])
    (var "dup")

-- | Find the first duplicate in a list, if any
findDuplicate :: TBinding ([Name] -> Maybe Name)
findDuplicate = define "findDuplicate" $
  doc "Find the first duplicate name in a list" $
  "names" ~>
  -- Fold through names, tracking seen names in a set.
  -- Accumulator is (Set Name, Maybe Name): seen set and first duplicate found.
  "result" <~ Lists.foldl
    ("acc" ~> "name" ~>
      "seen" <~ Pairs.first (var "acc") $
      "dup" <~ Pairs.second (var "acc") $
      Maybes.cases (var "dup")
        (Logic.ifElse (Sets.member (var "name") (var "seen"))
          (pair (var "seen") (just $ var "name"))
          (pair (Sets.insert (var "name") (var "seen")) nothing))
        (constant $ var "acc"))
    (pair Sets.empty nothing)
    (var "names") $
  Pairs.second (var "result")
