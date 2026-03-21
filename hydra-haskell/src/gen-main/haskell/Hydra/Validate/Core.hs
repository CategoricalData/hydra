-- Note: this is an automatically generated file. Do not edit.

-- | Validation functions for core terms

module Hydra.Validate.Core where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Check for duplicate binding names in a list of bindings
checkDuplicateBindings :: Accessors.AccessorPath -> [Core.Binding] -> Maybe Core_.InvalidTermError
checkDuplicateBindings path bindings =

      let names = Lists.map Core.bindingName bindings
          dup = findDuplicate names
      in (Maybes.map (\name -> Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
        Core_.duplicateBindingErrorLocation = path,
        Core_.duplicateBindingErrorName = name})) dup)

-- | Check for duplicate field names in a list of fields
checkDuplicateFields :: Accessors.AccessorPath -> [Core.Name] -> Maybe Core_.InvalidTermError
checkDuplicateFields path names =

      let dup = findDuplicate names
      in (Maybes.map (\name -> Core_.InvalidTermErrorDuplicateField (Core_.DuplicateFieldError {
        Core_.duplicateFieldErrorLocation = path,
        Core_.duplicateFieldErrorName = name})) dup)

-- | Check a single term node for duplicate bindings or fields
checkTerm :: Accessors.AccessorPath -> Core.Term -> Maybe Core_.InvalidTermError
checkTerm path term =
    case term of
      Core.TermLet v0 -> checkDuplicateBindings path (Core.letBindings v0)
      Core.TermRecord v0 -> checkDuplicateFields path (Lists.map Core.fieldName (Core.recordFields v0))
      _ -> Nothing

-- | Find the first duplicate name in a list
findDuplicate :: Ord t0 => ([t0] -> Maybe t0)
findDuplicate names =

      let result =
              Lists.foldl (\acc -> \name ->
                let seen = Pairs.first acc
                    dup = Pairs.second acc
                in (Maybes.cases dup (Logic.ifElse (Sets.member name seen) (seen, (Just name)) (Sets.insert name seen, Nothing)) (\_ -> acc))) (Sets.empty, Nothing) names
      in (Pairs.second result)

-- | Validate a term, returning the first error found or nothing if valid
term :: Graph.Graph -> Core.Term -> Maybe Core_.InvalidTermError
term g t =
    Rewriting.foldTermWithGraphAndPath (\recurse -> \path -> \cx -> \acc -> \trm -> Maybes.cases acc (
      let checkResult = checkTerm (Accessors.AccessorPath path) trm
      in (Maybes.cases checkResult (recurse Nothing trm) (\err -> Just err))) (\_ -> acc)) g Nothing t
