{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ValidateTypeSpec.spec
-}

module Hydra.ValidateTypeSpec where

import Hydra.Core.Model
import Hydra.Core.Error.Model
import Hydra.Core.Validation
import qualified Hydra.Core.Validate.Model as ValidateCore

import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import qualified Test.Hspec as H


-- | Direct unit coverage for type validation (#848): where void may appear, and which map key and set element types
-- are comparable. Type validation runs only on type definitions during generation, so the shared test suite has no
-- case kind for it.
spec :: H.Spec
spec = do
  H.describe "void placement (#848)" $ do
    let allowed name typ = H.it ("allows void " ++ name) $ firstError typ `H.shouldBe` Nothing
        forbidden name typ = H.it ("rejects void " ++ name) $ firstError typ `H.shouldSatisfy` isVoidError
    allowed "at the top level" TypeVoid
    allowed "as an either side" $ TypeEither (EitherType TypeVoid string)
    allowed "as a union variant" $ TypeUnion [FieldType (Name "none") TypeVoid, FieldType (Name "some") string]
    allowed "as a function domain" $ TypeFunction (FunctionType TypeVoid string)
    allowed "as a function codomain" $ TypeFunction (FunctionType string TypeVoid)
    allowed "as an effect result" $ TypeEffect TypeVoid
    forbidden "as an optional element" $ TypeOptional TypeVoid
    forbidden "as a list element" $ TypeList TypeVoid
    forbidden "as a set element" $ TypeSet TypeVoid
    forbidden "as a map key" $ TypeMap (MapType TypeVoid string)
    forbidden "as a map value" $ TypeMap (MapType string TypeVoid)
    forbidden "as a pair component" $ TypePair (PairType string TypeVoid)
    forbidden "as a record field" $ TypeRecord [FieldType (Name "x") TypeVoid]

  H.describe "map key and set element comparability (#848)" $ do
    H.it "accepts a literal map key" $
      firstError (TypeMap (MapType string string)) `H.shouldBe` Nothing
    H.it "rejects a function map key" $
      firstError (TypeMap (MapType (TypeFunction (FunctionType string string)) string))
        `H.shouldSatisfy` isMapKeyError
    H.it "rejects an effect map key" $
      firstError (TypeMap (MapType (TypeEffect string) string)) `H.shouldSatisfy` isMapKeyError
    H.it "rejects a function nested in a map key" $
      firstError (TypeMap (MapType (TypeList (TypeFunction (FunctionType string string))) string))
        `H.shouldSatisfy` isMapKeyError
    H.it "accepts a function map value" $
      firstError (TypeMap (MapType string (TypeFunction (FunctionType string string)))) `H.shouldBe` Nothing
    H.it "rejects a function set element" $
      firstError (TypeSet (TypeFunction (FunctionType string string))) `H.shouldSatisfy` isSetElementError
    H.it "accepts a nominal key whose definition is comparable" $
      firstError (TypeMap (MapType (TypeVariable plainRecord) string)) `H.shouldBe` Nothing
    H.it "rejects a nominal key whose definition contains a function" $
      firstError (TypeMap (MapType (TypeVariable recordWithFunction) string)) `H.shouldSatisfy` isMapKeyError
  where
    string = TypeLiteral LiteralTypeString
    plainRecord = Name "test.PlainRecord"
    recordWithFunction = Name "test.RecordWithFunction"
    schema = M.fromList [
      (plainRecord, mono $ TypeRecord [FieldType (Name "s") string]),
      (recordWithFunction, mono $ TypeRecord [FieldType (Name "f") (TypeFunction (FunctionType string string))])]
    mono t = TypeScheme [] t M.empty
    inScope = S.fromList (M.keys schema)
    firstError typ = Y.listToMaybe $ validationResultErrors $
      ValidateCore.type_ ValidateCore.kernelDefaultCoreProfile schema (ValidationResult [] []) inScope typ
    isVoidError e = case e of
      Just (InvalidTypeErrorVoidInNonBottomPosition _) -> True
      _ -> False
    isMapKeyError e = case e of
      Just (InvalidTypeErrorNonComparableMapKeyType _) -> True
      _ -> False
    isSetElementError e = case e of
      Just (InvalidTypeErrorNonComparableSetElementType _) -> True
      _ -> False
