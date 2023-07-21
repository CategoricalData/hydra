{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.TermEncoding where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.TermEncoding
import Hydra.Dsl.Terms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Types as Types


termEncodingModule :: Module Kv
termEncodingModule = Module (Namespace "hydra/termEncoding") elements [hydraCoreModule] $
    Just "Implementation of LambdaGraph's sigma encoding, which represents terms as terms"
  where
   elements = [
     Base.el sigmaEncodeAnnotatedDef,
     Base.el sigmaEncodeApplicationDef,
     Base.el sigmaEncodeCaseStatementDef,
     Base.el sigmaEncodeEliminationDef,
     Base.el sigmaEncodeFieldDef,
     Base.el sigmaEncodeFieldNameDef,
     Base.el sigmaEncodeFloatValueDef,
     Base.el sigmaEncodeFunctionDef,
     Base.el sigmaEncodeInjectionDef,
     Base.el sigmaEncodeIntegerValueDef,
     Base.el sigmaEncodeLambdaDef,
     --Base.el sigmaEncodeLetDef,
     Base.el sigmaEncodeLiteralDef,
     Base.el sigmaEncodeNameDef,
     Base.el sigmaEncodeNominalTermDef,
     Base.el sigmaEncodeOptionalCasesDef,
     Base.el sigmaEncodeProjectionDef,
     Base.el sigmaEncodeRecordDef,
     Base.el sigmaEncodeSumDef,
     Base.el sigmaEncodeTermDef]

ref :: Definition a -> Term Kv
ref (Definition name _) = TermVariable name

termEncodingDefinition :: String -> Type Kv -> Term Kv -> Definition x
termEncodingDefinition label dom term = Base.definitionInModule termEncodingModule ("sigmaEncode" ++ label) $
  Base.function dom termA $ Datum term

annotatedTermAA = Types.apply (Types.apply (TypeVariable _Annotated) termA) (Types.var "a") :: Type a
applicationA = Types.apply (TypeVariable _Application) (Types.var "a") :: Type a
caseStatementA = Types.apply (TypeVariable _CaseStatement) (Types.var "a") :: Type a
eliminationA = Types.apply (TypeVariable _Elimination) (Types.var "a") :: Type a
fieldA = Types.apply (TypeVariable _Field) (Types.var "a") :: Type a
functionA = Types.apply (TypeVariable _Function) (Types.var "a") :: Type a
injectionA = Types.apply (TypeVariable _Injection) (Types.var "a") :: Type a
lambdaA = Types.apply (TypeVariable _Lambda) (Types.var "a") :: Type a
letA = Types.apply (TypeVariable _Let) (Types.var "a") :: Type a
nominalTermA = Types.apply (TypeVariable _Nominal) termA :: Type a
optionalCasesA = Types.apply (TypeVariable _OptionalCases) (Types.var "a") :: Type a
recordA = Types.apply (TypeVariable _Record) (Types.var "a") :: Type a
sumA = Types.apply (TypeVariable _Sum) (Types.var "a") :: Type a
termA = Types.apply (TypeVariable _Term) (Types.var "a") :: Type a
typeA = Types.apply (TypeVariable _Type) (Types.var "a") :: Type a

encodedBinary :: Term a -> Term a
encodedBinary = encodedLiteral . variant _Literal _Literal_binary

encodedBoolean :: Term a -> Term a
encodedBoolean = encodedLiteral . variant _Literal _Literal_boolean

encodedCase :: Name -> FieldName -> Term a -> Field a
encodedCase tname fname fun = Field fname $ lambda "v" $ encodedVariant tname fname (fun @@ var "v")

encodedField :: FieldName -> Term a -> Term a
encodedField fname = encodedFieldRaw (encodedFieldName fname)

encodedFieldRaw :: Term a -> Term a -> Term a
encodedFieldRaw fname term = record _Field [
  Field _Field_name fname,
  Field _Field_term term]

encodedFieldName :: FieldName -> Term a
encodedFieldName = wrap _FieldName . string . unFieldName

encodedFloatValue :: Term a -> Term a
encodedFloatValue = encodedLiteral . variant _Literal _Literal_float

encodedInjection :: Name -> FieldName -> Term a -> Term a
encodedInjection tname fname term = record _Injection [
  Field _Injection_typeName $ encodedName tname,
  Field _Injection_field $ encodedField fname term]

encodedInt32 :: Term a -> Term a
encodedInt32 = encodedIntegerValue . variant _IntegerValue _IntegerValue_int32

encodedIntegerValue :: Term a -> Term a
encodedIntegerValue = encodedLiteral . variant _Literal _Literal_integer

encodedList :: Term a -> Term a
encodedList = variant _Term _Term_list

encodedLiteral :: Term a -> Term a
encodedLiteral = variant _Term _Term_literal

encodedMap :: Term a -> Term a
encodedMap = variant _Term _Term_map

encodedName :: Name -> Term a
encodedName = wrap _Name . string . unName

encodedNominal :: Name -> Term a -> Term a
encodedNominal name = encodedNominalRaw (encodedName name)

encodedNominalRaw :: Term a -> Term a -> Term a
encodedNominalRaw name term = variant _Term _Term_wrap $ record _Nominal [
  Field _Nominal_typeName name,
  Field _Nominal_object term]

encodedOptional :: Term a -> Term a
encodedOptional = variant _Term _Term_optional

encodedRecord :: Name -> [(FieldName, Term a)] -> Term a
encodedRecord tname pairs = variant _Term _Term_record $ record _Record [
    Field _Record_typeName $ encodedName tname,
    Field _Record_fields $ list (encField <$> pairs)]
  where
    encField (fname, term) = encodedField fname term

encodedSet :: Term a -> Term a
encodedSet = variant _Term _Term_set

encodedString :: Term a -> Term a
encodedString = encodedLiteral . variant _Literal _Literal_string

encodedUnion :: Term a -> Term a
encodedUnion = variant _Term _Term_union

encodedVariant :: Name -> FieldName -> Term a -> Term a
encodedVariant tname fname term = encodedUnion $ encodedInjection tname fname term

sigmaEncodeAnnotatedDef :: Definition (Annotated (Term a) a -> Term a)
sigmaEncodeAnnotatedDef = termEncodingDefinition "Annotated" annotatedTermAA $
  lambda "a" $ variant _Term _Term_annotated $ record _Annotated [
    Field _Annotated_subject $ ref sigmaEncodeTermDef @@ (project _Annotated _Annotated_subject @@ var "a"),
    Field _Annotated_annotation $ project _Annotated _Annotated_annotation @@ var "a"]

sigmaEncodeApplicationDef :: Definition (Application a -> Term a)
sigmaEncodeApplicationDef = termEncodingDefinition "Application" applicationA $
  lambda "app" $ encodedRecord _Application [
    (_Application_function, ref sigmaEncodeTermDef @@ (project _Application _Application_function @@ var "app")),
    (_Application_argument, ref sigmaEncodeTermDef @@ (project _Application _Application_argument @@ var "app"))]

sigmaEncodeCaseStatementDef :: Definition (CaseStatement a -> Term a)
sigmaEncodeCaseStatementDef = termEncodingDefinition "CaseStatement" caseStatementA $
  lambda "cs" $ encodedRecord _CaseStatement [
    (_CaseStatement_typeName, ref sigmaEncodeNameDef @@ (project _CaseStatement _CaseStatement_typeName @@ var "cs")),
    (_CaseStatement_default, encodedOptional
      (primitive _optionals_map @@ ref sigmaEncodeTermDef @@ (project _CaseStatement _CaseStatement_default @@ var "cs"))),
    (_CaseStatement_cases, encodedList
      (primitive _lists_map @@ ref sigmaEncodeFieldDef @@ (project _CaseStatement _CaseStatement_cases @@ var "cs")))]

sigmaEncodeEliminationDef :: Definition (Elimination a -> Term a)
sigmaEncodeEliminationDef = termEncodingDefinition "Elimination" eliminationA $
    match _Elimination Nothing [
      ecase _Elimination_list sigmaEncodeTermDef,
      ecase _Elimination_optional sigmaEncodeOptionalCasesDef,
      ecase _Elimination_record sigmaEncodeProjectionDef,
      ecase _Elimination_union sigmaEncodeCaseStatementDef,
      ecase _Elimination_wrap sigmaEncodeNameDef]
  where
    ecase fname funname = encodedCase _Elimination fname (ref funname)

sigmaEncodeFieldDef :: Definition (Field a -> Term a)
sigmaEncodeFieldDef = termEncodingDefinition "Field" fieldA $
  lambda "f" $ encodedRecord _Field [
    (_Field_name, encodedString $ (unwrap _FieldName @@ (project _Field _Field_name @@ var "f"))),
    (_Field_term, ref sigmaEncodeTermDef @@ (project _Field _Field_term @@ var "f"))]

sigmaEncodeFieldNameDef :: Definition (FieldName -> Term a)
sigmaEncodeFieldNameDef = termEncodingDefinition "FieldName" (TypeVariable _FieldName) $
  lambda "fn" $ encodedNominal _FieldName $ encodedString (unwrap _FieldName @@ var "fn")

sigmaEncodeFloatValueDef :: Definition (FloatValue -> Term a)
sigmaEncodeFloatValueDef = termEncodingDefinition "FloatValue" (TypeVariable _FloatValue) $
  match _FloatValue Nothing (varField <$> [
    _FloatValue_bigfloat,
    _FloatValue_float32,
    _FloatValue_float64])
  where
    varField fname = Field fname $ lambda "v" $ encodedVariant _FloatValue fname $ encodedFloatValue $
      variant _FloatValue fname $ var "v"

sigmaEncodeFunctionDef :: Definition (Function a -> Term a)
sigmaEncodeFunctionDef = termEncodingDefinition "Function" functionA $
    match _Function Nothing [
      ecase _Function_elimination sigmaEncodeEliminationDef,
      ecase _Function_lambda sigmaEncodeLambdaDef,
      ecase _Function_primitive sigmaEncodeNameDef]
  where
    ecase fname funname = encodedCase _Function fname (ref funname)

sigmaEncodeInjectionDef :: Definition (Injection a -> Term a)
sigmaEncodeInjectionDef = termEncodingDefinition "Injection" injectionA $
  lambda "i" $ encodedRecord _Injection [
    (_Injection_typeName, ref sigmaEncodeNameDef @@ (project _Injection _Injection_typeName @@ var "i")),
    (_Injection_field, ref sigmaEncodeFieldDef @@ (project _Injection _Injection_field @@ var "i"))]

sigmaEncodeIntegerValueDef :: Definition (IntegerValue -> Term a)
sigmaEncodeIntegerValueDef = termEncodingDefinition "IntegerValue" (TypeVariable _IntegerValue) $
  match _IntegerValue Nothing (varField <$> [
    _IntegerValue_bigint,
    _IntegerValue_int8,
    _IntegerValue_int16,
    _IntegerValue_int32,
    _IntegerValue_int64,
    _IntegerValue_uint8,
    _IntegerValue_uint16,
    _IntegerValue_uint32,
    _IntegerValue_uint64])
  where
    varField fname = Field fname $ lambda "v" $ encodedVariant _IntegerValue fname $ encodedIntegerValue $
      variant _IntegerValue fname $ var "v"

sigmaEncodeLambdaDef :: Definition (Lambda a -> Term a)
sigmaEncodeLambdaDef = termEncodingDefinition "Lambda" lambdaA $
  lambda "l" $ encodedRecord _Lambda [
    (_Lambda_parameter, ref sigmaEncodeNameDef @@ (project _Lambda _Lambda_parameter @@ var "l")),
    (_Lambda_body, ref sigmaEncodeTermDef @@ (project _Lambda _Lambda_body @@ var "l"))]

-- sigmaEncodeLetDef :: Definition (Let a -> Term a)
-- sigmaEncodeLetDef = termEncodingDefinition "Let" letA $
--   lambda "l" $ encodedRecord _Let [
--     (_Let_bindings, encodedMap
--       (primitive _maps_mapKeys @@ ref sigmaEncodeNameDef @@
--         (primitive _maps_map @@ ref sigmaEncodeTermDef @@ (project _Let _Let_bindings @@ var "l")))),
--     (_Let_environment, ref sigmaEncodeTermDef @@ (project _Let _Let_environment @@ var "l"))]

sigmaEncodeLiteralDef :: Definition (Literal -> Term a)
sigmaEncodeLiteralDef = termEncodingDefinition "Literal" (TypeVariable _Literal) $
  match _Literal Nothing [
    varField _Literal_binary $ encodedBinary $ var "v",
    varField _Literal_boolean $ encodedBoolean $ var "v",
    varField _Literal_float (ref sigmaEncodeFloatValueDef @@ var "v"),
    varField _Literal_integer (ref sigmaEncodeIntegerValueDef @@ var "v"),
    varField _Literal_string $ encodedString $ var "v"]
  where
    varField fname = Field fname . lambda "v" . encodedVariant _Literal fname

sigmaEncodeNameDef :: Definition (Name -> Term a)
sigmaEncodeNameDef = termEncodingDefinition "Name" (TypeVariable _Name) $
  lambda "fn" $ encodedNominal _Name $ encodedString (unwrap _Name @@ var "fn")

sigmaEncodeNominalTermDef :: Definition (Nominal (Term a) -> Term a)
sigmaEncodeNominalTermDef = termEncodingDefinition "NominalTerm" nominalTermA $
  lambda "n" $ encodedRecord _Nominal [
    (_Nominal_typeName, ref sigmaEncodeNameDef @@ (project _Nominal _Nominal_typeName @@ var "n")),
    (_Nominal_object, ref sigmaEncodeTermDef @@ (project _Nominal _Nominal_object @@ var "n"))]

sigmaEncodeOptionalCasesDef :: Definition (OptionalCases a -> Term a)
sigmaEncodeOptionalCasesDef = termEncodingDefinition "OptionalCases" optionalCasesA $
  lambda "oc" $ encodedRecord _OptionalCases [
    (_OptionalCases_nothing, ref sigmaEncodeTermDef @@ (project _OptionalCases _OptionalCases_nothing @@ var "oc")),
    (_OptionalCases_just, ref sigmaEncodeTermDef @@ (project _OptionalCases _OptionalCases_just @@ var "oc"))]

sigmaEncodeProjectionDef :: Definition (Projection -> Term a)
sigmaEncodeProjectionDef = termEncodingDefinition "Projection" (TypeVariable _Projection) $
  lambda "p" $ encodedRecord _Projection [
    (_Projection_typeName, ref sigmaEncodeNameDef @@ (project _Projection _Projection_typeName @@ var "p")),
    (_Projection_field, ref sigmaEncodeFieldNameDef @@ (project _Projection _Projection_field @@ var "p"))]

sigmaEncodeRecordDef :: Definition (Record a -> Term a)
sigmaEncodeRecordDef = termEncodingDefinition "Record" recordA $
  lambda "r" $ encodedRecord _Record [
    (_Record_typeName, ref sigmaEncodeNameDef @@ (project _Record _Record_typeName @@ var "r")),
    (_Record_fields, encodedList (primitive _lists_map @@ (ref sigmaEncodeFieldDef) @@ (project _Record _Record_fields @@ var "r")))]

sigmaEncodeSumDef :: Definition (Sum a -> Term a)
sigmaEncodeSumDef = termEncodingDefinition "Sum" sumA $
  lambda "s" $ encodedRecord _Sum [
    (_Sum_index, encodedInt32 $ project _Sum _Sum_index @@ var "s"),
    (_Sum_size, encodedInt32 $ project _Sum _Sum_size @@ var "s"),
    (_Sum_term, ref sigmaEncodeTermDef @@ (project _Sum _Sum_term @@ var "s"))]

sigmaEncodeTermDef :: Definition (Term a -> Term a)
sigmaEncodeTermDef = termEncodingDefinition "Term" termA $
  match _Term (Just $ encodedString $ string "not implemented") [
    ecase _Term_annotated (ref sigmaEncodeAnnotatedDef),
    ecase _Term_application (ref sigmaEncodeApplicationDef),
    ecase _Term_function (ref sigmaEncodeFunctionDef),
    -- TODO: restore let constructor after finding a way to infer "Ord a =>" for Haskell
    -- ecase _Term_let (ref sigmaEncodeLetDef),
    ecase _Term_literal (ref sigmaEncodeLiteralDef),
    ecase' _Term_list $ encodedList (primitive _lists_map @@ (ref sigmaEncodeTermDef) @@ var "v"),
    -- TODO: restore map and set constructors after finding a way to infer "Ord a =>" for Haskell
    -- _Term_map,
    ecase' _Term_optional $ encodedOptional (primitive _optionals_map @@ ref sigmaEncodeTermDef @@ var "v"),
    ecase' _Term_product $ encodedList (primitive _lists_map @@ ref sigmaEncodeTermDef @@ var "v"),
    ecase _Term_record (ref sigmaEncodeRecordDef),
    -- TODO: restore map and set constructors after finding a way to infer "Ord a =>" for Haskell
    -- ecase' _Term_set $ encodedSet (primitive _sets_map @@ (ref sigmaEncodeTermDef) @@ var "v")
    ecase _Term_sum (ref sigmaEncodeSumDef),
    -- TODO: determine whether streams have a sigma encoding
    -- _ Term_stream
    ecase _Term_union (ref sigmaEncodeInjectionDef),
    ecase _Term_variable (ref sigmaEncodeNameDef),
    ecase _Term_wrap (ref sigmaEncodeNominalTermDef)]
  where
    ecase = encodedCase _Term
    ecase' fname = Field fname . lambda "v" . encodedVariant _Term fname
