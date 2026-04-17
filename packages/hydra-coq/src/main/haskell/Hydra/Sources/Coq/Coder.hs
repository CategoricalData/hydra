{-# LANGUAGE FlexibleContexts #-}

-- | Coq code generator: converts Hydra type and term modules to Coq source code (.v files).
-- This is a DSL port of the previously hand-written Hydra.Coq.Coder.

module Hydra.Sources.Coq.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Other.Coq                   as CoqSyntax
import qualified Hydra.Sources.Coq.Environment             as CoqEnvironmentSource
import qualified Hydra.Sources.Coq.Language                as CoqLanguage
import qualified Hydra.Sources.Coq.Utils                   as CoqUtils
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as DL
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for Coq AST
import Hydra.Ast
import qualified Hydra.Coq.Syntax as C
import qualified Hydra.Coq.Environment as CE


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | Local helper: listAny via Lists.find + Maybes.isJust, since the DSL list
-- library does not expose an `any` primitive directly.
listAny :: TTerm (a -> Bool) -> TTerm [a] -> TTerm Bool
listAny pred xs = Maybes.isJust (Lists.find pred xs)

ns :: Namespace
ns = Namespace "hydra.coq.coder"

module_ :: Module
module_ = Module ns definitions
    [Formatting.ns, CoqLanguage.ns]
    (CoqEnvironmentSource.ns:CoqSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Coq code generator: converts Hydra modules to Coq source"
  where
    definitions = [
      toDefinition coqArrow,
      toDefinition coqIdent,
      toDefinition coqName,
      toDefinition coqQualid,
      toDefinition coqTermApp,
      toDefinition coqTermCast,
      toDefinition coqTermQualid,
      toDefinition coqTypeTerm,
      toDefinition encodeAxiomDefinitionPair,
      toDefinition encodeFloatLiteral,
      toDefinition encodeLambdaTerm,
      toDefinition encodeLiteral,
      toDefinition encodeLiteralType,
      toDefinition encodeProjectionElim,
      toDefinition encodeTerm,
      toDefinition encodeTermDefinition,
      toDefinition encodeTermDefinitionPair,
      toDefinition encodeType,
      toDefinition encodeTypeDefinition,
      toDefinition encodeTypeDefinitionPair,
      toDefinition encodeUnionConstructor,
      toDefinition encodeUnionElim,
      toDefinition encodeWrapElim,
      toDefinition escapeCoqString,
      toDefinition extractLambdaBinders,
      toDefinition isUnitDomain,
      toDefinition isUnitLambda,
      toDefinition localTypeName,
      toDefinition moduleToCoq,
      toDefinition renameLibKeyword,
      toDefinition requireImportSentence,
      toDefinition resolveQualifiedName,
      toDefinition sanitizeStripped,
      toDefinition sanitizeVar,
      toDefinition standardImports,
      toDefinition stripLambdas,
      toDefinition termReferencesVar,
      toDefinition unionConstructorName,
      toDefinition unitLambdaBody]

-- | Build a Coq arrow type "dom -> cod", represented as forall (_ : dom), cod.
coqArrow :: TTermDefinition (C.Term -> C.Term -> C.Term)
coqArrow = define "coqArrow" $
  doc "Build the Coq dependent-function term `forall (_ : dom), cod` used as the arrow type" $
  lambdas ["dom", "cod"] $
    inject C._Term C._Term_forallOrFun $
      inject C._ForallOrFun C._ForallOrFun_forall $
        record C._Forall [
          C._Forall_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
            inject C._Binder C._Binder_type $
              record C._TypeBinders [
                C._TypeBinders_names>>: list [wrap C._Name (nothing :: TTerm (Maybe C.Ident))],
                C._TypeBinders_type>>: coqTypeTerm @@ var "dom"]]),
          C._Forall_type>>: coqTypeTerm @@ var "cod"]

-- | Build a Coq Ident from a string.
coqIdent :: TTermDefinition (String -> C.Ident)
coqIdent = define "coqIdent" $
  lambda "s" $ wrap C._Ident $ wrap C._String $ var "s"

-- | Build a Coq Name (optional Ident) from a string.
coqName :: TTermDefinition (String -> C.Name)
coqName = define "coqName" $
  lambda "s" $ wrap C._Name $ just (coqIdent @@ var "s")

-- | Build a simple Coq Qualid from a string (no field-path components).
coqQualid :: TTermDefinition (String -> C.Qualid)
coqQualid = define "coqQualid" $
  lambda "s" $ record C._Qualid [
    C._Qualid_id>>: coqIdent @@ var "s",
    C._Qualid_fieldIds>>: list ([] :: [TTerm C.FieldIdent])]

-- | Build a Coq function application term "f args..." (or just "f" when there are no args).
coqTermApp :: TTermDefinition (C.Term -> [C.Term] -> C.Term)
coqTermApp = define "coqTermApp" $
  doc "Apply a Coq term to a list of argument terms, parenthesising each" $
  lambdas ["f", "args"] $
    Logic.ifElse (Lists.null (var "args"))
      (var "f")
      (inject C._Term C._Term_term100 $
        inject C._Term100 C._Term100_term10 $
          inject C._Term10 C._Term10_application $
            inject C._Application C._Application_normal $
              record C._NormalApplication [
                C._NormalApplication_lhs>>:
                  inject C._Term1 C._Term1_term0 $
                    inject C._Term0 C._Term0_parens (var "f"),
                C._NormalApplication_rhs>>:
                  Lists.map
                    (lambda "a" $ inject C._Arg C._Arg_term $
                      inject C._Term1 C._Term1_term0 $
                        inject C._Term0 C._Term0_parens (var "a"))
                    (var "args")])

-- | Wrap a term in a Coq type annotation: `(t : T)`. Used to force type
-- inference for empty containers like `None`, `nil`, and empty maps/sets,
-- whose element type cannot otherwise be inferred from context.
coqTermCast :: TTermDefinition (C.Term -> C.Type -> C.Term)
coqTermCast = define "coqTermCast" $
  doc "Build a Coq Term expressing `(t : T)` with the normal cast operator" $
  lambdas ["t", "ty"] $
    inject C._Term C._Term_term100 $
      inject C._Term100 C._Term100_cast $
        record C._TypeCast [
          C._TypeCast_term>>:
            inject C._Term10 C._Term10_oneTerm $
              inject C._OneTerm C._OneTerm_term1 $
                inject C._Term1 C._Term1_term0 $
                  inject C._Term0 C._Term0_parens (var "t"),
          C._TypeCast_type>>: var "ty",
          C._TypeCast_operator>>:
            inject C._TypeCastOperator C._TypeCastOperator_normal unit]

-- | Build a Coq atomic term from a qualified name (unapplied).
coqTermQualid :: TTermDefinition (String -> C.Term)
coqTermQualid = define "coqTermQualid" $
  doc "Build a Coq Term that references a (possibly qualified) identifier" $
  lambda "s" $
    inject C._Term C._Term_term100 $
      inject C._Term100 C._Term100_term10 $
        inject C._Term10 C._Term10_oneTerm $
          inject C._OneTerm C._OneTerm_explicit $
            record C._QualidAnnotated [
              C._QualidAnnotated_qualid>>: coqQualid @@ var "s",
              C._QualidAnnotated_univAnnot>>: (nothing :: TTerm (Maybe C.UnivAnnot))]

-- | Wrap a Term as a Type.
coqTypeTerm :: TTermDefinition (C.Term -> C.Type)
coqTypeTerm = define "coqTypeTerm" $
  lambda "t" $ wrap C._Type $ var "t"

-- | Escape double quotes in a Coq string literal by doubling them.
escapeCoqString :: TTermDefinition (String -> String)
escapeCoqString = define "escapeCoqString" $
  doc "Escape a string for Coq string literals: double any embedded quotes" $
  lambda "s" $ Strings.intercalate (string "\"\"") (Strings.splitOn (string "\"") (var "s"))

-- | Sanitize a variable name to avoid Coq reserved words by appending an underscore.
sanitizeVar :: TTermDefinition (String -> String)
sanitizeVar = define "sanitizeVar" $
  doc "Append an underscore if the name collides with a Coq reserved word" $
  lambda "s" $ Formatting.escapeWithUnderscore @@ CoqLanguage.coqReservedWords @@ var "s"

-- | Sanitize a stripped local reference. Uses a narrower reserved-word set
-- than `sanitizeVar`, matching the old text-stripper's behaviour.
sanitizeStripped :: TTermDefinition (String -> String)
sanitizeStripped = define "sanitizeStripped" $
  doc "Append an underscore if a stripped-local-name reference collides with a Coq reserved word" $
  lambda "s" $ Formatting.escapeWithUnderscore @@ CoqLanguage.coqStrippedReservedWords @@ var "s"

-- | Encode a Hydra LiteralType to a Coq Term (qualid reference).
encodeLiteralType :: TTermDefinition (LiteralType -> C.Term)
encodeLiteralType = define "encodeLiteralType" $
  doc "Map a Hydra LiteralType to its Coq stdlib counterpart" $
  lambda "lt" $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_boolean>>: constant (coqTermQualid @@ string "bool"),
    _LiteralType_decimal>>: constant (coqTermQualid @@ string "Q"),
    _LiteralType_float>>: constant (coqTermQualid @@ string "Q"),
    _LiteralType_integer>>: "it" ~> cases _IntegerType (var "it") Nothing [
      _IntegerType_bigint>>: constant (coqTermQualid @@ string "Z"),
      _IntegerType_int8>>: constant (coqTermQualid @@ string "Z"),
      _IntegerType_int16>>: constant (coqTermQualid @@ string "Z"),
      _IntegerType_int32>>: constant (coqTermQualid @@ string "Z"),
      _IntegerType_int64>>: constant (coqTermQualid @@ string "Z"),
      _IntegerType_uint8>>: constant (coqTermQualid @@ string "nat"),
      _IntegerType_uint16>>: constant (coqTermQualid @@ string "nat"),
      _IntegerType_uint32>>: constant (coqTermQualid @@ string "nat"),
      _IntegerType_uint64>>: constant (coqTermQualid @@ string "nat")],
    _LiteralType_string>>: constant (coqTermQualid @@ string "string"),
    _LiteralType_binary>>: constant (coqTermQualid @@ string "string")]

-- | Encode a Hydra Type to a Coq Term.
encodeType :: TTermDefinition (CE.CoqEnvironment -> Type -> C.Term)
encodeType = define "encodeType" $
  doc "Translate a Hydra Type into a Coq Term representing that type. The environment is consulted to resolve qualified type variable references" $
  lambdas ["env", "ty"] $ cases _Type (var "ty") Nothing [
    _Type_annotated>>: "at" ~> encodeType @@ var "env" @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~>
      coqTermApp
        @@ (encodeType @@ var "env" @@ (Core.applicationTypeFunction $ var "app"))
        @@ list [encodeType @@ var "env" @@ (Core.applicationTypeArgument $ var "app")],
    _Type_either>>: "et" ~>
      coqTermApp @@ (coqTermQualid @@ string "sum") @@ list [
        encodeType @@ var "env" @@ (Core.eitherTypeLeft $ var "et"),
        encodeType @@ var "env" @@ (Core.eitherTypeRight $ var "et")],
    _Type_forall>>: "ft" ~>
      inject C._Term C._Term_forallOrFun $
        inject C._ForallOrFun C._ForallOrFun_forall $
          record C._Forall [
            C._Forall_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
              inject C._Binder C._Binder_type $
                record C._TypeBinders [
                  C._TypeBinders_names>>:
                    list [coqName @@ (unwrap _Name @@ (Core.forallTypeParameter $ var "ft"))],
                  C._TypeBinders_type>>: coqTypeTerm @@ (coqTermQualid @@ string "Type")]]),
            C._Forall_type>>: coqTypeTerm @@ (encodeType @@ var "env" @@ (Core.forallTypeBody $ var "ft"))],
    _Type_function>>: "ft" ~>
      coqArrow
        @@ (encodeType @@ var "env" @@ (Core.functionTypeDomain $ var "ft"))
        @@ (encodeType @@ var "env" @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_list>>: "t" ~>
      coqTermApp @@ (coqTermQualid @@ string "list") @@ list [encodeType @@ var "env" @@ var "t"],
    _Type_literal>>: "lt" ~> encodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~>
      coqTermApp @@ (coqTermQualid @@ string "list") @@ list [
        coqTermApp @@ (coqTermQualid @@ string "prod") @@ list [
          encodeType @@ var "env" @@ (Core.mapTypeKeys $ var "mt"),
          encodeType @@ var "env" @@ (Core.mapTypeValues $ var "mt")]],
    _Type_maybe>>: "t" ~>
      coqTermApp @@ (coqTermQualid @@ string "option") @@ list [encodeType @@ var "env" @@ var "t"],
    _Type_pair>>: "pt" ~>
      coqTermApp @@ (coqTermQualid @@ string "prod") @@ list [
        encodeType @@ var "env" @@ (Core.pairTypeFirst $ var "pt"),
        encodeType @@ var "env" @@ (Core.pairTypeSecond $ var "pt")],
    _Type_record>>: constant (coqTermQualid @@ string "unit"),
    _Type_set>>: "t" ~>
      coqTermApp @@ (coqTermQualid @@ string "list") @@ list [encodeType @@ var "env" @@ var "t"],
    _Type_union>>: constant (coqTermQualid @@ string "unit"),
    _Type_unit>>: constant (coqTermQualid @@ string "unit"),
    _Type_variable>>: "n" ~> lets [
      "raw">: unwrap _Name @@ var "n",
      -- Type-variable references are emitted raw when unqualified (Coq
      -- primitives like `Type`, locally-bound type-param names like `t0`);
      -- only `hydra.<ns>.<x>` / `Build_hydra.<ns>.<x>` references need to
      -- flow through the full qualified-name resolver.
      "headSeg">: Maybes.fromMaybe (var "raw") (Lists.maybeHead (Strings.splitOn (string ".") (var "raw")))] $
      Logic.ifElse (Logic.or
          (Equality.equal (var "headSeg") (string "hydra"))
          (Equality.equal (var "headSeg") (string "Build_hydra")))
        (coqTermQualid @@ (resolveQualifiedName @@ var "env" @@ var "raw"))
        (coqTermQualid @@ var "raw"),
    _Type_void>>: constant (coqTermQualid @@ string "Empty_set"),
    _Type_wrap>>: "wt" ~> encodeType @@ var "env" @@ var "wt"]

-- | Encode a (name, Type) pair as a Coq Sentence containing an Axiom declaration.
-- Used for modules whose definitions cannot practically compile under coqc (e.g.
-- hydra.hoisting, hydra.inference) — their term definitions are replaced by
-- axioms of the same type, which the rest of the Coq build can still consume.
encodeAxiomDefinitionPair :: TTermDefinition (CE.CoqEnvironment -> (String, Type) -> C.Sentence)
encodeAxiomDefinitionPair = define "encodeAxiomDefinitionPair" $
  doc "Produce `Axiom name : type.` from a (name, Hydra type) pair" $
  lambdas ["env", "nt"] $
    record C._Sentence [
      C._Sentence_comment>>: (nothing :: TTerm (Maybe C.Comment)),
      C._Sentence_content>>:
        inject C._SentenceContent C._SentenceContent_axiom $
          record C._AxiomDeclaration [
            C._AxiomDeclaration_name>>: coqIdent @@ Pairs.first (var "nt"),
            C._AxiomDeclaration_type>>: coqTypeTerm @@ (encodeType @@ var "env" @@ Pairs.second (var "nt"))]]

-- | Translate a float string (produced by `showBigfloat`) to a Coq Term.
-- Ordinary values become parenthesised `Q`-scope literals. IEEE 754 special
-- values (`Infinity`, `-Infinity`, `NaN`) have no representation in Coq's
-- exact rationals, so they are emitted as references to the corresponding
-- `hydra_posInf`/`hydra_negInf`/`hydra_nan` axioms from `hydra.lib.base`.
encodeFloatLiteral :: TTermDefinition (String -> C.Term)
encodeFloatLiteral = define "encodeFloatLiteral" $
  doc "Map a Haskell-`show`n Double/Scientific to a Coq term, routing NaN/Inf to base-lib axioms" $
  lambda "s" $
    Logic.ifElse (Equality.equal (var "s") (string "Infinity"))
      (coqTermQualid @@ string "hydra_posInf") $
    Logic.ifElse (Equality.equal (var "s") (string "-Infinity"))
      (coqTermQualid @@ string "hydra_negInf") $
    Logic.ifElse (Equality.equal (var "s") (string "NaN"))
      (coqTermQualid @@ string "hydra_nan")
      (coqTermQualid @@ Strings.cat (list [string "(", var "s", string ")"]))

-- | Encode a single Hydra Lambda term into a Coq `fun` term.
encodeLambdaTerm :: TTermDefinition (CE.CoqEnvironment -> Lambda -> C.Term)
encodeLambdaTerm = define "encodeLambdaTerm" $
  doc "Encode a Lambda into a Coq `fun` expression, sanitising the parameter name" $
  lambdas ["env", "lam"] $ lets [
    "paramName">: sanitizeVar @@ (unwrap _Name @@ (Core.lambdaParameter $ var "lam")),
    "binder">: Maybes.maybe
      (inject C._Binder C._Binder_name (coqName @@ var "paramName"))
      (lambda "domTy" $ inject C._Binder C._Binder_type $
        record C._TypeBinders [
          C._TypeBinders_names>>: list [coqName @@ var "paramName"],
          C._TypeBinders_type>>: coqTypeTerm @@ (encodeType @@ var "env" @@ var "domTy")])
      (Core.lambdaDomain $ var "lam")] $
    inject C._Term C._Term_forallOrFun $
      inject C._ForallOrFun C._ForallOrFun_fun $
        record C._Fun [
          C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [var "binder"]),
          C._Fun_body>>: encodeTerm @@ var "env" @@ (Core.lambdaBody $ var "lam")]

-- | Encode a Hydra literal into a Coq Term.
encodeLiteral :: TTermDefinition (Literal -> C.Term)
encodeLiteral = define "encodeLiteral" $
  doc "Translate a Hydra literal into its Coq stdlib form, with disambiguating parentheses" $
  lambda "lit" $ cases _Literal (var "lit") Nothing [
    _Literal_boolean>>: "b" ~>
      Logic.ifElse (var "b") (coqTermQualid @@ string "true") (coqTermQualid @@ string "false"),
    _Literal_decimal>>: "d" ~> coqTermQualid @@ Strings.cat (list [
      string "(", Literals.showDecimal (var "d"), string ")"]),
    _Literal_float>>: "fv" ~> cases _FloatValue (var "fv") Nothing [
      _FloatValue_bigfloat>>: "v" ~> encodeFloatLiteral @@ Literals.showBigfloat (var "v"),
      _FloatValue_float32>>: "v" ~> encodeFloatLiteral @@ Literals.showBigfloat (Literals.float32ToBigfloat $ var "v"),
      _FloatValue_float64>>: "v" ~> encodeFloatLiteral @@ Literals.showBigfloat (Literals.float64ToBigfloat $ var "v")],
    _Literal_integer>>: "iv" ~> cases _IntegerValue (var "iv") Nothing [
      _IntegerValue_bigint>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showBigint (var "v"), string ")%Z"]),
      _IntegerValue_int8>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showInt8 (var "v"), string ")%Z"]),
      _IntegerValue_int16>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showInt16 (var "v"), string ")%Z"]),
      _IntegerValue_int32>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showInt32 (var "v"), string ")%Z"]),
      _IntegerValue_int64>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showInt64 (var "v"), string ")%Z"]),
      _IntegerValue_uint8>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showUint8 (var "v"), string ")"]),
      _IntegerValue_uint16>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showUint16 (var "v"), string ")"]),
      _IntegerValue_uint32>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showUint32 (var "v"), string ")"]),
      _IntegerValue_uint64>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showUint64 (var "v"), string ")"])],
    _Literal_string>>: "s" ~> coqTermQualid @@ Strings.cat (list [
      string "\"", escapeCoqString @@ var "s", string "\"%string"]),
    _Literal_binary>>: constant (coqTermQualid @@ string "\"\"")]

-- | Encode a Hydra record projection as a Coq `fun r_ => r_.(field)` term.
-- If the projection refers to a field that was sanitised to `unit` during
-- type generation (because of a positivity failure), emit an unreachable
-- stub instead. That replaces the former `replaceSanitizedAccessors` text
-- post-processor in `packages/hydra-coq/.../Hydra/Coq/Generate.hs`.
encodeProjectionElim :: TTermDefinition (CE.CoqEnvironment -> Projection -> C.Term)
encodeProjectionElim = define "encodeProjectionElim" $
  doc "Translate a Hydra record projection into a Coq lambda that pulls out the field" $
  lambdas ["env", "p"] $ lets [
    "fname">: Core.projectionField $ var "p",
    "rawFname">: unwrap _Name @@ var "fname",
    "sanitizedSet">: project CE._CoqEnvironment CE._CoqEnvironment_sanitizedAccessors @@ var "env"] $
    Logic.ifElse (Sets.member (var "rawFname") (var "sanitizedSet"))
      -- Sanitised accessor: the field is typed as `unit` in the generated
      -- record (positivity workaround). Any call site that projects it would
      -- get `tt`, which is never the real value. Emit `hydra_unreachable`
      -- so the caller traps rather than silently proceeding.
      (inject C._Term C._Term_forallOrFun $
        inject C._ForallOrFun C._ForallOrFun_fun $
          record C._Fun [
            C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
              inject C._Binder C._Binder_name (coqName @@ string "_")]),
            C._Fun_body>>: coqTermQualid @@ string "hydra_unreachable"])
      (inject C._Term C._Term_forallOrFun $
        inject C._ForallOrFun C._ForallOrFun_fun $
          record C._Fun [
            C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
              inject C._Binder C._Binder_name (coqName @@ string "r_")]),
            C._Fun_body>>: coqTermApp
              @@ (coqTermQualid @@ var "rawFname")
              @@ list [coqTermQualid @@ string "r_"]])

-- | Encode a Hydra Term into a Coq Term.
encodeTerm :: TTermDefinition (CE.CoqEnvironment -> Term -> C.Term)
encodeTerm = define "encodeTerm" $
  doc "Translate a Hydra Term into its Coq Term counterpart. The environment provides the constructor-count map used by encodeUnionElim (to decide whether a match is exhaustive) and the ambiguous-name set used by resolveQualifiedName (to decide whether cross-module references need to stay fully qualified)." $
  lambdas ["env", "tm"] $ cases _Term (var "tm") Nothing [
    _Term_annotated>>: "at" ~> encodeTerm @@ var "env" @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~>
      coqTermApp
        @@ (encodeTerm @@ var "env" @@ (Core.applicationFunction $ var "app"))
        @@ list [encodeTerm @@ var "env" @@ (Core.applicationArgument $ var "app")],
    _Term_cases>>: "cs" ~> encodeUnionElim @@ var "env" @@ var "cs",
    _Term_either>>: "e" ~> Eithers.either_
      (lambda "l" $ coqTermApp @@ (coqTermQualid @@ string "inl") @@ list [encodeTerm @@ var "env" @@ var "l"])
      (lambda "r" $ coqTermApp @@ (coqTermQualid @@ string "inr") @@ list [encodeTerm @@ var "env" @@ var "r"])
      (var "e"),
    _Term_inject>>: "inj" ~> lets [
      "uname">: Core.injectionTypeName $ var "inj",
      "ufield">: Core.injectionField $ var "inj",
      "fname">: Core.fieldName $ var "ufield",
      "fterm">: Core.fieldTerm $ var "ufield",
      "constrName">: unionConstructorName
        @@ (unwrap _Name @@ var "uname")
        @@ (unwrap _Name @@ var "fname")] $
      coqTermApp
        @@ (coqTermQualid @@ (resolveQualifiedName @@ var "env" @@ var "constrName"))
        @@ list [encodeTerm @@ var "env" @@ var "fterm"],
    _Term_lambda>>: "lam" ~> encodeLambdaTerm @@ var "env" @@ var "lam",
    _Term_let>>: "lt" ~> lets [
      "bindings">: Core.letBindings $ var "lt",
      "body">: Core.letBody $ var "lt"] $
      Lists.foldr
        (lambdas ["binding", "acc"] $ lets [
          "bname">: Core.bindingName $ var "binding",
          "bterm">: Core.bindingTerm $ var "binding",
          "safeName">: sanitizeVar @@ (unwrap _Name @@ var "bname"),
          "recursive">: termReferencesVar @@ var "bname" @@ var "bterm",
          "recBody">: coqTermApp @@ (coqTermQualid @@ string "hydra_fix") @@ list [
            inject C._Term C._Term_forallOrFun $
              inject C._ForallOrFun C._ForallOrFun_fun $
                record C._Fun [
                  C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
                    inject C._Binder C._Binder_name (coqName @@ var "safeName")]),
                  C._Fun_body>>: encodeTerm @@ var "env" @@ var "bterm"]],
          "boundTerm">: Logic.ifElse (var "recursive") (var "recBody") (encodeTerm @@ var "env" @@ var "bterm")] $
          inject C._Term C._Term_let $
            record C._Let [
              C._Let_bindings>>:
                inject C._LetBindings C._LetBindings_named $
                  record C._LetNamed [
                    C._LetNamed_binder>>: record C._LetBinder [
                      C._LetBinder_name>>: coqName @@ var "safeName",
                      C._LetBinder_type>>: (nothing :: TTerm (Maybe C.Type)),
                      C._LetBinder_term>>: var "boundTerm"],
                    C._LetNamed_binders>>: list ([] :: [TTerm C.LetBinder])],
              C._Let_in>>: var "acc"])
        (encodeTerm @@ var "env" @@ var "body")
        (var "bindings"),
    _Term_list>>: "xs" ~>
      Lists.foldr
        (lambdas ["el", "acc"] $ coqTermApp @@ (coqTermQualid @@ string "cons") @@ list [
          encodeTerm @@ var "env" @@ var "el", var "acc"])
        (coqTermQualid @@ string "nil")
        (var "xs"),
    _Term_literal>>: "l" ~> encodeLiteral @@ var "l",
    _Term_map>>: constant (coqTermQualid @@ string "nil"),
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (coqTermQualid @@ string "None")
      (lambda "v" $ coqTermApp @@ (coqTermQualid @@ string "Some") @@ list [encodeTerm @@ var "env" @@ var "v"])
      (var "mt"),
    _Term_pair>>: "p" ~>
      coqTermApp @@ (coqTermQualid @@ string "pair") @@ list [
        encodeTerm @@ var "env" @@ Pairs.first (var "p"),
        encodeTerm @@ var "env" @@ Pairs.second (var "p")],
    _Term_project>>: "pr" ~> encodeProjectionElim @@ var "env" @@ var "pr",
    _Term_record>>: "r" ~> lets [
      "rname">: Core.recordTypeName $ var "r",
      "rfields">: Core.recordFields $ var "r"] $
      Logic.ifElse (Lists.null $ var "rfields")
        (coqTermQualid @@ string "tt")
        (coqTermApp
          @@ (coqTermQualid @@ (resolveQualifiedName @@ var "env"
                @@ (Strings.cat2 (string "Build_") (unwrap _Name @@ var "rname"))))
          @@ Lists.map
            (lambda "f" $ encodeTerm @@ var "env" @@ (Core.fieldTerm $ var "f"))
            (var "rfields")),
    _Term_set>>: constant (coqTermQualid @@ string "nil"),
    _Term_typeApplication>>: "ta" ~> lets [
      "body">: Core.typeApplicationTermBody $ var "ta",
      "tyArg">: Core.typeApplicationTermType $ var "ta",
      "encoded">: encodeTerm @@ var "env" @@ var "body",
      -- A ground (free-variable-free) type argument can safely be emitted as
      -- a Coq type annotation `(t : T)`. A type that mentions a Hydra type
      -- variable cannot, because such variables come from outer `TypeLambda`
      -- binders that Hydra erases in Coq — the variable would be free.
      "isGround">: Sets.null (CoqUtils.collectFreeTypeVarsInType @@ var "tyArg")] $
      -- An empty container (None, [], empty map, empty set) cannot have its
      -- element type inferred by Coq from the surrounding context. Emit an
      -- explicit `(None : option T)` / `(nil : list T)` cast using the type
      -- argument from the enclosing TypeApplication, but only when the type
      -- is ground. Non-empty bodies are left untouched.
      Logic.ifElse (Logic.not $ var "isGround") (var "encoded") $
      -- `_Term_maybe Nothing` and `_Term_either (inl|inr)` need type
      -- annotations: Coq cannot infer the "other" type parameter of `option`
      -- / `sum` from context when these appear as record fields or function
      -- arguments. Empty lists/maps/sets are NOT annotated here because the
      -- TypeApplication's `type` field does not always match the Coq-side
      -- expected type (e.g. Hydra's `Map Name Term` vs Coq's
      -- `list (Name * Term)`), so a blind cast would produce the wrong type.
      cases _Term (var "body")
        (Just (var "encoded")) [
        _Term_maybe>>: "mt" ~> Maybes.maybe
          (coqTermCast @@ (coqTermQualid @@ string "None")
            @@ (coqTypeTerm @@ (coqTermApp @@ (coqTermQualid @@ string "option")
              @@ list [encodeType @@ var "env" @@ var "tyArg"])))
          (constant $ var "encoded")
          (var "mt"),
        -- Empty list: annotate with `list <tyArg>` when the element type is
        -- compound (either, pair, map). This handles `lefts nil` / `rights nil`
        -- (tyArg = sum) and similar. Simple tyArgs like `Name` are skipped
        -- because Hydra sometimes passes a wrong single-type element (e.g.
        -- `Name` for an annotation field whose Coq type is `list (Name * Term)`).
        _Term_list>>: "xs" ~> Logic.ifElse
          (Logic.and (Lists.null $ var "xs")
            (cases _Type (var "tyArg") (Just (boolean False)) [
              _Type_either>>: constant (boolean True),
              _Type_pair>>: constant (boolean True),
              _Type_map>>: constant (boolean True)]))
          (coqTermCast @@ (coqTermQualid @@ string "nil")
            @@ (coqTypeTerm @@ (coqTermApp @@ (coqTermQualid @@ string "list")
              @@ list [encodeType @@ var "env" @@ var "tyArg"])))
          (var "encoded"),
        -- For `_Term_either`, Hydra wraps in TWO nested TypeApplications
        -- (one per type parameter): `TypeApp (TypeApp (Either x) L) R`.
        -- We receive the outer one here. If `tyArg` is already a full
        -- `_Type_either`, use it directly. Otherwise, fall through.
        _Term_either>>: "e" ~>
          cases _Type (var "tyArg")
            (Just (var "encoded")) [
            _Type_either>>: "et" ~> lets [
              "sumTy">: coqTypeTerm @@ (coqTermApp @@ (coqTermQualid @@ string "sum") @@ list [
                encodeType @@ var "env" @@ (Core.eitherTypeLeft $ var "et"),
                encodeType @@ var "env" @@ (Core.eitherTypeRight $ var "et")])] $
              coqTermCast @@ var "encoded" @@ var "sumTy"],
        -- Nested TypeApplication: body is another TypeApplication.
        -- Peel one layer and recurse. This handles `TypeApp (TypeApp (Either x) L) R`
        -- where the inner body is `_Term_either`.
        _Term_typeApplication>>: "innerTa" ~> lets [
          "innerBody">: Core.typeApplicationTermBody $ var "innerTa",
          "innerTyArg">: Core.typeApplicationTermType $ var "innerTa",
          "innerEncoded">: encodeTerm @@ var "env" @@ var "innerBody"] $
          cases _Term (var "innerBody")
            (Just (var "encoded")) [
            _Term_either>>: "innerE" ~> lets [
              "sumTy">: coqTypeTerm @@ (coqTermApp @@ (coqTermQualid @@ string "sum") @@ list [
                encodeType @@ var "env" @@ var "innerTyArg",
                encodeType @@ var "env" @@ var "tyArg"])] $
              coqTermCast @@ var "innerEncoded" @@ var "sumTy"]],
    _Term_typeLambda>>: "tl" ~> encodeTerm @@ var "env" @@ (Core.typeLambdaBody $ var "tl"),
    _Term_unit>>: constant (coqTermQualid @@ string "tt"),
    _Term_unwrap>>: "n" ~> encodeWrapElim @@ var "n",
    _Term_variable>>: "n" ~>
      coqTermQualid @@ (resolveQualifiedName @@ var "env" @@ (unwrap _Name @@ var "n")),
    _Term_wrap>>: "wt" ~> encodeTerm @@ var "env" @@ (Core.wrappedTermBody $ var "wt")]

-- | Wrap an encoded term body as a Coq Definition sentence content.
encodeTermDefinition :: TTermDefinition (CE.CoqEnvironment -> String -> Term -> C.SentenceContent)
encodeTermDefinition = define "encodeTermDefinition" $
  doc "Build a Coq `Definition name := body.` sentence from a Hydra term" $
  lambdas ["env", "name", "body"] $
    inject C._SentenceContent C._SentenceContent_definition $
      record C._Definition [
        C._Definition_locality>>: (nothing :: TTerm (Maybe C.Locality)),
        C._Definition_name>>: coqIdent @@ var "name",
        C._Definition_binders>>: list ([] :: [TTerm C.Binder]),
        C._Definition_type>>: (nothing :: TTerm (Maybe C.Type)),
        C._Definition_body>>: encodeTerm @@ var "env" @@ var "body"]

-- | Encode a (name, Term) pair as a Coq Sentence.
encodeTermDefinitionPair :: TTermDefinition (CE.CoqEnvironment -> (String, Term) -> C.Sentence)
encodeTermDefinitionPair = define "encodeTermDefinitionPair" $
  doc "Wrap encodeTermDefinition in a Coq Sentence with no leading comment" $
  lambdas ["env", "ed"] $
    record C._Sentence [
      C._Sentence_comment>>: (nothing :: TTerm (Maybe C.Comment)),
      C._Sentence_content>>: encodeTermDefinition @@ var "env" @@ Pairs.first (var "ed") @@ Pairs.second (var "ed")]

-- | Wrap an encoded type body as a Coq Definition sentence content (typed `: Type`).
encodeTypeDefinition :: TTermDefinition (CE.CoqEnvironment -> String -> Type -> C.SentenceContent)
encodeTypeDefinition = define "encodeTypeDefinition" $
  doc "Build a Coq `Definition name : Type := body.` sentence from a Hydra type" $
  lambdas ["env", "name", "ty"] $
    inject C._SentenceContent C._SentenceContent_definition $
      record C._Definition [
        C._Definition_locality>>: (nothing :: TTerm (Maybe C.Locality)),
        C._Definition_name>>: coqIdent @@ var "name",
        C._Definition_binders>>: list ([] :: [TTerm C.Binder]),
        C._Definition_type>>: just (coqTypeTerm @@ (coqTermQualid @@ string "Type")),
        C._Definition_body>>: encodeType @@ var "env" @@ var "ty"]

-- | Encode a (name, Type) pair as a Coq Sentence.
encodeTypeDefinitionPair :: TTermDefinition (CE.CoqEnvironment -> (String, Type) -> C.Sentence)
encodeTypeDefinitionPair = define "encodeTypeDefinitionPair" $
  doc "Wrap encodeTypeDefinition in a Coq Sentence with no leading comment" $
  lambdas ["env", "td"] $
    record C._Sentence [
      C._Sentence_comment>>: (nothing :: TTerm (Maybe C.Comment)),
      C._Sentence_content>>: encodeTypeDefinition @@ var "env" @@ Pairs.first (var "td") @@ Pairs.second (var "td")]

-- | Build a Coq Constructor from a Hydra union FieldType.
encodeUnionConstructor :: TTermDefinition (CE.CoqEnvironment -> String -> FieldType -> C.Constructor)
encodeUnionConstructor = define "encodeUnionConstructor" $
  doc "Construct a Coq Inductive Constructor line `Name_Tag : body -> Name` for a union variant" $
  lambdas ["env", "typeName", "f"] $ lets [
    "ufn">: Core.fieldTypeName $ var "f",
    "uft">: Core.fieldTypeType $ var "f",
    "constrName">: Strings.cat (list [
      var "typeName", string "_",
      Formatting.capitalize @@ (unwrap _Name @@ var "ufn")])] $
    record C._Constructor [
      C._Constructor_name>>: coqIdent @@ var "constrName",
      C._Constructor_binders>>: list ([] :: [TTerm C.Binder]),
      C._Constructor_type>>: just $ coqTypeTerm @@
        (coqArrow
          @@ (encodeType @@ var "env" @@ var "uft")
          @@ (coqTermQualid @@ var "typeName"))]

-- | Translate a union eliminator (Hydra `TermCases`) into a Coq `fun x_ => match x_ with ... end`.
encodeUnionElim :: TTermDefinition (CE.CoqEnvironment -> CaseStatement -> C.Term)
encodeUnionElim = define "encodeUnionElim" $
  doc "Build a Coq match expression from a Hydra union eliminator. Uses the constructor-count map in the environment to decide whether the match is exhaustive: if so, an explicit default is suppressed; if not and the kernel didn't provide one, inserts `| _ => hydra_unreachable`." $
  lambdas ["env", "cs"] $ lets [
    "csName">: Core.caseStatementTypeName $ var "cs",
    "csCases">: Core.caseStatementCases $ var "cs",
    "csDefault">: Core.caseStatementDefault $ var "cs",
    "csLocalName">: localTypeName @@ (unwrap _Name @@ var "csName"),
    "expectedCount">: Maps.lookup (var "csLocalName")
      (project CE._CoqEnvironment CE._CoqEnvironment_constructorCounts @@ var "env"),
    "caseCount">: Lists.length $ var "csCases",
    "baseEqs">: Lists.map
      (lambda "c" $ lets [
        "cfn">: Core.fieldName $ var "c",
        "cft">: Core.fieldTerm $ var "c",
        "constr">: resolveQualifiedName @@ var "env" @@ (unionConstructorName
          @@ (unwrap _Name @@ var "csName")
          @@ (unwrap _Name @@ var "cfn"))] $
        Logic.ifElse (isUnitLambda @@ var "cft")
          -- Unit-typed variant: wildcard pattern for unused unit arg.
          (record C._Equation [
            C._Equation_pattern>>: list [list [
              inject C._Pattern C._Pattern_pattern $
                inject C._Pattern10 C._Pattern10_qualiid $
                  record C._Pattern10_Qualid [
                    C._Pattern10_Qualid_qualid>>: coqQualid @@ var "constr",
                    C._Pattern10_Qualid_patterns>>: list [
                      record C._Pattern1 [
                        C._Pattern1_pattern>>:
                          inject C._Pattern0 C._Pattern0_qualid (coqQualid @@ string "_"),
                        C._Pattern1_scope>>: (nothing :: TTerm (Maybe C.ScopeKey))]]]]],
            C._Equation_term>>: encodeTerm @@ var "env" @@ (unitLambdaBody @@ var "cft")])
          -- Normal variant: bind v_ and apply the case body to it.
          (record C._Equation [
            C._Equation_pattern>>: list [list [
              inject C._Pattern C._Pattern_pattern $
                inject C._Pattern10 C._Pattern10_qualiid $
                  record C._Pattern10_Qualid [
                    C._Pattern10_Qualid_qualid>>: coqQualid @@ var "constr",
                    C._Pattern10_Qualid_patterns>>: list [
                      record C._Pattern1 [
                        C._Pattern1_pattern>>:
                          inject C._Pattern0 C._Pattern0_qualid (coqQualid @@ string "v_"),
                        C._Pattern1_scope>>: (nothing :: TTerm (Maybe C.ScopeKey))]]]]],
            C._Equation_term>>:
              coqTermApp @@ (encodeTerm @@ var "env" @@ var "cft") @@ list [coqTermQualid @@ string "v_"]]))
      (var "csCases"),
    "wildcardEq">: lambda "body" $ record C._Equation [
      C._Equation_pattern>>: list [list [
        inject C._Pattern C._Pattern_pattern $
          inject C._Pattern10 C._Pattern10_qualiid $
            record C._Pattern10_Qualid [
              C._Pattern10_Qualid_qualid>>: coqQualid @@ string "_",
              C._Pattern10_Qualid_patterns>>: list ([] :: [TTerm C.Pattern1])]]],
      C._Equation_term>>: var "body"],
    "defaultEqs">: Maybes.maybe
      -- No explicit default: if the match is non-exhaustive, synthesize one as
      -- `| _ => hydra_unreachable` (replacing the old addPartialMatchCatchAll pass).
      -- If the match is exhaustive (or we lack the count), emit no default.
      (Logic.ifElse
        (Maybes.maybe (boolean False)
          (lambda "n" $ Logic.not $ Equality.gte (var "caseCount") (var "n"))
          (var "expectedCount"))
        (list [var "wildcardEq" @@ (coqTermQualid @@ string "hydra_unreachable")])
        (list ([] :: [TTerm C.Equation])))
      -- Kernel provided an explicit default: if the non-default cases already cover
      -- every constructor, drop it (replacing the old removeRedundantDefaults pass);
      -- otherwise keep it.
      (lambda "defT" $ Logic.ifElse
        (Maybes.maybe (boolean False)
          (lambda "n" $ Equality.gte (var "caseCount") (var "n"))
          (var "expectedCount"))
        (list ([] :: [TTerm C.Equation]))
        (list [var "wildcardEq" @@ (encodeTerm @@ var "env" @@ var "defT")]))
      (var "csDefault"),
    "allEqs">: Lists.concat2 (var "baseEqs") (var "defaultEqs")] $
    inject C._Term C._Term_forallOrFun $
      inject C._ForallOrFun C._ForallOrFun_fun $
        record C._Fun [
          C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
            inject C._Binder C._Binder_name (coqName @@ string "x_")]),
          C._Fun_body>>:
            inject C._Term C._Term_term100 $
              inject C._Term100 C._Term100_term10 $
                inject C._Term10 C._Term10_oneTerm $
                  inject C._OneTerm C._OneTerm_term1 $
                    inject C._Term1 C._Term1_term0 $
                      inject C._Term0 C._Term0_match $
                        record C._Match [
                          C._Match_caseItems>>: list [
                            record C._CaseItem [
                              C._CaseItem_term>>:
                                inject C._Term100 C._Term100_term10 $
                                  inject C._Term10 C._Term10_oneTerm $
                                    inject C._OneTerm C._OneTerm_explicit $
                                      record C._QualidAnnotated [
                                        C._QualidAnnotated_qualid>>: coqQualid @@ string "x_",
                                        C._QualidAnnotated_univAnnot>>: (nothing :: TTerm (Maybe C.UnivAnnot))],
                              C._CaseItem_as>>: (nothing :: TTerm (Maybe Name)),
                              C._CaseItem_in>>: (nothing :: TTerm (Maybe C.Pattern))]],
                          C._Match_return>>: (nothing :: TTerm (Maybe C.Term100)),
                          C._Match_pipe>>: true,
                          C._Match_equations>>: var "allEqs"]]

-- | Translate a Hydra wrap eliminator (`TermUnwrap`) into a Coq identity lambda.
encodeWrapElim :: TTermDefinition (Name -> C.Term)
encodeWrapElim = define "encodeWrapElim" $
  doc "A Hydra wrap eliminator is just the identity on the wrapped object in Coq" $
  lambda "_n" $
    inject C._Term C._Term_forallOrFun $
      inject C._ForallOrFun C._ForallOrFun_fun $
        record C._Fun [
          C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
            inject C._Binder C._Binder_name (coqName @@ string "w_")]),
          C._Fun_body>>: coqTermQualid @@ string "w_"]

-- | Walk a Hydra term, collecting the leading lambda binders as Coq Binders.
extractLambdaBinders :: TTermDefinition (CE.CoqEnvironment -> Term -> [C.Binder])
extractLambdaBinders = define "extractLambdaBinders" $
  doc "Collect a chain of leading lambdas as Coq binders, converting type annotations as well" $
  lambdas ["env", "tm"] $ cases _Term (var "tm") (Just $ list ([] :: [TTerm C.Binder])) [
    _Term_annotated>>: "at" ~>
      extractLambdaBinders @@ var "env" @@ (Core.annotatedTermBody $ var "at"),
    _Term_lambda>>: "lam" ~> lets [
      "param">: Core.lambdaParameter $ var "lam",
      "mDomain">: Core.lambdaDomain $ var "lam",
      "binder">: Maybes.maybe
        (inject C._Binder C._Binder_name (coqName @@ (unwrap _Name @@ var "param")))
        (lambda "domTy" $ inject C._Binder C._Binder_type $
          record C._TypeBinders [
            C._TypeBinders_names>>: list [coqName @@ (unwrap _Name @@ var "param")],
            C._TypeBinders_type>>: coqTypeTerm @@ (encodeType @@ var "env" @@ var "domTy")])
        (var "mDomain")] $
      Lists.cons (var "binder") (extractLambdaBinders @@ var "env" @@ (Core.lambdaBody $ var "lam"))]

-- | Test whether a domain type is the unit type (possibly wrapped in annotations).
isUnitDomain :: TTermDefinition (Maybe Type -> Bool)
isUnitDomain = define "isUnitDomain" $
  doc "True if the Maybe Type is the unit type, looking through annotations" $
  lambda "mty" $ Maybes.maybe (boolean False)
    (lambda "ty" $ cases _Type (var "ty") (Just (boolean False)) [
      _Type_unit>>: constant true,
      _Type_record>>: "fs" ~> Lists.null (var "fs"),
      _Type_annotated>>: "at" ~>
        isUnitDomain @@ just (Core.annotatedTypeBody $ var "at")])
    (var "mty")

-- | Test whether a Hydra term is a lambda that ignores its (unit-typed) parameter.
isUnitLambda :: TTermDefinition (Term -> Bool)
isUnitLambda = define "isUnitLambda" $
  doc "Detect a lambda over the unit type whose parameter is not referenced in the body" $
  lambda "tm" $ cases _Term (var "tm") (Just (boolean False)) [
    _Term_annotated>>: "at" ~>
      isUnitLambda @@ (Core.annotatedTermBody $ var "at"),
    _Term_lambda>>: "lam" ~> lets [
      "unused">: Logic.not $ termReferencesVar
        @@ (Core.lambdaParameter $ var "lam")
        @@ (Core.lambdaBody $ var "lam")] $
      Logic.and
        (isUnitDomain @@ (Core.lambdaDomain $ var "lam"))
        (var "unused")]

-- | Given a possibly-qualified name (e.g. "hydra.core.Term"), return its local
-- part sanitized to avoid Coq reserved words. Used as the lookup key for the
-- constructor-count map in encodeUnionElim.
localTypeName :: TTermDefinition (String -> String)
localTypeName = define "localTypeName" $
  doc "Take the last dot-separated segment of a (possibly) qualified Hydra name and sanitize it" $
  lambda "s" $ lets [
    "parts">: Strings.splitOn (string ".") (var "s"),
    "localPart">: Maybes.fromMaybe (var "s") (Lists.maybeLast $ var "parts")] $
    sanitizeVar @@ var "localPart"

-- | Combine a universe of type and term definitions into a single Coq Document.
-- The CoqEnvironment is threaded through every encoder so that match expressions
-- can decide whether to emit a default arm and qualified references can be
-- rewritten to their short forms.
moduleToCoq :: TTermDefinition (CE.CoqEnvironment -> [(String, Type)] -> [(String, Term)] -> C.Document)
moduleToCoq = define "moduleToCoq" $
  doc "Build a Coq Document from lists of type definitions and term definitions" $
  lambdas ["env", "typeDefs", "termDefs"] $ lets [
    "typesSentences">: Lists.map (lambda "td" $ encodeTypeDefinitionPair @@ var "env" @@ var "td") (var "typeDefs"),
    "termsSentences">: Lists.map (lambda "ed" $ encodeTermDefinitionPair @@ var "env" @@ var "ed") (var "termDefs")] $
    record C._Document [
      C._Document_sentences>>: Lists.concat $ list [
        list [standardImports],
        var "typesSentences",
        var "termsSentences"]]

-- | Rename a small set of hydra.lib.* identifiers whose stripped form would
-- collide with a Coq reserved word (e.g., `lists.at` -> `lists.at_`).
renameLibKeyword :: TTermDefinition (String -> String)
renameLibKeyword = define "renameLibKeyword" $
  doc "Rewrite a stripped hydra.lib.<mod>.<func> name to avoid Coq keyword collisions" $
  lambda "s" $
    Logic.ifElse (Equality.equal (var "s") (string "lists.at"))
      (string "lists.at_") $
    Logic.ifElse (Equality.equal (var "s") (string "math.mod"))
      (string "math.mod_")
      (var "s")

-- | Build a Require Import sentence for a list of module names.
requireImportSentence :: TTermDefinition ([String] -> C.Sentence)
requireImportSentence = define "requireImportSentence" $
  doc "Emit a Coq `Require Import m1 m2 ...` sentence with a `Standard library imports` comment" $
  lambda "mods" $
    record C._Sentence [
      C._Sentence_comment>>: just (wrap C._Comment $ string "Standard library imports"),
      C._Sentence_content>>:
        inject C._SentenceContent C._SentenceContent_requireImport $
          record C._RequireImport [
            C._RequireImport_from>>: (nothing :: TTerm (Maybe C.Qualid)),
            C._RequireImport_require>>: true,
            C._RequireImport_qualification>>:
              just (inject C._ImportQualification C._ImportQualification_import unit),
            C._RequireImport_modules>>:
              Lists.map (lambda "m" $ coqQualid @@ var "m") (var "mods")]]

-- | Rewrite a (possibly qualified) Hydra name into the form that should appear
-- in emitted Coq source. Replaces the former text post-processor
-- `stripHydraQualifications` in `packages/hydra-coq/.../Hydra/Coq/Generate.hs`.
--
-- * `hydra.lib.<mod>.<func>` -> `<mod>.<func>` (with keyword rewrites).
-- * `Build_hydra.<ns>.<x>`   -> `Build_<sanitizeVar x>`.
-- * `hydra.<ns>.<x>`         -> fully qualified `<ns>.<sanitizeVar x>` when
--   `<x>` is in `ambiguousNames` and `<ns>` is not the current module's
--   namespace, or when `<ns>` matches one of the hardcoded
--   collision-prone modules (e.g. `parsers`); otherwise `<sanitizeVar x>`.
-- * Otherwise the input is returned unchanged, after `sanitizeVar`.
resolveQualifiedName :: TTermDefinition (CE.CoqEnvironment -> String -> String)
resolveQualifiedName = define "resolveQualifiedName" $
  doc "Resolve a (possibly qualified) Hydra identifier to the form that should appear in Coq source" $
  lambdas ["env", "s"] $ lets [
    "parts">: Strings.splitOn (string ".") (var "s"),
    "head1">: Maybes.fromMaybe (var "s") (Lists.maybeHead (var "parts")),
    "currentNs">: project CE._CoqEnvironment CE._CoqEnvironment_currentNamespace @@ var "env",
    "ambig">: project CE._CoqEnvironment CE._CoqEnvironment_ambiguousNames @@ var "env"] $
    -- Coq.<name> : synthetic marker (from `typeToTerm`) for a Coq-builtin
    -- type constructor referenced in a term position. Emit the tail segment
    -- raw, bypassing sanitizeVar — otherwise `Coq.list` would collide with a
    -- user-level lambda parameter named `list` and get escaped to `list_`.
    Logic.ifElse (Equality.equal (var "head1") (string "Coq"))
      (Maybes.fromMaybe (var "s") (Lists.maybeLast (var "parts"))) $
    -- Build_hydra.<ns>.<x> : strip to Build_<sanitized local>.
    Logic.ifElse (Equality.equal (var "head1") (string "Build_hydra"))
      (Strings.cat2 (string "Build_") (sanitizeStripped @@ (Maybes.fromMaybe (var "s") (Lists.maybeLast (var "parts"))))) $
    Logic.ifElse (Equality.equal (var "head1") (string "hydra"))
      (lets [
        "rest">: Lists.drop (int32 1) (var "parts"),
        "head2">: Maybes.fromMaybe (string "") (Lists.maybeHead (var "rest"))] $
        -- hydra.lib.<mod>.<func> : keep module.function, with keyword rewrites.
        Logic.ifElse (Equality.equal (var "head2") (string "lib"))
          (renameLibKeyword @@ (Strings.intercalate (string ".") (Lists.drop (int32 1) (var "rest")))) $
          -- hydra.<ns>...<x> : compute local name + source namespace.
          lets [
            "localRaw">: Maybes.fromMaybe (var "s") (Lists.maybeLast (var "parts")),
            "localN">: sanitizeStripped @@ var "localRaw",
            "sourceNs">: Strings.intercalate (string ".") (Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeInit (var "parts"))),
            "isCurrent">: Equality.equal (var "sourceNs") (var "currentNs"),
            -- Ambiguity check is against the raw (unsanitised) local name,
            -- since the ambiguous-names set in the environment is populated
            -- from pre-sanitisation Hydra names (`localName td`).
            "isAmbig">: Sets.member (var "localRaw") (var "ambig"),
            -- Non-lib modules whose function names may collide with other imports
            -- (e.g. `parsers.map` vs `maps.map`). Cross-namespace references to
            -- these are kept in `<mod>.<func>` form.
            "isCollisionProne">: Logic.and
              (Equality.equal (Lists.length (var "parts")) (int32 3))
              (Logic.and
                (Equality.equal (var "head2") (string "parsers"))
                (Logic.not (var "isCurrent")))] $
          Logic.ifElse (Logic.and (var "isAmbig") (Logic.not (var "isCurrent")))
            (Strings.cat (list [var "sourceNs", string ".", var "localN"])) $
          Logic.ifElse (var "isCollisionProne")
            (Strings.cat (list [
              sanitizeStripped @@ var "head2",
              string ".",
              sanitizeStripped @@ var "localRaw"]))
            (var "localN"))
      -- Non-hydra identifiers: apply the WIDE `sanitizeVar`, matching the
      -- pre-refactor DSL encoder's behaviour for local (unqualified) term
      -- variable references (lambda parameters like `at`, `end` etc., which
      -- must become `at_`, `end_` to avoid Coq keyword conflicts).
      (sanitizeVar @@ var "s")

-- | The fixed set of imports used at the head of every generated .v file.
standardImports :: TTermDefinition C.Sentence
standardImports = define "standardImports" $
  doc "The Coq stdlib modules plus the hand-written hydra.lib.base axioms" $
  requireImportSentence @@ list [
    string "Stdlib.Strings.String",
    string "Stdlib.Lists.List",
    string "Stdlib.ZArith.ZArith",
    string "Stdlib.QArith.QArith",
    string "hydra.lib.base"]

-- | Strip a chain of leading lambdas from a term, returning the inner body.
stripLambdas :: TTermDefinition (Term -> Term)
stripLambdas = define "stripLambdas" $
  doc "Peel off leading lambdas and annotations, returning the first non-lambda body" $
  lambda "tm" $ cases _Term (var "tm") (Just (var "tm")) [
    _Term_annotated>>: "at" ~>
      stripLambdas @@ (Core.annotatedTermBody $ var "at"),
    _Term_lambda>>: "lam" ~>
      stripLambdas @@ (Core.lambdaBody $ var "lam")]

-- | Test whether a variable name appears free within a term.
termReferencesVar :: TTermDefinition (Name -> Term -> Bool)
termReferencesVar = define "termReferencesVar" $
  doc "Syntactic free-variable check over the shapes encodeTerm walks through" $
  lambdas ["name", "tm"] $ cases _Term (var "tm") (Just (boolean False)) [
    _Term_variable>>: "v" ~> Equality.equal (var "v") (var "name"),
    _Term_annotated>>: "at" ~>
      termReferencesVar @@ var "name" @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~> Logic.or
      (termReferencesVar @@ var "name" @@ (Core.applicationFunction $ var "app"))
      (termReferencesVar @@ var "name" @@ (Core.applicationArgument $ var "app")),
    _Term_lambda>>: "lam" ~>
      termReferencesVar @@ var "name" @@ (Core.lambdaBody $ var "lam"),
    _Term_cases>>: "cs" ~> Logic.or
      (listAny
        (lambda "f" $ termReferencesVar @@ var "name" @@ (Core.fieldTerm $ var "f"))
        (Core.caseStatementCases $ var "cs"))
      (Maybes.maybe (boolean False)
        (lambda "d" $ termReferencesVar @@ var "name" @@ var "d")
        (Core.caseStatementDefault $ var "cs")),
    _Term_let>>: "lt" ~> Logic.or
      (listAny
        (lambda "b" $ termReferencesVar @@ var "name" @@ (Core.bindingTerm $ var "b"))
        (Core.letBindings $ var "lt"))
      (termReferencesVar @@ var "name" @@ (Core.letBody $ var "lt")),
    _Term_list>>: "xs" ~>
      listAny (lambda "el" $ termReferencesVar @@ var "name" @@ var "el") (var "xs"),
    _Term_maybe>>: "mt" ~> Maybes.maybe (boolean False)
      (lambda "el" $ termReferencesVar @@ var "name" @@ var "el")
      (var "mt"),
    _Term_pair>>: "p" ~> Logic.or
      (termReferencesVar @@ var "name" @@ Pairs.first (var "p"))
      (termReferencesVar @@ var "name" @@ Pairs.second (var "p")),
    _Term_record>>: "r" ~>
      listAny
        (lambda "f" $ termReferencesVar @@ var "name" @@ (Core.fieldTerm $ var "f"))
        (Core.recordFields $ var "r"),
    _Term_inject>>: "inj" ~>
      termReferencesVar @@ var "name" @@
        (Core.fieldTerm $ Core.injectionField $ var "inj"),
    _Term_either>>: "e" ~> Eithers.either_
      (lambda "l" $ termReferencesVar @@ var "name" @@ var "l")
      (lambda "r" $ termReferencesVar @@ var "name" @@ var "r")
      (var "e"),
    _Term_typeApplication>>: "ta" ~>
      termReferencesVar @@ var "name" @@ (Core.typeApplicationTermBody $ var "ta"),
    _Term_typeLambda>>: "tl" ~>
      termReferencesVar @@ var "name" @@ (Core.typeLambdaBody $ var "tl"),
    _Term_wrap>>: "wt" ~>
      termReferencesVar @@ var "name" @@ (Core.wrappedTermBody $ var "wt")]

-- | Build a union-constructor name by combining a (possibly qualified) type name and a field.
unionConstructorName :: TTermDefinition (String -> String -> String)
unionConstructorName = define "unionConstructorName" $
  doc "Combine a type name and field name into a constructor identifier, preserving the namespace prefix" $
  lambdas ["typeName", "fieldName"] $ lets [
    "parts">: Strings.splitOn (string ".") (var "typeName"),
    "localPart">: Maybes.fromMaybe (var "typeName") (Lists.maybeLast $ var "parts"),
    "prefixParts">: Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeInit $ var "parts"),
    "prefix">: Logic.ifElse (Lists.null $ var "prefixParts")
      (string "")
      (Strings.cat2 (Strings.intercalate (string ".") (var "prefixParts")) (string ".")),
    "sanitized">: sanitizeVar @@ var "localPart"] $
    Strings.cat (list [
      var "prefix",
      var "sanitized",
      string "_",
      Formatting.capitalize @@ var "fieldName"])

-- | Extract the body of a unit-consuming lambda, skipping lambda/annotation wrappers.
unitLambdaBody :: TTermDefinition (Term -> Term)
unitLambdaBody = define "unitLambdaBody" $
  doc "Peel the outer unit lambda off a term, returning the body" $
  lambda "tm" $ cases _Term (var "tm") (Just (var "tm")) [
    _Term_annotated>>: "at" ~>
      unitLambdaBody @@ (Core.annotatedTermBody $ var "at"),
    _Term_lambda>>: "lam" ~> (Core.lambdaBody $ var "lam")]
