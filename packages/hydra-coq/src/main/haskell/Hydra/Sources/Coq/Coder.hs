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
import qualified Hydra.Sources.Coq.Language                as CoqLanguage
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as DL
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for Coq AST
import Hydra.Ast
import qualified Hydra.Coq.Syntax as C


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
    (CoqSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Coq code generator: converts Hydra modules to Coq source"
  where
    definitions = [
      toDefinition coqArrow,
      toDefinition coqIdent,
      toDefinition coqName,
      toDefinition coqQualid,
      toDefinition coqTermApp,
      toDefinition coqTermQualid,
      toDefinition coqTypeTerm,
      toDefinition encodeAxiomDefinitionPair,
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
      toDefinition requireImportSentence,
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

-- | Encode a Hydra LiteralType to a Coq Term (qualid reference).
encodeLiteralType :: TTermDefinition (LiteralType -> C.Term)
encodeLiteralType = define "encodeLiteralType" $
  doc "Map a Hydra LiteralType to its Coq stdlib counterpart" $
  lambda "lt" $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_boolean>>: constant (coqTermQualid @@ string "bool"),
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
encodeType :: TTermDefinition (Type -> C.Term)
encodeType = define "encodeType" $
  doc "Translate a Hydra Type into a Coq Term representing that type" $
  lambda "ty" $ cases _Type (var "ty") Nothing [
    _Type_annotated>>: "at" ~> encodeType @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~>
      coqTermApp
        @@ (encodeType @@ (Core.applicationTypeFunction $ var "app"))
        @@ list [encodeType @@ (Core.applicationTypeArgument $ var "app")],
    _Type_either>>: "et" ~>
      coqTermApp @@ (coqTermQualid @@ string "sum") @@ list [
        encodeType @@ (Core.eitherTypeLeft $ var "et"),
        encodeType @@ (Core.eitherTypeRight $ var "et")],
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
            C._Forall_type>>: coqTypeTerm @@ (encodeType @@ (Core.forallTypeBody $ var "ft"))],
    _Type_function>>: "ft" ~>
      coqArrow
        @@ (encodeType @@ (Core.functionTypeDomain $ var "ft"))
        @@ (encodeType @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_list>>: "t" ~>
      coqTermApp @@ (coqTermQualid @@ string "list") @@ list [encodeType @@ var "t"],
    _Type_literal>>: "lt" ~> encodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~>
      coqTermApp @@ (coqTermQualid @@ string "list") @@ list [
        coqTermApp @@ (coqTermQualid @@ string "prod") @@ list [
          encodeType @@ (Core.mapTypeKeys $ var "mt"),
          encodeType @@ (Core.mapTypeValues $ var "mt")]],
    _Type_maybe>>: "t" ~>
      coqTermApp @@ (coqTermQualid @@ string "option") @@ list [encodeType @@ var "t"],
    _Type_pair>>: "pt" ~>
      coqTermApp @@ (coqTermQualid @@ string "prod") @@ list [
        encodeType @@ (Core.pairTypeFirst $ var "pt"),
        encodeType @@ (Core.pairTypeSecond $ var "pt")],
    _Type_record>>: constant (coqTermQualid @@ string "unit"),
    _Type_set>>: "t" ~>
      coqTermApp @@ (coqTermQualid @@ string "list") @@ list [encodeType @@ var "t"],
    _Type_union>>: constant (coqTermQualid @@ string "unit"),
    _Type_unit>>: constant (coqTermQualid @@ string "unit"),
    _Type_variable>>: "n" ~> coqTermQualid @@ (unwrap _Name @@ var "n"),
    _Type_void>>: constant (coqTermQualid @@ string "Empty_set"),
    _Type_wrap>>: "wt" ~> encodeType @@ var "wt"]

-- | Encode a (name, Type) pair as a Coq Sentence containing an Axiom declaration.
-- Used for modules whose definitions cannot practically compile under coqc (e.g.
-- hydra.hoisting, hydra.inference) — their term definitions are replaced by
-- axioms of the same type, which the rest of the Coq build can still consume.
encodeAxiomDefinitionPair :: TTermDefinition ((String, Type) -> C.Sentence)
encodeAxiomDefinitionPair = define "encodeAxiomDefinitionPair" $
  doc "Produce `Axiom name : type.` from a (name, Hydra type) pair" $
  lambda "nt" $
    record C._Sentence [
      C._Sentence_comment>>: (nothing :: TTerm (Maybe C.Comment)),
      C._Sentence_content>>:
        inject C._SentenceContent C._SentenceContent_axiom $
          record C._AxiomDeclaration [
            C._AxiomDeclaration_name>>: coqIdent @@ Pairs.first (var "nt"),
            C._AxiomDeclaration_type>>: coqTypeTerm @@ (encodeType @@ Pairs.second (var "nt"))]]

-- | Encode a single Hydra Lambda term into a Coq `fun` term.
encodeLambdaTerm :: TTermDefinition (M.Map String Int -> Lambda -> C.Term)
encodeLambdaTerm = define "encodeLambdaTerm" $
  doc "Encode a Lambda into a Coq `fun` expression, sanitising the parameter name" $
  lambdas ["cc", "lam"] $ lets [
    "paramName">: sanitizeVar @@ (unwrap _Name @@ (Core.lambdaParameter $ var "lam")),
    "binder">: Maybes.maybe
      (inject C._Binder C._Binder_name (coqName @@ var "paramName"))
      (lambda "domTy" $ inject C._Binder C._Binder_type $
        record C._TypeBinders [
          C._TypeBinders_names>>: list [coqName @@ var "paramName"],
          C._TypeBinders_type>>: coqTypeTerm @@ (encodeType @@ var "domTy")])
      (Core.lambdaDomain $ var "lam")] $
    inject C._Term C._Term_forallOrFun $
      inject C._ForallOrFun C._ForallOrFun_fun $
        record C._Fun [
          C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [var "binder"]),
          C._Fun_body>>: encodeTerm @@ var "cc" @@ (Core.lambdaBody $ var "lam")]

-- | Encode a Hydra literal into a Coq Term.
encodeLiteral :: TTermDefinition (Literal -> C.Term)
encodeLiteral = define "encodeLiteral" $
  doc "Translate a Hydra literal into its Coq stdlib form, with disambiguating parentheses" $
  lambda "lit" $ cases _Literal (var "lit") Nothing [
    _Literal_boolean>>: "b" ~>
      Logic.ifElse (var "b") (coqTermQualid @@ string "true") (coqTermQualid @@ string "false"),
    _Literal_float>>: "fv" ~> cases _FloatValue (var "fv") Nothing [
      _FloatValue_bigfloat>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showBigfloat (var "v"), string ")"]),
      _FloatValue_float32>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showBigfloat (Literals.float32ToBigfloat $ var "v"), string ")"]),
      _FloatValue_float64>>: "v" ~> coqTermQualid @@ Strings.cat (list [
        string "(", Literals.showBigfloat (Literals.float64ToBigfloat $ var "v"), string ")"])],
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
encodeProjectionElim :: TTermDefinition (Projection -> C.Term)
encodeProjectionElim = define "encodeProjectionElim" $
  doc "Translate a Hydra record projection into a Coq lambda that pulls out the field" $
  lambda "p" $ lets [
    "fname">: Core.projectionField $ var "p"] $
    inject C._Term C._Term_forallOrFun $
      inject C._ForallOrFun C._ForallOrFun_fun $
        record C._Fun [
          C._Fun_binders>>: inject C._OpenBinders C._OpenBinders_binders (list [
            inject C._Binder C._Binder_name (coqName @@ string "r_")]),
          C._Fun_body>>: coqTermApp
            @@ (coqTermQualid @@ (unwrap _Name @@ var "fname"))
            @@ list [coqTermQualid @@ string "r_"]]

-- | Encode a Hydra Term into a Coq Term.
-- Not yet ported: TermCases (union eliminator), TermLet (let bindings).
-- Those two cases still need porting; for now encodeTerm covers every other Term variant.
encodeTerm :: TTermDefinition (M.Map String Int -> Term -> C.Term)
encodeTerm = define "encodeTerm" $
  doc "Translate a Hydra Term into its Coq Term counterpart. The constructor-count map lets encodeUnionElim decide whether a match is exhaustive and either drop a redundant default or add `| _ => hydra_unreachable`." $
  lambdas ["cc", "tm"] $ cases _Term (var "tm") Nothing [
    _Term_annotated>>: "at" ~> encodeTerm @@ var "cc" @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~>
      coqTermApp
        @@ (encodeTerm @@ var "cc" @@ (Core.applicationFunction $ var "app"))
        @@ list [encodeTerm @@ var "cc" @@ (Core.applicationArgument $ var "app")],
    _Term_cases>>: "cs" ~> encodeUnionElim @@ var "cc" @@ var "cs",
    _Term_either>>: "e" ~> Eithers.either_
      (lambda "l" $ coqTermApp @@ (coqTermQualid @@ string "inl") @@ list [encodeTerm @@ var "cc" @@ var "l"])
      (lambda "r" $ coqTermApp @@ (coqTermQualid @@ string "inr") @@ list [encodeTerm @@ var "cc" @@ var "r"])
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
        @@ (coqTermQualid @@ var "constrName")
        @@ list [encodeTerm @@ var "cc" @@ var "fterm"],
    _Term_lambda>>: "lam" ~> encodeLambdaTerm @@ var "cc" @@ var "lam",
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
                  C._Fun_body>>: encodeTerm @@ var "cc" @@ var "bterm"]],
          "boundTerm">: Logic.ifElse (var "recursive") (var "recBody") (encodeTerm @@ var "cc" @@ var "bterm")] $
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
        (encodeTerm @@ var "cc" @@ var "body")
        (var "bindings"),
    _Term_list>>: "xs" ~>
      Lists.foldr
        (lambdas ["el", "acc"] $ coqTermApp @@ (coqTermQualid @@ string "cons") @@ list [
          encodeTerm @@ var "cc" @@ var "el", var "acc"])
        (coqTermQualid @@ string "nil")
        (var "xs"),
    _Term_literal>>: "l" ~> encodeLiteral @@ var "l",
    _Term_map>>: constant (coqTermQualid @@ string "nil"),
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (coqTermQualid @@ string "None")
      (lambda "v" $ coqTermApp @@ (coqTermQualid @@ string "Some") @@ list [encodeTerm @@ var "cc" @@ var "v"])
      (var "mt"),
    _Term_pair>>: "p" ~>
      coqTermApp @@ (coqTermQualid @@ string "pair") @@ list [
        encodeTerm @@ var "cc" @@ Pairs.first (var "p"),
        encodeTerm @@ var "cc" @@ Pairs.second (var "p")],
    _Term_project>>: "pr" ~> encodeProjectionElim @@ var "pr",
    _Term_record>>: "r" ~> lets [
      "rname">: Core.recordTypeName $ var "r",
      "rfields">: Core.recordFields $ var "r"] $
      Logic.ifElse (Lists.null $ var "rfields")
        (coqTermQualid @@ string "tt")
        (coqTermApp
          @@ (coqTermQualid @@ Strings.cat2 (string "Build_") (unwrap _Name @@ var "rname"))
          @@ Lists.map
            (lambda "f" $ encodeTerm @@ var "cc" @@ (Core.fieldTerm $ var "f"))
            (var "rfields")),
    _Term_set>>: constant (coqTermQualid @@ string "nil"),
    _Term_typeApplication>>: "ta" ~> encodeTerm @@ var "cc" @@ (Core.typeApplicationTermBody $ var "ta"),
    _Term_typeLambda>>: "tl" ~> encodeTerm @@ var "cc" @@ (Core.typeLambdaBody $ var "tl"),
    _Term_unit>>: constant (coqTermQualid @@ string "tt"),
    _Term_unwrap>>: "n" ~> encodeWrapElim @@ var "n",
    _Term_variable>>: "n" ~>
      coqTermQualid @@ (sanitizeVar @@ (unwrap _Name @@ var "n")),
    _Term_wrap>>: "wt" ~> encodeTerm @@ var "cc" @@ (Core.wrappedTermBody $ var "wt")]

-- | Wrap an encoded term body as a Coq Definition sentence content.
encodeTermDefinition :: TTermDefinition (M.Map String Int -> String -> Term -> C.SentenceContent)
encodeTermDefinition = define "encodeTermDefinition" $
  doc "Build a Coq `Definition name := body.` sentence from a Hydra term" $
  lambdas ["cc", "name", "body"] $
    inject C._SentenceContent C._SentenceContent_definition $
      record C._Definition [
        C._Definition_locality>>: (nothing :: TTerm (Maybe C.Locality)),
        C._Definition_name>>: coqIdent @@ var "name",
        C._Definition_binders>>: list ([] :: [TTerm C.Binder]),
        C._Definition_type>>: (nothing :: TTerm (Maybe C.Type)),
        C._Definition_body>>: encodeTerm @@ var "cc" @@ var "body"]

-- | Encode a (name, Term) pair as a Coq Sentence.
encodeTermDefinitionPair :: TTermDefinition (M.Map String Int -> (String, Term) -> C.Sentence)
encodeTermDefinitionPair = define "encodeTermDefinitionPair" $
  doc "Wrap encodeTermDefinition in a Coq Sentence with no leading comment" $
  lambdas ["cc", "ed"] $
    record C._Sentence [
      C._Sentence_comment>>: (nothing :: TTerm (Maybe C.Comment)),
      C._Sentence_content>>: encodeTermDefinition @@ var "cc" @@ Pairs.first (var "ed") @@ Pairs.second (var "ed")]

-- | Wrap an encoded type body as a Coq Definition sentence content (typed `: Type`).
encodeTypeDefinition :: TTermDefinition (String -> Type -> C.SentenceContent)
encodeTypeDefinition = define "encodeTypeDefinition" $
  doc "Build a Coq `Definition name : Type := body.` sentence from a Hydra type" $
  lambdas ["name", "ty"] $
    inject C._SentenceContent C._SentenceContent_definition $
      record C._Definition [
        C._Definition_locality>>: (nothing :: TTerm (Maybe C.Locality)),
        C._Definition_name>>: coqIdent @@ var "name",
        C._Definition_binders>>: list ([] :: [TTerm C.Binder]),
        C._Definition_type>>: just (coqTypeTerm @@ (coqTermQualid @@ string "Type")),
        C._Definition_body>>: encodeType @@ var "ty"]

-- | Encode a (name, Type) pair as a Coq Sentence.
encodeTypeDefinitionPair :: TTermDefinition ((String, Type) -> C.Sentence)
encodeTypeDefinitionPair = define "encodeTypeDefinitionPair" $
  doc "Wrap encodeTypeDefinition in a Coq Sentence with no leading comment" $
  lambda "td" $
    record C._Sentence [
      C._Sentence_comment>>: (nothing :: TTerm (Maybe C.Comment)),
      C._Sentence_content>>: encodeTypeDefinition @@ Pairs.first (var "td") @@ Pairs.second (var "td")]

-- | Build a Coq Constructor from a Hydra union FieldType.
encodeUnionConstructor :: TTermDefinition (String -> FieldType -> C.Constructor)
encodeUnionConstructor = define "encodeUnionConstructor" $
  doc "Construct a Coq Inductive Constructor line `Name_Tag : body -> Name` for a union variant" $
  lambdas ["typeName", "f"] $ lets [
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
          @@ (encodeType @@ var "uft")
          @@ (coqTermQualid @@ var "typeName"))]

-- | Translate a union eliminator (Hydra `TermCases`) into a Coq `fun x_ => match x_ with ... end`.
encodeUnionElim :: TTermDefinition (M.Map String Int -> CaseStatement -> C.Term)
encodeUnionElim = define "encodeUnionElim" $
  doc "Build a Coq match expression from a Hydra union eliminator. Uses the constructor-count map to decide whether the match is exhaustive: if so, an explicit default is suppressed; if not and the kernel didn't provide one, inserts `| _ => hydra_unreachable`." $
  lambdas ["cc", "cs"] $ lets [
    "csName">: Core.caseStatementTypeName $ var "cs",
    "csCases">: Core.caseStatementCases $ var "cs",
    "csDefault">: Core.caseStatementDefault $ var "cs",
    "csLocalName">: localTypeName @@ (unwrap _Name @@ var "csName"),
    "expectedCount">: Maps.lookup (var "csLocalName") (var "cc"),
    "caseCount">: Lists.length $ var "csCases",
    "baseEqs">: Lists.map
      (lambda "c" $ lets [
        "cfn">: Core.fieldName $ var "c",
        "cft">: Core.fieldTerm $ var "c",
        "constr">: unionConstructorName
          @@ (unwrap _Name @@ var "csName")
          @@ (unwrap _Name @@ var "cfn")] $
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
            C._Equation_term>>: encodeTerm @@ var "cc" @@ (unitLambdaBody @@ var "cft")])
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
              coqTermApp @@ (encodeTerm @@ var "cc" @@ var "cft") @@ list [coqTermQualid @@ string "v_"]]))
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
        (list [var "wildcardEq" @@ (encodeTerm @@ var "cc" @@ var "defT")]))
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
extractLambdaBinders :: TTermDefinition (Term -> [C.Binder])
extractLambdaBinders = define "extractLambdaBinders" $
  doc "Collect a chain of leading lambdas as Coq binders, converting type annotations as well" $
  lambda "tm" $ cases _Term (var "tm") (Just $ list ([] :: [TTerm C.Binder])) [
    _Term_annotated>>: "at" ~>
      extractLambdaBinders @@ (Core.annotatedTermBody $ var "at"),
    _Term_lambda>>: "lam" ~> lets [
      "param">: Core.lambdaParameter $ var "lam",
      "mDomain">: Core.lambdaDomain $ var "lam",
      "binder">: Maybes.maybe
        (inject C._Binder C._Binder_name (coqName @@ (unwrap _Name @@ var "param")))
        (lambda "domTy" $ inject C._Binder C._Binder_type $
          record C._TypeBinders [
            C._TypeBinders_names>>: list [coqName @@ (unwrap _Name @@ var "param")],
            C._TypeBinders_type>>: coqTypeTerm @@ (encodeType @@ var "domTy")])
        (var "mDomain")] $
      Lists.cons (var "binder") (extractLambdaBinders @@ (Core.lambdaBody $ var "lam"))]

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
    "localPart">: Logic.ifElse (Lists.null $ var "parts")
      (var "s")
      (Lists.last $ var "parts")] $
    sanitizeVar @@ var "localPart"

-- | Combine a universe of type and term definitions into a single Coq Document.
-- The constructor-count map is threaded through the term encoder so that match
-- expressions can decide whether to emit a default arm.
moduleToCoq :: TTermDefinition (M.Map String Int -> [(String, Type)] -> [(String, Term)] -> C.Document)
moduleToCoq = define "moduleToCoq" $
  doc "Build a Coq Document from lists of type definitions and term definitions" $
  lambdas ["cc", "typeDefs", "termDefs"] $ lets [
    "typesSentences">: Lists.map (lambda "td" $ encodeTypeDefinitionPair @@ var "td") (var "typeDefs"),
    "termsSentences">: Lists.map (lambda "ed" $ encodeTermDefinitionPair @@ var "cc" @@ var "ed") (var "termDefs")] $
    record C._Document [
      C._Document_sentences>>: Lists.concat $ list [
        list [standardImports],
        var "typesSentences",
        var "termsSentences"]]

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
    "localPart">: Lists.last $ var "parts",
    "prefixParts">: Lists.init $ var "parts",
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
