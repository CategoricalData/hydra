{-# LANGUAGE FlexibleContexts #-}

-- | Coq serializer: converts the Coq syntax AST to concrete Coq source code (.v files).

module Hydra.Sources.Coq.Serde where

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
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Other.Coq               as CoqSyntax
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

ns :: Namespace
ns = Namespace "hydra.coq.serde"

module_ :: Module
module_ = Module ns definitions
    [Constants.ns, Formatting.ns, Serialization.ns]
    (CoqSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Coq serializer: converts Coq AST to concrete Coq source code"
  where
    definitions = [
      toDefinition applicationToExpr,
      toDefinition axiomDeclarationToExpr,
      toDefinition binderToExpr,
      toDefinition commentToExpr,
      toDefinition constructorToExpr,
      toDefinition definitionToExpr,
      toDefinition documentToExpr,
      toDefinition fixpointDefinitionToExpr,
      toDefinition identToExpr,
      toDefinition inductiveBodyToExpr,
      toDefinition inductiveDefinitionToExpr,
      toDefinition localityToExpr,
      toDefinition matchToExpr,
      toDefinition moduleDefinitionToExpr,
      toDefinition pattern0ToExpr,
      toDefinition pattern10ToExpr,
      toDefinition pattern1ToExpr,
      toDefinition patternToExpr,
      toDefinition qualidToExpr,
      toDefinition recordDefinitionToExpr,
      toDefinition recordFieldToExpr,
      toDefinition requireImportToExpr,
      toDefinition sectionDefinitionToExpr,
      toDefinition sentenceContentToExpr,
      toDefinition sentenceToExpr,
      toDefinition sortToExpr,
      toDefinition termToExpr,
      toDefinition term0ToExpr,
      toDefinition term1ToExpr,
      toDefinition term10ToExpr,
      toDefinition term100ToExpr,
      toDefinition theoremBodyToExpr,
      toDefinition typeToExpr]

-- ===========================================================================
-- Helpers
-- ===========================================================================

-- | Render a Coq keyword
kw :: String -> TTerm Expr
kw s = Serialization.cst @@ string s

-- | Render space-separated expressions
sp :: [TTerm Expr] -> TTerm Expr
sp xs = Serialization.spaceSep @@ list xs

-- | Render with a period terminator
withDot :: TTerm Expr -> TTerm Expr
withDot e = Serialization.suffix @@ string "." @@ e

-- | Render an optional part, yielding empty list or singleton
optPart :: TTerm (Maybe a) -> (TTerm a -> TTerm Expr) -> TTerm [Expr]
optPart opt f = Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "x" $ list [f (var "x")]) opt

-- ===========================================================================
-- Term language serialization
-- ===========================================================================

-- | Serialize an Ident
identToExpr :: TTermDefinition (C.Ident -> Expr)
identToExpr = define "identToExpr" $
  lambda "ident" $
    Serialization.cst @@ (unwrap C._String @@ (unwrap C._Ident @@ var "ident"))

-- | Serialize a Qualid: ident.field1.field2
qualidToExpr :: TTermDefinition (C.Qualid -> Expr)
qualidToExpr = define "qualidToExpr" $
  lambda "q" $ lets [
    "idExpr">: identToExpr @@ (project C._Qualid C._Qualid_id @@ var "q"),
    "fieldIds">: project C._Qualid C._Qualid_fieldIds @@ var "q",
    "fieldExprs">: Lists.map
      (lambda "f" $ identToExpr @@ (unwrap C._FieldIdent @@ var "f"))
      (var "fieldIds")] $
    Logic.ifElse (Lists.null $ var "fieldExprs")
      (var "idExpr")
      (Serialization.dotSep @@ Lists.concat2 (list [var "idExpr"]) (var "fieldExprs"))

-- | Serialize a Sort
sortToExpr :: TTermDefinition (C.Sort -> Expr)
sortToExpr = define "sortToExpr" $
  lambda "s" $ cases C._Sort (var "s") Nothing [
    C._Sort_set>>: constant $ kw "Set",
    C._Sort_prop>>: constant $ kw "Prop",
    C._Sort_sProp>>: constant $ kw "SProp",
    C._Sort_type>>: constant $ kw "Type",
    C._Sort_typeWithAnyUniverse>>: constant $ kw "Type",
    C._Sort_typeWithUniverse>>: lambda "u" $
      Serialization.noSep @@ list [kw "Type", kw "@{", kw "}" ]]

-- | Serialize a Type (which is just a wrapper around Term)
typeToExpr :: TTermDefinition (C.Type -> Expr)
typeToExpr = define "typeToExpr" $
  lambda "t" $ termToExpr @@ (unwrap C._Type @@ var "t")

-- | Serialize a Binder
binderToExpr :: TTermDefinition (C.Binder -> Expr)
binderToExpr = define "binderToExpr" $
  lambda "b" $ cases C._Binder (var "b") Nothing [
    C._Binder_name>>: lambda "n" $
      Maybes.maybe (kw "_")
        (lambda "i" $ identToExpr @@ var "i")
        (unwrap C._Name @@ var "n"),
    C._Binder_type>>: lambda "tb" $ lets [
      "names">: Lists.map
        (lambda "n" $ Maybes.maybe (kw "_")
          (lambda "i" $ identToExpr @@ var "i")
          (unwrap C._Name @@ var "n"))
        (project C._TypeBinders C._TypeBinders_names @@ var "tb"),
      "ty">: typeToExpr @@ (project C._TypeBinders C._TypeBinders_type @@ var "tb")] $
      Serialization.parens @@ (sp [
        Serialization.spaceSep @@ var "names",
        kw ":",
        var "ty"]),
    C._Binder_term>>: lambda "lb" $ lets [
      "name">: Maybes.maybe (kw "_")
        (lambda "i" $ identToExpr @@ var "i")
        (unwrap C._Name @@ (project C._LetBinder C._LetBinder_name @@ var "lb")),
      "ty">: Maybes.maybe (list ([] :: [TTerm Expr]))
        (lambda "t" $ list [kw ":", typeToExpr @@ var "t"])
        (project C._LetBinder C._LetBinder_type @@ var "lb"),
      "body">: termToExpr @@ (project C._LetBinder C._LetBinder_term @@ var "lb")] $
      Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat (list [
        list [var "name"],
        var "ty",
        list [kw ":=", var "body"]])),
    C._Binder_implicit>>: lambda "ib" $
      cases C._ImplicitBinders (var "ib") Nothing [
        C._ImplicitBinders_maximallyInserted>>: lambda "tb" $ lets [
          "names">: Lists.map
            (lambda "n" $ Maybes.maybe (kw "_")
              (lambda "i" $ identToExpr @@ var "i")
              (unwrap C._Name @@ var "n"))
            (project C._TypeBinders C._TypeBinders_names @@ var "tb"),
          "ty">: typeToExpr @@ (project C._TypeBinders C._TypeBinders_type @@ var "tb")] $
          Serialization.brackets @@ (asTerm Serialization.curlyBraces) @@ (asTerm Serialization.inlineStyle) @@
            (sp [Serialization.spaceSep @@ var "names", kw ":", var "ty"]),
        C._ImplicitBinders_nonMaximallyInserted>>: lambda "tb" $ lets [
          "names">: Lists.map
            (lambda "n" $ Maybes.maybe (kw "_")
              (lambda "i" $ identToExpr @@ var "i")
              (unwrap C._Name @@ var "n"))
            (project C._TypeBinders C._TypeBinders_names @@ var "tb"),
          "ty">: typeToExpr @@ (project C._TypeBinders C._TypeBinders_type @@ var "tb")] $
          Serialization.brackets @@ (asTerm Serialization.squareBrackets) @@ (asTerm Serialization.inlineStyle) @@
            (sp [Serialization.spaceSep @@ var "names", kw ":", var "ty"])],
    C._Binder_generalizing>>: lambda "gb" $
      cases C._GeneralizingBinder (var "gb") Nothing [
        C._GeneralizingBinder_explicit>>: lambda "tc" $
          Serialization.parens @@ (termToExpr @@ (project C._TypeclassConstraint C._TypeclassConstraint_term @@ var "tc")),
        C._GeneralizingBinder_implicitMaximallyInserted>>: lambda "tc" $
          Serialization.brackets @@ (asTerm Serialization.curlyBraces) @@ (asTerm Serialization.inlineStyle) @@
            (termToExpr @@ (project C._TypeclassConstraint C._TypeclassConstraint_term @@ var "tc")),
        C._GeneralizingBinder_implicitNonMaximallyInserted>>: lambda "tc" $
          Serialization.brackets @@ (asTerm Serialization.squareBrackets) @@ (asTerm Serialization.inlineStyle) @@
            (termToExpr @@ (project C._TypeclassConstraint C._TypeclassConstraint_term @@ var "tc"))],
    C._Binder_pattern>>: lambda "p" $ kw "_"]

-- | Serialize a Term
termToExpr :: TTermDefinition (C.Term -> Expr)
termToExpr = define "termToExpr" $
  lambda "t" $ cases C._Term (var "t") Nothing [
    C._Term_forallOrFun>>: lambda "fof" $
      cases C._ForallOrFun (var "fof") Nothing [
        C._ForallOrFun_forall>>: lambda "fa" $ sp [
          kw "forall",
          openBindersToExpr (project C._Forall C._Forall_binders @@ var "fa"),
          kw ",",
          typeToExpr @@ (project C._Forall C._Forall_type @@ var "fa")],
        C._ForallOrFun_fun>>: lambda "fn" $ sp [
          kw "fun",
          openBindersToExpr (project C._Fun C._Fun_binders @@ var "fn"),
          kw "=>",
          termToExpr @@ (project C._Fun C._Fun_body @@ var "fn")]],
    C._Term_let>>: lambda "lt" $ lets [
      "bindings">: project C._Let C._Let_bindings @@ var "lt",
      "body">: termToExpr @@ (project C._Let C._Let_in @@ var "lt")] $
      cases C._LetBindings (var "bindings") Nothing [
        C._LetBindings_named>>: lambda "ln" $ lets [
          "binder">: project C._LetNamed C._LetNamed_binder @@ var "ln",
          "name">: Maybes.maybe (kw "_")
            (lambda "i" $ identToExpr @@ var "i")
            (unwrap C._Name @@ (project C._LetBinder C._LetBinder_name @@ var "binder")),
          "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
            (project C._LetNamed C._LetNamed_binders @@ var "ln"),
          "ty">: Maybes.maybe (list ([] :: [TTerm Expr]))
            (lambda "t2" $ list [kw ":", typeToExpr @@ var "t2"])
            (project C._LetBinder C._LetBinder_type @@ var "binder"),
          "val">: termToExpr @@ (project C._LetBinder C._LetBinder_term @@ var "binder")] $
          sp [kw "let", Serialization.spaceSep @@ Lists.concat (list [
            list [var "name"],
            var "binders",
            var "ty",
            list [kw ":=", var "val"],
            list [kw "in"]]),
            var "body"],
        C._LetBindings_destructuring>>: lambda "ld" $ sp [kw "let", kw "...", kw "in", var "body"]],
    C._Term_if>>: lambda "ifE" $ lets [
      "cond">: termToExpr @@ (project C._If C._If_condition @@ var "ifE"),
      "thn">: termToExpr @@ (project C._If C._If_then @@ var "ifE"),
      "els">: termToExpr @@ (project C._If C._If_else @@ var "ifE")] $
      sp [kw "if", var "cond", kw "then", var "thn", kw "else", var "els"],
    C._Term_fix>>: lambda "fx" $
      cases C._Fix (var "fx") Nothing [
        C._Fix_decl>>: lambda "d" $ sp [kw "fix", fixDeclToExpr (var "d")],
        C._Fix_qual>>: lambda "q" $ kw "fix"],
    C._Term_cofix>>: lambda "cf" $ sp [kw "cofix", kw "..."],
    C._Term_term100>>: lambda "t100" $ term100ToExpr @@ var "t100"]

-- | Serialize OpenBinders
openBindersToExpr :: TTerm C.OpenBinders -> TTerm Expr
openBindersToExpr ob =
  cases C._OpenBinders ob Nothing [
    C._OpenBinders_type>>: lambda "tb" $ lets [
      "names">: Lists.map
        (lambda "n" $ Maybes.maybe (kw "_")
          (lambda "i" $ identToExpr @@ var "i")
          (unwrap C._Name @@ var "n"))
        (project C._TypeBinders C._TypeBinders_names @@ var "tb"),
      "ty">: typeToExpr @@ (project C._TypeBinders C._TypeBinders_type @@ var "tb")] $
      sp [Serialization.parens @@ (sp [Serialization.spaceSep @@ var "names", kw ":", var "ty"])],
    C._OpenBinders_binders>>: lambda "bs" $
      Serialization.spaceSep @@ Lists.map (lambda "b" $ binderToExpr @@ var "b") (var "bs")]

-- | Serialize a Fix_Decl
fixDeclToExpr :: TTerm C.Fix_Decl -> TTerm Expr
fixDeclToExpr d = lets [
  "name">: identToExpr @@ (project C._Fix_Decl C._Fix_Decl_ident @@ d),
  "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
    (project C._Fix_Decl C._Fix_Decl_binders @@ d),
  "ty">: Maybes.maybe (list ([] :: [TTerm Expr]))
    (lambda "t" $ list [kw ":", typeToExpr @@ var "t"])
    (project C._Fix_Decl C._Fix_Decl_type @@ d),
  "body">: termToExpr @@ (project C._Fix_Decl C._Fix_Decl_term @@ d)] $
  Serialization.spaceSep @@ Lists.concat (list [
    list [var "name"],
    var "binders",
    var "ty",
    list [kw ":=", var "body"]])

-- | Serialize a Term100
term100ToExpr :: TTermDefinition (C.Term100 -> Expr)
term100ToExpr = define "term100ToExpr" $
  lambda "t" $ cases C._Term100 (var "t") Nothing [
    C._Term100_cast>>: lambda "tc" $ sp [
      term10ToExpr @@ (project C._TypeCast C._TypeCast_term @@ var "tc"),
      kw ":",
      typeToExpr @@ (project C._TypeCast C._TypeCast_type @@ var "tc")],
    C._Term100_term10>>: lambda "t10" $ term10ToExpr @@ var "t10"]

-- | Serialize a Term10
term10ToExpr :: TTermDefinition (C.Term10 -> Expr)
term10ToExpr = define "term10ToExpr" $
  lambda "t" $ cases C._Term10 (var "t") Nothing [
    C._Term10_application>>: lambda "app" $ applicationToExpr @@ var "app",
    C._Term10_oneTerm>>: lambda "ot" $
      cases C._OneTerm (var "ot") Nothing [
        C._OneTerm_explicit>>: lambda "qa" $ qualidAnnotatedToExpr (var "qa"),
        C._OneTerm_term1>>: lambda "t1" $ term1ToExpr @@ var "t1"]]

-- | Serialize a Term1
term1ToExpr :: TTermDefinition (C.Term1 -> Expr)
term1ToExpr = define "term1ToExpr" $
  lambda "t" $ cases C._Term1 (var "t") Nothing [
    C._Term1_projection>>: constant $ kw "?projection",
    C._Term1_scope>>: constant $ kw "?scope",
    C._Term1_term0>>: lambda "t0" $ term0ToExpr @@ var "t0"]

-- | Serialize a Term0
term0ToExpr :: TTermDefinition (C.Term0 -> Expr)
term0ToExpr = define "term0ToExpr" $
  lambda "t" $ cases C._Term0 (var "t") Nothing [
    C._Term0_qualidAnnotated>>: lambda "qa" $ qualidAnnotatedToExpr (var "qa"),
    C._Term0_sort>>: lambda "s" $ sortToExpr @@ var "s",
    C._Term0_primitiveNotations>>: lambda "pn" $
      cases C._PrimitiveNotations (var "pn") Nothing [
        C._PrimitiveNotations_number>>: lambda "n" $ lets [
          "v">: unwrap C._Number @@ var "n"] $
          Serialization.cst @@ (Literals.showBigfloat (var "v")),
        C._PrimitiveNotations_string>>: lambda "s2" $
          sp [kw "\"", Serialization.cst @@ (unwrap C._String @@ var "s2"), kw "\""]],
    C._Term0_evar>>: lambda "ev" $ kw "?evar",
    C._Term0_match>>: lambda "m" $ matchToExpr @@ var "m",
    C._Term0_record>>: constant $ kw "{| |}",
    C._Term0_generalizing>>: constant $ kw "`( )",
    C._Term0_ltac>>: constant $ kw "ltac:( )",
    C._Term0_parens>>: lambda "inner" $
      Serialization.parens @@ (termToExpr @@ var "inner")]

-- | Serialize a QualidAnnotated
qualidAnnotatedToExpr :: TTerm C.QualidAnnotated -> TTerm Expr
qualidAnnotatedToExpr qa = qualidToExpr @@ (project C._QualidAnnotated C._QualidAnnotated_qualid @@ qa)

-- | Serialize an Application
applicationToExpr :: TTermDefinition (C.Application -> Expr)
applicationToExpr = define "applicationToExpr" $
  lambda "app" $ cases C._Application (var "app") Nothing [
    C._Application_normal>>: lambda "na" $ sp [
      term1ToExpr @@ (project C._NormalApplication C._NormalApplication_lhs @@ var "na"),
      Serialization.spaceSep @@ Lists.map
        (lambda "a" $ cases C._Arg (var "a") Nothing [
          C._Arg_ident>>: lambda "ia" $ sp [
            Serialization.parens @@ (sp [
              identToExpr @@ (project C._IdentArg C._IdentArg_ident @@ var "ia"),
              kw ":=",
              termToExpr @@ (project C._IdentArg C._IdentArg_term @@ var "ia")])],
          C._Arg_natural>>: lambda "na2" $ lets [
            "v">: unwrap C._Natural @@ (project C._NaturalArg C._NaturalArg_natural @@ var "na2")] $
            Serialization.cst @@ (Literals.showBigint (var "v")),
          C._Arg_term>>: lambda "t1" $ term1ToExpr @@ var "t1"])
        (project C._NormalApplication C._NormalApplication_rhs @@ var "na")],
    C._Application_annotated>>: lambda "aa" $ sp [
      kw "@",
      qualidAnnotatedToExpr (project C._AnnotatedApplication C._AnnotatedApplication_annot @@ var "aa")]]

-- | Serialize an AxiomDeclaration: `Axiom name : type.`
axiomDeclarationToExpr :: TTermDefinition (C.AxiomDeclaration -> Expr)
axiomDeclarationToExpr = define "axiomDeclarationToExpr" $
  lambda "a" $
    withDot $ sp [
      kw "Axiom",
      identToExpr @@ (project C._AxiomDeclaration C._AxiomDeclaration_name @@ var "a"),
      kw ":",
      typeToExpr @@ (project C._AxiomDeclaration C._AxiomDeclaration_type @@ var "a")]

-- | Serialize a Pattern0 (primitive / grouped pattern)
pattern0ToExpr :: TTermDefinition (C.Pattern0 -> Expr)
pattern0ToExpr = define "pattern0ToExpr" $
  lambda "p" $ cases C._Pattern0 (var "p") Nothing [
    C._Pattern0_qualid>>: lambda "q" $ qualidToExpr @@ var "q",
    C._Pattern0_qualIdAndPattern>>: constant $ kw "...",
    C._Pattern0_placeholder>>: constant $ kw "_",
    C._Pattern0_parens>>: lambda "ps" $ Serialization.parens @@
      (Serialization.infixWsList @@ string ", " @@
        Lists.map (lambda "p2" $ patternToExpr @@ var "p2") (var "ps")),
    C._Pattern0_number>>: lambda "n" $ lets [
      "v">: unwrap C._Number @@ var "n"] $
      Serialization.cst @@ (Literals.showBigfloat (var "v")),
    C._Pattern0_string>>: lambda "s" $ sp [
      kw "\"", Serialization.cst @@ (unwrap C._String @@ var "s"), kw "\""]]

-- | Serialize a Pattern1 (scoped pattern; we ignore the scope for now)
pattern1ToExpr :: TTermDefinition (C.Pattern1 -> Expr)
pattern1ToExpr = define "pattern1ToExpr" $
  lambda "p" $ pattern0ToExpr @@ (project C._Pattern1 C._Pattern1_pattern @@ var "p")

-- | Serialize a Pattern10 (as-binding, juxtaposition, or qualid-with-args)
pattern10ToExpr :: TTermDefinition (C.Pattern10 -> Expr)
pattern10ToExpr = define "pattern10ToExpr" $
  lambda "p" $ cases C._Pattern10 (var "p") Nothing [
    C._Pattern10_as>>: lambda "pa" $ sp [
      pattern1ToExpr @@ (project C._Pattern10_As C._Pattern10_As_pattern @@ var "pa"),
      kw "as",
      Maybes.maybe (kw "_")
        (lambda "i" $ identToExpr @@ var "i")
        (unwrap C._Name @@ (project C._Pattern10_As C._Pattern10_As_as @@ var "pa"))],
    C._Pattern10_patterns>>: lambda "pps" $ lets [
      "first">: pattern1ToExpr @@ (project C._Pattern10_Patterns C._Pattern10_Patterns_pattern @@ var "pps"),
      "rest">: Lists.map (lambda "p2" $ pattern1ToExpr @@ var "p2")
        (project C._Pattern10_Patterns C._Pattern10_Patterns_patterns @@ var "pps")] $
      Serialization.spaceSep @@ Lists.cons (var "first") (var "rest"),
    C._Pattern10_qualiid>>: lambda "pq" $ lets [
      "q">: qualidToExpr @@ (project C._Pattern10_Qualid C._Pattern10_Qualid_qualid @@ var "pq"),
      "args">: Lists.map (lambda "p2" $ pattern1ToExpr @@ var "p2")
        (project C._Pattern10_Qualid C._Pattern10_Qualid_patterns @@ var "pq")] $
      Logic.ifElse (Lists.null (var "args"))
        (var "q")
        (Serialization.spaceSep @@ Lists.cons (var "q") (var "args"))]

-- | Serialize a Pattern (top-level; we delegate to Pattern10 and ignore the annotation term)
patternToExpr :: TTermDefinition (C.Pattern -> Expr)
patternToExpr = define "patternToExpr" $
  lambda "p" $ cases C._Pattern (var "p") Nothing [
    C._Pattern_pattern>>: lambda "p10" $ pattern10ToExpr @@ var "p10",
    C._Pattern_term>>: constant $ kw "_"]

-- | Serialize a Match expression
matchToExpr :: TTermDefinition (C.Match -> Expr)
matchToExpr = define "matchToExpr" $
  lambda "m" $ lets [
    "items">: Lists.map
      (lambda "ci" $ lets [
        "t">: term100ToExpr @@ (project C._CaseItem C._CaseItem_term @@ var "ci"),
        "asP">: Maybes.maybe (list ([] :: [TTerm Expr]))
          (lambda "n" $ list [kw "as", Maybes.maybe (kw "_") (lambda "i" $ identToExpr @@ var "i")
            (unwrap C._Name @@ var "n")])
          (project C._CaseItem C._CaseItem_as @@ var "ci")] $
        Serialization.spaceSep @@ Lists.concat (list [list [var "t"], var "asP"]))
      (project C._Match C._Match_caseItems @@ var "m"),
    "ret">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "r" $ list [kw "return", term100ToExpr @@ var "r"])
      (project C._Match C._Match_return @@ var "m"),
    "eqs">: Lists.map
      (lambda "eq" $ lets [
        "patGroups">: Lists.map
          (lambda "grp" $ Serialization.spaceSep @@
            Lists.map (lambda "p" $ patternToExpr @@ var "p") (var "grp"))
          (project C._Equation C._Equation_pattern @@ var "eq"),
        "pats">: Serialization.infixWsList @@ string " | " @@ (var "patGroups"),
        "body">: termToExpr @@ (project C._Equation C._Equation_term @@ var "eq")] $
        sp [kw "|", var "pats", kw "=>", var "body"])
      (project C._Match C._Match_equations @@ var "m")] $
    Serialization.newlineSep @@ Lists.concat (list [
      list [Serialization.spaceSep @@ Lists.concat (list [
        list [kw "match"],
        var "items",
        var "ret",
        list [kw "with"]])],
      var "eqs",
      list [kw "end"]])

-- ===========================================================================
-- Vernacular serialization
-- ===========================================================================

-- | Serialize a Comment
commentToExpr :: TTermDefinition (C.Comment -> Expr)
commentToExpr = define "commentToExpr" $
  lambda "c" $
    Serialization.cst @@ Strings.cat (list [
      string "(* ", unwrap C._Comment @@ var "c", string " *)"])

-- | Serialize a Locality qualifier
localityToExpr :: TTermDefinition (C.Locality -> Expr)
localityToExpr = define "localityToExpr" $
  lambda "loc" $ cases C._Locality (var "loc") Nothing [
    C._Locality_local>>: constant $ kw "Local",
    C._Locality_global>>: constant $ kw "Global"]

-- | Serialize a RequireImport
requireImportToExpr :: TTermDefinition (C.RequireImport -> Expr)
requireImportToExpr = define "requireImportToExpr" $
  lambda "ri" $ lets [
    "fromPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "q" $ list [kw "From", qualidToExpr @@ var "q"])
      (project C._RequireImport C._RequireImport_from @@ var "ri"),
    "requirePart">: Logic.ifElse (project C._RequireImport C._RequireImport_require @@ var "ri")
      (list [kw "Require"])
      (list ([] :: [TTerm Expr])),
    "qualPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "q" $ cases C._ImportQualification (var "q") Nothing [
        C._ImportQualification_import>>: constant $ list [kw "Import"],
        C._ImportQualification_export>>: constant $ list [kw "Export"]])
      (project C._RequireImport C._RequireImport_qualification @@ var "ri"),
    "mods">: Lists.map (lambda "m" $ qualidToExpr @@ var "m")
      (project C._RequireImport C._RequireImport_modules @@ var "ri")] $
    withDot $ Serialization.spaceSep @@ Lists.concat (list [
      var "fromPart",
      var "requirePart",
      var "qualPart",
      var "mods"])

-- | Serialize a Constructor
constructorToExpr :: TTermDefinition (C.Constructor -> Expr)
constructorToExpr = define "constructorToExpr" $
  lambda "c" $ lets [
    "name">: identToExpr @@ (project C._Constructor C._Constructor_name @@ var "c"),
    "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
      (project C._Constructor C._Constructor_binders @@ var "c"),
    "ty">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "t" $ list [kw ":", typeToExpr @@ var "t"])
      (project C._Constructor C._Constructor_type @@ var "c")] $
    Serialization.spaceSep @@ Lists.concat (list [
      list [kw "|", var "name"],
      var "binders",
      var "ty"])

-- | Serialize an InductiveBody
inductiveBodyToExpr :: TTermDefinition (C.InductiveBody -> Expr)
inductiveBodyToExpr = define "inductiveBodyToExpr" $
  lambda "ib" $ lets [
    "name">: identToExpr @@ (project C._InductiveBody C._InductiveBody_name @@ var "ib"),
    "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
      (project C._InductiveBody C._InductiveBody_binders @@ var "ib"),
    "ty">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "t" $ list [kw ":", typeToExpr @@ var "t"])
      (project C._InductiveBody C._InductiveBody_type @@ var "ib"),
    "constrs">: Lists.map (lambda "c" $ constructorToExpr @@ var "c")
      (project C._InductiveBody C._InductiveBody_constructors @@ var "ib")] $
    Serialization.newlineSep @@ Lists.concat (list [
      list [Serialization.spaceSep @@ Lists.concat (list [
        list [var "name"],
        var "binders",
        var "ty",
        list [kw ":="]])],
      var "constrs"])

-- | Serialize an InductiveDefinition.
-- Produces "[Locality] Inductive body1 with body2 with body3." for mutual inductives.
inductiveDefinitionToExpr :: TTermDefinition (C.InductiveDefinition -> Expr)
inductiveDefinitionToExpr = define "inductiveDefinitionToExpr" $
  lambda "id" $ lets [
    "locPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "l" $ list [localityToExpr @@ var "l"])
      (project C._InductiveDefinition C._InductiveDefinition_locality @@ var "id"),
    "kwPart">: Logic.ifElse (project C._InductiveDefinition C._InductiveDefinition_coinductive @@ var "id")
      (kw "CoInductive")
      (kw "Inductive"),
    "bodyExprs">: Lists.map (lambda "b" $ inductiveBodyToExpr @@ var "b")
      (project C._InductiveDefinition C._InductiveDefinition_bodies @@ var "id"),
    "firstBody">: Lists.head $ var "bodyExprs",
    "restBodies">: Lists.map
      (lambda "b" $ Serialization.spaceSep @@ list [kw "with", var "b"])
      (Lists.tail $ var "bodyExprs"),
    "firstLine">: Serialization.spaceSep @@ Lists.concat (list [
      var "locPart",
      list [var "kwPart", var "firstBody"]])] $
    withDot $ Serialization.newlineSep @@ Lists.cons (var "firstLine") (var "restBodies")

-- | Serialize a RecordField
recordFieldToExpr :: TTermDefinition (C.RecordField -> Expr)
recordFieldToExpr = define "recordFieldToExpr" $
  lambda "rf" $ sp [
    identToExpr @@ (project C._RecordField C._RecordField_name @@ var "rf"),
    kw ":",
    typeToExpr @@ (project C._RecordField C._RecordField_type @@ var "rf")]

-- | Serialize a RecordDefinition
recordDefinitionToExpr :: TTermDefinition (C.RecordDefinition -> Expr)
recordDefinitionToExpr = define "recordDefinitionToExpr" $
  lambda "rd" $ lets [
    "locPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "l" $ list [localityToExpr @@ var "l"])
      (project C._RecordDefinition C._RecordDefinition_locality @@ var "rd"),
    "name">: identToExpr @@ (project C._RecordDefinition C._RecordDefinition_name @@ var "rd"),
    "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
      (project C._RecordDefinition C._RecordDefinition_binders @@ var "rd"),
    "sortPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "s" $ list [kw ":", sortToExpr @@ var "s"])
      (project C._RecordDefinition C._RecordDefinition_sort @@ var "rd"),
    "body">: project C._RecordDefinition C._RecordDefinition_body @@ var "rd",
    "constrPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "c" $ list [identToExpr @@ var "c"])
      (project C._RecordBody C._RecordBody_constructor @@ var "body"),
    "fields">: Lists.map (lambda "f" $ Serialization.suffix @@ string " ;" @@ (recordFieldToExpr @@ var "f"))
      (project C._RecordBody C._RecordBody_fields @@ var "body")] $
    withDot $ Serialization.newlineSep @@ Lists.concat (list [
      list [Serialization.spaceSep @@ Lists.concat (list [
        var "locPart",
        list [kw "Record"],
        list [var "name"],
        var "binders",
        var "sortPart",
        list [kw ":="],
        var "constrPart",
        list [kw "{"]])],
      var "fields",
      list [kw "}"]])

-- | Serialize a Definition
definitionToExpr :: TTermDefinition (C.Definition -> Expr)
definitionToExpr = define "definitionToExpr" $
  lambda "d" $ lets [
    "locPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "l" $ list [localityToExpr @@ var "l"])
      (project C._Definition C._Definition_locality @@ var "d"),
    "name">: identToExpr @@ (project C._Definition C._Definition_name @@ var "d"),
    "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
      (project C._Definition C._Definition_binders @@ var "d"),
    "ty">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "t" $ list [kw ":", typeToExpr @@ var "t"])
      (project C._Definition C._Definition_type @@ var "d"),
    "body">: termToExpr @@ (project C._Definition C._Definition_body @@ var "d")] $
    withDot $ Serialization.spaceSep @@ Lists.concat (list [
      var "locPart",
      list [kw "Definition", var "name"],
      var "binders",
      var "ty",
      list [kw ":=", var "body"]])

-- | Serialize a FixpointDefinition
fixpointDefinitionToExpr :: TTermDefinition (C.FixpointDefinition -> Expr)
fixpointDefinitionToExpr = define "fixpointDefinitionToExpr" $
  lambda "fd" $ lets [
    "locPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "l" $ list [localityToExpr @@ var "l"])
      (project C._FixpointDefinition C._FixpointDefinition_locality @@ var "fd"),
    "name">: identToExpr @@ (project C._FixpointDefinition C._FixpointDefinition_name @@ var "fd"),
    "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
      (project C._FixpointDefinition C._FixpointDefinition_binders @@ var "fd"),
    "ty">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "t" $ list [kw ":", typeToExpr @@ var "t"])
      (project C._FixpointDefinition C._FixpointDefinition_type @@ var "fd"),
    "body">: termToExpr @@ (project C._FixpointDefinition C._FixpointDefinition_body @@ var "fd")] $
    withDot $ Serialization.spaceSep @@ Lists.concat (list [
      var "locPart",
      list [kw "Fixpoint", var "name"],
      var "binders",
      var "ty",
      list [kw ":=", var "body"]])

-- | Serialize a TheoremBody
theoremBodyToExpr :: TTermDefinition (C.TheoremBody -> Expr)
theoremBodyToExpr = define "theoremBodyToExpr" $
  lambda "tb" $ lets [
    "kindKw">: cases C._TheoremKind (project C._TheoremBody C._TheoremBody_kind @@ var "tb") Nothing [
      C._TheoremKind_theorem>>: constant $ kw "Theorem",
      C._TheoremKind_lemma>>: constant $ kw "Lemma",
      C._TheoremKind_proposition>>: constant $ kw "Proposition",
      C._TheoremKind_corollary>>: constant $ kw "Corollary",
      C._TheoremKind_example>>: constant $ kw "Example"],
    "name">: identToExpr @@ (project C._TheoremBody C._TheoremBody_name @@ var "tb"),
    "binders">: Lists.map (lambda "b" $ binderToExpr @@ var "b")
      (project C._TheoremBody C._TheoremBody_binders @@ var "tb"),
    "ty">: typeToExpr @@ (project C._TheoremBody C._TheoremBody_type @@ var "tb"),
    "proof">: termToExpr @@ (project C._TheoremBody C._TheoremBody_proof @@ var "tb")] $
    Serialization.newlineSep @@ list [
      withDot $ Serialization.spaceSep @@ Lists.concat (list [
        list [var "kindKw", var "name"],
        var "binders",
        list [kw ":", var "ty"]]),
      kw "Proof.",
      sp [kw "exact", Serialization.parens @@ var "proof"],
      kw "Qed."]

-- | Serialize a ModuleDefinition
moduleDefinitionToExpr :: TTermDefinition (C.ModuleDefinition -> Expr)
moduleDefinitionToExpr = define "moduleDefinitionToExpr" $
  lambda "md" $ lets [
    "name">: identToExpr @@ (project C._ModuleDefinition C._ModuleDefinition_name @@ var "md"),
    "sentences">: Lists.map (lambda "s" $ sentenceToExpr @@ var "s")
      (project C._ModuleDefinition C._ModuleDefinition_sentences @@ var "md")] $
    Serialization.doubleNewlineSep @@ Lists.concat (list [
      list [withDot $ sp [kw "Module", var "name"]],
      var "sentences",
      list [withDot $ sp [kw "End", var "name"]]])

-- | Serialize a SectionDefinition
sectionDefinitionToExpr :: TTermDefinition (C.SectionDefinition -> Expr)
sectionDefinitionToExpr = define "sectionDefinitionToExpr" $
  lambda "sd" $ lets [
    "name">: identToExpr @@ (project C._SectionDefinition C._SectionDefinition_name @@ var "sd"),
    "sentences">: Lists.map (lambda "s" $ sentenceToExpr @@ var "s")
      (project C._SectionDefinition C._SectionDefinition_sentences @@ var "sd")] $
    Serialization.doubleNewlineSep @@ Lists.concat (list [
      list [withDot $ sp [kw "Section", var "name"]],
      var "sentences",
      list [withDot $ sp [kw "End", var "name"]]])

-- | Serialize a SentenceContent
sentenceContentToExpr :: TTermDefinition (C.SentenceContent -> Expr)
sentenceContentToExpr = define "sentenceContentToExpr" $
  lambda "sc" $ cases C._SentenceContent (var "sc") Nothing [
    C._SentenceContent_axiom>>: lambda "a" $ axiomDeclarationToExpr @@ var "a",
    C._SentenceContent_definition>>: lambda "d" $ definitionToExpr @@ var "d",
    C._SentenceContent_fixpoint>>: lambda "f" $ fixpointDefinitionToExpr @@ var "f",
    C._SentenceContent_inductive>>: lambda "i" $ inductiveDefinitionToExpr @@ var "i",
    C._SentenceContent_module>>: lambda "m" $ moduleDefinitionToExpr @@ var "m",
    C._SentenceContent_notation>>: lambda "n" $ kw "(* notation *)",
    C._SentenceContent_record>>: lambda "r" $ recordDefinitionToExpr @@ var "r",
    C._SentenceContent_requireImport>>: lambda "ri" $ requireImportToExpr @@ var "ri",
    C._SentenceContent_section>>: lambda "s" $ sectionDefinitionToExpr @@ var "s",
    C._SentenceContent_theorem>>: lambda "t" $ theoremBodyToExpr @@ var "t"]

-- | Serialize a Sentence (optional comment + content)
sentenceToExpr :: TTermDefinition (C.Sentence -> Expr)
sentenceToExpr = define "sentenceToExpr" $
  lambda "s" $ lets [
    "cmtPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "c" $ list [commentToExpr @@ var "c"])
      (project C._Sentence C._Sentence_comment @@ var "s"),
    "content">: sentenceContentToExpr @@ (project C._Sentence C._Sentence_content @@ var "s")] $
    Serialization.newlineSep @@ Lists.concat (list [var "cmtPart", list [var "content"]])

-- | Serialize a Document (complete .v file)
documentToExpr :: TTermDefinition (C.Document -> Expr)
documentToExpr = define "documentToExpr" $
  lambda "doc" $
    Serialization.doubleNewlineSep @@ Lists.map
      (lambda "s" $ sentenceToExpr @@ var "s")
      (project C._Document C._Document_sentences @@ var "doc")
