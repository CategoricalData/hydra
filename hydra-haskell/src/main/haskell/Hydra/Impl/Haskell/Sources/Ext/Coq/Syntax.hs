{-|
Model for Coq core and extensions

Based on the Coq 8.15 grammar provided here:
  https://coq.github.io/doc/v8.15/refman/language/core/basic.html#essential-vocabulary
-}

module Hydra.Impl.Haskell.Sources.Ext.Coq.Syntax where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


coqSyntaxModule :: Module Meta
coqSyntaxModule = Module coqSyntax []

coqSyntaxName :: GraphName
coqSyntaxName = GraphName "hydra/ext/coq/syntax"

coqSyntax :: Graph Meta
coqSyntax = Graph coqSyntaxName elements (const True) hydraCoreName
  where
    def = datatype coqSyntaxName
    coq = nominal . qualify coqSyntaxName . Name

    elements = [

      def "Application" $ union [
        field "normal" $ coq "ApplicationNormal",
        field "annotated" $ coq "ApplicationAnnotated"],
      
      def "ApplicationNormal" $ record [
        field "lhs" $ coq "Term1",
        field "rhs" $ nonemptyList $ coq "Arg"],
      
      def "ApplicationAnnotated" $ record [
        field "annot" $ coq "QualidAnnotated",
        field "terms" $ nonemptyList $ coq "Term1"],
      
      def "Arg" $ union [
        field "ident" $ coq "ArgIdent",
        field "natural" $ coq "ArgNatural",
        field "term" $ coq "Term1"],
      
      def "ArgIdent" $ record [
        field "ident" $ coq "Ident",
        field "term" $ coq "Term"],
        
      def "ArgNatural" $ record [
        field "natural" $ coq "Natural",
        field "term" $ coq "Term"],
            
      def "Binder" $ union [
        field "name" $ coq "Name",
        field "type" $ coq "TypeBinders",
        field "term" $ coq "LetBinder",
        field "implicit" $ coq "ImplicitBinders",
        field "generalizing" $ coq "GeneralizingBinder",
        -- 	( name : type | term )  -- TODO
        field "pattern" $ coq "Pattern0"],
  
      def "CaseItem" $ record [
        field "term" $ coq "Term100",
        field "as" $ optional $ coq "Name",
        field "in" $ optional $ coq "Pattern"],  
        
      def "Cofix" $ record [
        field "body" $ coq "CofixBody",
        field "qual" $ optional $ coq "CofixQual"],
        
      def "CofixBody" $ record [
        field "ident" $ coq "Ident",
        field "binders" $ list $ coq "Binder",
        field "type" $ optional $ coq "Type",
        field "term" $ coq "Term"],
      
      def "CofixQual" $ union [
        field "in" $ coq "Term",
        field "with" $ coq "CofixWith"],
        
      def "CofixWith" $ record [
        field "with" $ nonemptyList $ coq "CofixBody",
        field "for" $ optional $ coq "Ident"],

      def "Equation" $ record [
        field "pattern" $ nonemptyList $ nonemptyList $ coq "Pattern",
        field "term" $ coq "Term"],

      def "ExistentialVariable" $ record [
        field "ident" $ coq "Ident",
        field "variant" $ coq "ExistentialVariableVariant"],
        
      def "ExistentialVariableVariant" $ union [
        field "placeholder" unit,
        field "inside1" unit,
        field "inside2" unit,
        field "outside" $ optional $ coq "ArgIdent"],
      
      def "FieldIdent" $ coq "Ident",

      def "Fix" $ union [
        field "decl" $ coq "FixDecl",
        field "qual" $ optional $ coq "FixQual"],
        
      def "FixAnnot" $ union [
        field "struct" $ coq "Ident",
        field "wf" $ coq "FixAnnotWf",
        field "measure" $ coq "FixAnnotMeasure"],
      
      def "FixAnnotMeasure" $ record [
        field "term" $ coq "OneTerm",
        field "ident" $ optional $ coq "Ident",
        field "term2" $ optional $ coq "OneTerm"],
      
      def "FixAnnotWf" $ record [
        field "term" $ coq "OneTerm",
        field "ident" $ coq "Ident"],
      
      def "FixDecl" $ record [
        field "ident" $ coq "Ident",
        field "binders" $ list $ coq "Binder",
        field "annot" $ optional $ coq "FixAnnot",
        field "type" $ optional $ coq "Type",
        field "term" $ coq "Term"],
      
      def "FixQual" $ union [
        field "in" $ coq "Term",
        field "with" $ coq "FixWith"],

      def "FixWith" $ record [
        field "decls" $ nonemptyList $ coq "FixDecl",
        field "for" $ optional $ coq "Ident"],
      
      def "Forall" $ record [
        field "binders" $ coq "OpenBinders",
        field "type" $ coq "Type"],

      def "ForallOrFun" $ union [
        field "forall" $ coq "Forall",
        field "fun" $ coq "Fun"],
        
      def "Fun" $ record [
        field "binders" $ coq "OpenBinders",
        field "body" $ coq "Term"],
      
      def "GeneralizingBinder" $ union [
        field "explicit" $
          doc "Terms surrounded by `( ) introduce their free variables as explicit arguments" $
          coq "TypeclassConstraint",
          
        field "implicitMaximallyInserted" $
          doc "Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments" $
          coq "TypeclassConstraint",
          
        field "implicitNonMaximallyInserted" $
          doc "Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments" $
          coq "TypeclassConstraint"],
      
      def "Ident" $ coq "String",

      def "If" $
        doc "Pattern match on boolean values" $
        record [
          field "condition" $ coq "Term",
          field "returnAs" $ optional $ coq "ReturnAs",
          field "then" $ coq "Term",
          field "else" $ coq "Term"],
        
      def "ImplicitBinders" $
        doc "In the context of a function definition, these forms specify that name is an implicit argument." $
        union [
          field "maximallyInserted" $
            doc "The first form, with curly braces, makes name a maximally inserted implicit argument" $
            coq "TypeBinders",
            
          field "nonMaximallyInserted" $
            doc "The second form, with square brackets, makes name a non-maximally inserted implicit argument." $
            coq "TypeBinders"],

      def "Let" $
        doc "A let-in definition" $
        record [
          field "bindings" $ coq "LetBindings",
          field "in" $ coq "Term"],
      
      def "LetBinder" $
        doc "Some constructions allow the binding of a variable to value. This is called a “let-binder”." $
        record [
          field "name" $ coq "Name",
          field "type" $ optional $ coq "Type",
          field "term" $ coq "Term"],

      def "LetBindings" $ union [
        field "named" $ coq "LetNamed",
        field "destructuring" $ coq "LetDestructuring"],
      
      def "LetNamed" $ record [
        field "binder" $ coq "LetBinder",
        field "binders" $ list $ coq "Binder"],
      
      def "LetDestructuring" $ union [
        field "variant1" $ coq "LetDestructuringVariant1",
        field "variant2" $ coq "LetDestructuringVariant2",
        field "variant3" $ coq "LetDestructuringVariant3"],
      
      def "LetDestructuringVariant1" $ record [
        field "names" $ list $ coq "Name",
        field "returnAs" $ optional $ coq "ReturnAs",
        field "term" $ coq "Term"],
      
      def "LetDestructuringVariant2" $ record [
        field "pattern" $ coq "Pattern",
        field "term" $ coq "Term",
        field "return" $ optional $ coq "Term100"],
        
      def "LetDestructuringVariant3" $ record [
        field "pattern1" $ coq "Pattern",
        field "pattern2" $ coq "Pattern",
        field "term" $ coq "Term",
        field "return" $ coq "Term100"],
      
      def "Match" $ record [
        field "caseItems" $ nonemptyList $ coq "CaseItem",
        field "return" $ optional $ coq "Term100",
        field "pipe" boolean,
        field "equations" $ list $ coq "Equation"],
              
      def "Name" $ optional $ coq "Ident",

      def "Natural" $
        doc "A non-negative arbitrary-precision integer"
        bigint,

      def "Number" bigfloat,
      
      def "OneTerm" $ union [
        field "explicit" $ coq "QualidAnnotated",
        field "term1" $ coq "Term1"],

      def "OpenBinders" $ union [
        field "type" $ coq "TypeBinders",
        field "binders" $ list $ coq "Binder"],

      def "Pattern" $ union [
        field "pattern" $ coq "Pattern10",
        field "term" $ optional $ coq "Term"],

      def "Pattern0" $ union [
        field "qualid" $ coq "Qualid",
        field "qualIdAndPattern" $ coq "QualidAndPattern",
        field "placeholder" unit,
        field "parens" $ nonemptyList $ coq "Pattern",
        field "number" $ coq "Number",
        field "string" $ coq "String"],

      def "Pattern1" $ record [
        field "pattern" $ coq "Pattern0",
        field "scope" $ optional $ coq "ScopeKey"],
      
      def "Pattern10" $ union [
        field "as" $ coq "Pattern10As",
        field "patterns" $ coq "Pattern10Patterns",
        field "qualiid" $ coq "Pattern10Qualid"],
      
      def "Pattern10As" $ record [
        field "pattern" $ coq "Pattern1",
        field "as" $ coq "Name"],
        
      def "Pattern10Patterns" $ record [
        field "pattern" $ coq "Pattern1",
        field "patterns" $ list $ coq "Pattern1"],
      
      def "Pattern10Qualid" $ record [
        field "qualid" $ coq "Qualid",
        field "patterns" $ list $ coq "Pattern1"],
      
      def "PrimitiveNotations" $ union [
        field "number" $ coq "Number",
        field "string" $ coq "String"],
        
      def "Qualid" $
        doc "A qualified identifier" $
        record [
          field "id" $ coq "Ident",
          field "fieldIds" $ list $ coq "FieldIdent"],
            
      def "QualidAndPattern" $ record [
        field "qualid" $ coq "Qualid",
        field "pattern" $ coq "Pattern"],
        
      def "QualidAnnotated" $ record [
        field "qualid" $ coq "Qualid",
        field "univAnnot" $ optional $ coq "UnivAnnot"],
       
      def "ReturnAs" $ record [
        field "as" $ optional $ coq "Name",
        field "return" $ coq "Term100"],

      def "ScopeKey" $ coq "Ident",

      def "Sort" $
        doc "The types of types are called sorts." $
        union [
          field "set" $
            doc "The sort 𝖲𝖾𝗍 intends to be the type of small sets." unit,
          field "prop" $
            doc "The sort 𝖯𝗋𝗈𝗉 intends to be the type of logical propositions." unit,
          field "sProp" $
            doc "The sort 𝖲𝖯𝗋𝗈𝗉 is like 𝖯𝗋𝗈𝗉 but the propositions in 𝖲𝖯𝗋𝗈𝗉 are known to have irrelevant proofs (all proofs are equal)." unit,
          field "type" unit,
          field "typeWithAnyUniverse" unit,
          field "typeWithUniverse" $ coq "Universe"],
      
      def "String" string,
      
      def "Term" $ union [
        field "forallOrFun" $ coq "ForallOrFun",
        field "let" $ coq "Let",
        field "if" $ coq "If",
        field "fix" $ coq "Fix",
        field "cofix" $ coq "Cofix",
        field "term100" $ coq "Term100"],
      
      def "Term0" $ union [
        field "qualidAnnotated" $ coq "QualidAnnotated",
        field "sort" $ coq "Sort",
        field "primitiveNotations" $ coq "PrimitiveNotations",
        field "evar" $ coq "ExistentialVariable",
        field "match" $ coq "Match",
        field "record" unit,
        field "generalizing" unit,
        -- 	[| term*; | term : type? |] univ_annot? -- TODO
        field "ltac" unit,
        field "parens" $ coq "Term"],

      def "Term1" $ union [
        field "projection" unit,
        field "scope" unit,
        field "term0" $ coq "Term0"],

      def "Term10" $ union [
        field "application" $ coq "Application",
        field "oneTerm" $ coq "OneTerm"],

      def "Term100" $ union [
        field "cast" $ coq "TypeCast",
        field "term10" $ coq "Term10"],
              
      def "Type" $ coq "Term",

      def "TypeCast" $ record [
        field "term" $ coq "Term10",
        field "type" $ coq "Type",
        field "operator" $ coq "TypeCastOperator"],
        
      def "TypeCastOperator" $ union [
        field "normal" $
          doc "The expression term10 : type is a type cast expression. It enforces the type of term10 to be type."  unit,
        field "vmCompute" $
          doc "term10 <: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute)." unit,
        field "nativeCompute" $
          doc "term10 <<: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute)." unit],
      
      def "TypeBinders" $ record [
        field "names" $ nonemptyList $ coq "Name",
        field "type" $ coq "Type"],

      def "TypeclassConstraint" $
        record [
          field "name" $ optional $ coq "Name",
          field "generalizing" boolean,
          field "term" $ coq "Term"],
          
      def "UnivAnnot" $ list $ coq "UniverseLevel",
      
      def "Universe" $ union [
        field "max" $ nonemptyList $ coq "UniverseExpr",
        field "expr" $ coq "UniverseExpr"],
        
      def "UniverseExpr" $ record [
        field "name" $ coq "UniverseName",
        field "number" $ optional $ coq "Natural"],
      
      def "UniverseLevel" $ union [
        field "set" unit,
        field "prop" unit,
        field "type" unit,
        field "ignored" unit,
        field "qualid" $ coq "Qualid"],
        
      def "UniverseName" $ union [
        field "qualid" $ coq "Qualid",
        field "set" unit,
        field "prop" unit]]
