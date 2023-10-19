-- | A model for Coq core and extensions. Based on the Coq 8.15 grammar:
-- |   https://coq.github.io/doc/v8.15/refman/language/core/basic.html#essential-vocabulary

module Hydra.Langs.Coq.Syntax where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data AnnotatedApplication = 
  AnnotatedApplication {
    annotatedApplicationAnnot :: QualidAnnotated,
    annotatedApplicationTerms :: [Term1]}
  deriving (Eq, Ord, Read, Show)

_AnnotatedApplication = (Core.Name "hydra/langs/coq/syntax.AnnotatedApplication")

_AnnotatedApplication_annot = (Core.FieldName "annot")

_AnnotatedApplication_terms = (Core.FieldName "terms")

data Application = 
  ApplicationNormal NormalApplication |
  ApplicationAnnotated AnnotatedApplication
  deriving (Eq, Ord, Read, Show)

_Application = (Core.Name "hydra/langs/coq/syntax.Application")

_Application_normal = (Core.FieldName "normal")

_Application_annotated = (Core.FieldName "annotated")

data Arg = 
  ArgIdent IdentArg |
  ArgNatural NaturalArg |
  ArgTerm Term1
  deriving (Eq, Ord, Read, Show)

_Arg = (Core.Name "hydra/langs/coq/syntax.Arg")

_Arg_ident = (Core.FieldName "ident")

_Arg_natural = (Core.FieldName "natural")

_Arg_term = (Core.FieldName "term")

data Binder = 
  BinderName Name |
  BinderType TypeBinders |
  BinderTerm LetBinder |
  BinderImplicit ImplicitBinders |
  BinderGeneralizing GeneralizingBinder |
  BinderPattern Pattern0
  deriving (Eq, Ord, Read, Show)

_Binder = (Core.Name "hydra/langs/coq/syntax.Binder")

_Binder_name = (Core.FieldName "name")

_Binder_type = (Core.FieldName "type")

_Binder_term = (Core.FieldName "term")

_Binder_implicit = (Core.FieldName "implicit")

_Binder_generalizing = (Core.FieldName "generalizing")

_Binder_pattern = (Core.FieldName "pattern")

data CaseItem = 
  CaseItem {
    caseItemTerm :: Term100,
    caseItemAs :: (Maybe Name),
    caseItemIn :: (Maybe Pattern)}
  deriving (Eq, Ord, Read, Show)

_CaseItem = (Core.Name "hydra/langs/coq/syntax.CaseItem")

_CaseItem_term = (Core.FieldName "term")

_CaseItem_as = (Core.FieldName "as")

_CaseItem_in = (Core.FieldName "in")

data Cofix = 
  Cofix {
    cofixBody :: CofixBody,
    cofixQual :: (Maybe CofixQual)}
  deriving (Eq, Ord, Read, Show)

_Cofix = (Core.Name "hydra/langs/coq/syntax.Cofix")

_Cofix_body = (Core.FieldName "body")

_Cofix_qual = (Core.FieldName "qual")

data CofixBody = 
  CofixBody {
    cofixBodyIdent :: Ident,
    cofixBodyBinders :: [Binder],
    cofixBodyType :: (Maybe Type),
    cofixBodyTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_CofixBody = (Core.Name "hydra/langs/coq/syntax.CofixBody")

_CofixBody_ident = (Core.FieldName "ident")

_CofixBody_binders = (Core.FieldName "binders")

_CofixBody_type = (Core.FieldName "type")

_CofixBody_term = (Core.FieldName "term")

data CofixQual = 
  CofixQualIn Term |
  CofixQualWith CofixWith
  deriving (Eq, Ord, Read, Show)

_CofixQual = (Core.Name "hydra/langs/coq/syntax.CofixQual")

_CofixQual_in = (Core.FieldName "in")

_CofixQual_with = (Core.FieldName "with")

data CofixWith = 
  CofixWith {
    cofixWithWith :: [CofixBody],
    cofixWithFor :: (Maybe Ident)}
  deriving (Eq, Ord, Read, Show)

_CofixWith = (Core.Name "hydra/langs/coq/syntax.CofixWith")

_CofixWith_with = (Core.FieldName "with")

_CofixWith_for = (Core.FieldName "for")

data Equation = 
  Equation {
    equationPattern :: [[Pattern]],
    equationTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_Equation = (Core.Name "hydra/langs/coq/syntax.Equation")

_Equation_pattern = (Core.FieldName "pattern")

_Equation_term = (Core.FieldName "term")

data ExistentialVariable = 
  ExistentialVariable {
    existentialVariableIdent :: Ident,
    existentialVariableVariant :: ExistentialVariableVariant}
  deriving (Eq, Ord, Read, Show)

_ExistentialVariable = (Core.Name "hydra/langs/coq/syntax.ExistentialVariable")

_ExistentialVariable_ident = (Core.FieldName "ident")

_ExistentialVariable_variant = (Core.FieldName "variant")

data ExistentialVariableVariant = 
  ExistentialVariableVariantPlaceholder  |
  ExistentialVariableVariantInside1  |
  ExistentialVariableVariantInside2  |
  ExistentialVariableVariantOutside (Maybe IdentArg)
  deriving (Eq, Ord, Read, Show)

_ExistentialVariableVariant = (Core.Name "hydra/langs/coq/syntax.ExistentialVariableVariant")

_ExistentialVariableVariant_placeholder = (Core.FieldName "placeholder")

_ExistentialVariableVariant_inside1 = (Core.FieldName "inside1")

_ExistentialVariableVariant_inside2 = (Core.FieldName "inside2")

_ExistentialVariableVariant_outside = (Core.FieldName "outside")

newtype FieldIdent = 
  FieldIdent {
    unFieldIdent :: Ident}
  deriving (Eq, Ord, Read, Show)

_FieldIdent = (Core.Name "hydra/langs/coq/syntax.FieldIdent")

data Fix = 
  FixDecl Fix_Decl |
  FixQual (Maybe Fix_Qual)
  deriving (Eq, Ord, Read, Show)

_Fix = (Core.Name "hydra/langs/coq/syntax.Fix")

_Fix_decl = (Core.FieldName "decl")

_Fix_qual = (Core.FieldName "qual")

data FixAnnot = 
  FixAnnotStruct Ident |
  FixAnnotWf FixAnnot_Wf |
  FixAnnotMeasure FixAnnot_Measure
  deriving (Eq, Ord, Read, Show)

_FixAnnot = (Core.Name "hydra/langs/coq/syntax.FixAnnot")

_FixAnnot_struct = (Core.FieldName "struct")

_FixAnnot_wf = (Core.FieldName "wf")

_FixAnnot_measure = (Core.FieldName "measure")

data FixAnnot_Measure = 
  FixAnnot_Measure {
    fixAnnot_MeasureTerm :: OneTerm,
    fixAnnot_MeasureIdent :: (Maybe Ident),
    fixAnnot_MeasureTerm2 :: (Maybe OneTerm)}
  deriving (Eq, Ord, Read, Show)

_FixAnnot_Measure = (Core.Name "hydra/langs/coq/syntax.FixAnnot.Measure")

_FixAnnot_Measure_term = (Core.FieldName "term")

_FixAnnot_Measure_ident = (Core.FieldName "ident")

_FixAnnot_Measure_term2 = (Core.FieldName "term2")

data FixAnnot_Wf = 
  FixAnnot_Wf {
    fixAnnot_WfTerm :: OneTerm,
    fixAnnot_WfIdent :: Ident}
  deriving (Eq, Ord, Read, Show)

_FixAnnot_Wf = (Core.Name "hydra/langs/coq/syntax.FixAnnot.Wf")

_FixAnnot_Wf_term = (Core.FieldName "term")

_FixAnnot_Wf_ident = (Core.FieldName "ident")

data Fix_Decl = 
  Fix_Decl {
    fix_DeclIdent :: Ident,
    fix_DeclBinders :: [Binder],
    fix_DeclAnnot :: (Maybe FixAnnot),
    fix_DeclType :: (Maybe Type),
    fix_DeclTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_Fix_Decl = (Core.Name "hydra/langs/coq/syntax.Fix.Decl")

_Fix_Decl_ident = (Core.FieldName "ident")

_Fix_Decl_binders = (Core.FieldName "binders")

_Fix_Decl_annot = (Core.FieldName "annot")

_Fix_Decl_type = (Core.FieldName "type")

_Fix_Decl_term = (Core.FieldName "term")

data Fix_Qual = 
  Fix_QualIn Term |
  Fix_QualWith FixWith
  deriving (Eq, Ord, Read, Show)

_Fix_Qual = (Core.Name "hydra/langs/coq/syntax.Fix.Qual")

_Fix_Qual_in = (Core.FieldName "in")

_Fix_Qual_with = (Core.FieldName "with")

data FixWith = 
  FixWith {
    fixWithDecls :: [Fix_Decl],
    fixWithFor :: (Maybe Ident)}
  deriving (Eq, Ord, Read, Show)

_FixWith = (Core.Name "hydra/langs/coq/syntax.FixWith")

_FixWith_decls = (Core.FieldName "decls")

_FixWith_for = (Core.FieldName "for")

data Forall = 
  Forall {
    forallBinders :: OpenBinders,
    forallType :: Type}
  deriving (Eq, Ord, Read, Show)

_Forall = (Core.Name "hydra/langs/coq/syntax.Forall")

_Forall_binders = (Core.FieldName "binders")

_Forall_type = (Core.FieldName "type")

data ForallOrFun = 
  ForallOrFunForall Forall |
  ForallOrFunFun Fun
  deriving (Eq, Ord, Read, Show)

_ForallOrFun = (Core.Name "hydra/langs/coq/syntax.ForallOrFun")

_ForallOrFun_forall = (Core.FieldName "forall")

_ForallOrFun_fun = (Core.FieldName "fun")

data Fun = 
  Fun {
    funBinders :: OpenBinders,
    funBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Fun = (Core.Name "hydra/langs/coq/syntax.Fun")

_Fun_binders = (Core.FieldName "binders")

_Fun_body = (Core.FieldName "body")

data GeneralizingBinder = 
  -- | Terms surrounded by `( ) introduce their free variables as explicit arguments
  GeneralizingBinderExplicit TypeclassConstraint |
  -- | Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments
  GeneralizingBinderImplicitMaximallyInserted TypeclassConstraint |
  -- | Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments
  GeneralizingBinderImplicitNonMaximallyInserted TypeclassConstraint
  deriving (Eq, Ord, Read, Show)

_GeneralizingBinder = (Core.Name "hydra/langs/coq/syntax.GeneralizingBinder")

_GeneralizingBinder_explicit = (Core.FieldName "explicit")

_GeneralizingBinder_implicitMaximallyInserted = (Core.FieldName "implicitMaximallyInserted")

_GeneralizingBinder_implicitNonMaximallyInserted = (Core.FieldName "implicitNonMaximallyInserted")

newtype Ident = 
  Ident {
    unIdent :: String_}
  deriving (Eq, Ord, Read, Show)

_Ident = (Core.Name "hydra/langs/coq/syntax.Ident")

data IdentArg = 
  IdentArg {
    identArgIdent :: Ident,
    identArgTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_IdentArg = (Core.Name "hydra/langs/coq/syntax.IdentArg")

_IdentArg_ident = (Core.FieldName "ident")

_IdentArg_term = (Core.FieldName "term")

-- | Pattern match on boolean values
data If = 
  If {
    ifCondition :: Term,
    ifReturnAs :: (Maybe ReturnAs),
    ifThen :: Term,
    ifElse :: Term}
  deriving (Eq, Ord, Read, Show)

_If = (Core.Name "hydra/langs/coq/syntax.If")

_If_condition = (Core.FieldName "condition")

_If_returnAs = (Core.FieldName "returnAs")

_If_then = (Core.FieldName "then")

_If_else = (Core.FieldName "else")

-- | In the context of a function definition, these forms specify that name is an implicit argument.
data ImplicitBinders = 
  -- | The first form, with curly braces, makes name a maximally inserted implicit argument
  ImplicitBindersMaximallyInserted TypeBinders |
  -- | The second form, with square brackets, makes name a non-maximally inserted implicit argument.
  ImplicitBindersNonMaximallyInserted TypeBinders
  deriving (Eq, Ord, Read, Show)

_ImplicitBinders = (Core.Name "hydra/langs/coq/syntax.ImplicitBinders")

_ImplicitBinders_maximallyInserted = (Core.FieldName "maximallyInserted")

_ImplicitBinders_nonMaximallyInserted = (Core.FieldName "nonMaximallyInserted")

-- | A let-in definition
data Let = 
  Let {
    letBindings :: LetBindings,
    letIn :: Term}
  deriving (Eq, Ord, Read, Show)

_Let = (Core.Name "hydra/langs/coq/syntax.Let")

_Let_bindings = (Core.FieldName "bindings")

_Let_in = (Core.FieldName "in")

-- | Some constructions allow the binding of a variable to value. This is called a “let-binder”.
data LetBinder = 
  LetBinder {
    letBinderName :: Name,
    letBinderType :: (Maybe Type),
    letBinderTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_LetBinder = (Core.Name "hydra/langs/coq/syntax.LetBinder")

_LetBinder_name = (Core.FieldName "name")

_LetBinder_type = (Core.FieldName "type")

_LetBinder_term = (Core.FieldName "term")

data LetBindings = 
  LetBindingsNamed LetNamed |
  LetBindingsDestructuring LetDestructuring
  deriving (Eq, Ord, Read, Show)

_LetBindings = (Core.Name "hydra/langs/coq/syntax.LetBindings")

_LetBindings_named = (Core.FieldName "named")

_LetBindings_destructuring = (Core.FieldName "destructuring")

data LetNamed = 
  LetNamed {
    letNamedBinder :: LetBinder,
    letNamedBinders :: [Binder]}
  deriving (Eq, Ord, Read, Show)

_LetNamed = (Core.Name "hydra/langs/coq/syntax.LetNamed")

_LetNamed_binder = (Core.FieldName "binder")

_LetNamed_binders = (Core.FieldName "binders")

data LetDestructuring = 
  LetDestructuringVariant1 LetDestructuring_Variant1 |
  LetDestructuringVariant2 LetDestructuring_Variant2 |
  LetDestructuringVariant3 LetDestructuring_Variant3
  deriving (Eq, Ord, Read, Show)

_LetDestructuring = (Core.Name "hydra/langs/coq/syntax.LetDestructuring")

_LetDestructuring_variant1 = (Core.FieldName "variant1")

_LetDestructuring_variant2 = (Core.FieldName "variant2")

_LetDestructuring_variant3 = (Core.FieldName "variant3")

data LetDestructuring_Variant1 = 
  LetDestructuring_Variant1 {
    letDestructuring_Variant1Names :: [Name],
    letDestructuring_Variant1ReturnAs :: (Maybe ReturnAs),
    letDestructuring_Variant1Term :: Term}
  deriving (Eq, Ord, Read, Show)

_LetDestructuring_Variant1 = (Core.Name "hydra/langs/coq/syntax.LetDestructuring.Variant1")

_LetDestructuring_Variant1_names = (Core.FieldName "names")

_LetDestructuring_Variant1_returnAs = (Core.FieldName "returnAs")

_LetDestructuring_Variant1_term = (Core.FieldName "term")

data LetDestructuring_Variant2 = 
  LetDestructuring_Variant2 {
    letDestructuring_Variant2Pattern :: Pattern,
    letDestructuring_Variant2Term :: Term,
    letDestructuring_Variant2Return :: (Maybe Term100)}
  deriving (Eq, Ord, Read, Show)

_LetDestructuring_Variant2 = (Core.Name "hydra/langs/coq/syntax.LetDestructuring.Variant2")

_LetDestructuring_Variant2_pattern = (Core.FieldName "pattern")

_LetDestructuring_Variant2_term = (Core.FieldName "term")

_LetDestructuring_Variant2_return = (Core.FieldName "return")

data LetDestructuring_Variant3 = 
  LetDestructuring_Variant3 {
    letDestructuring_Variant3Pattern1 :: Pattern,
    letDestructuring_Variant3Pattern2 :: Pattern,
    letDestructuring_Variant3Term :: Term,
    letDestructuring_Variant3Return :: Term100}
  deriving (Eq, Ord, Read, Show)

_LetDestructuring_Variant3 = (Core.Name "hydra/langs/coq/syntax.LetDestructuring.Variant3")

_LetDestructuring_Variant3_pattern1 = (Core.FieldName "pattern1")

_LetDestructuring_Variant3_pattern2 = (Core.FieldName "pattern2")

_LetDestructuring_Variant3_term = (Core.FieldName "term")

_LetDestructuring_Variant3_return = (Core.FieldName "return")

data Match = 
  Match {
    matchCaseItems :: [CaseItem],
    matchReturn :: (Maybe Term100),
    matchPipe :: Bool,
    matchEquations :: [Equation]}
  deriving (Eq, Ord, Read, Show)

_Match = (Core.Name "hydra/langs/coq/syntax.Match")

_Match_caseItems = (Core.FieldName "caseItems")

_Match_return = (Core.FieldName "return")

_Match_pipe = (Core.FieldName "pipe")

_Match_equations = (Core.FieldName "equations")

newtype Name = 
  Name {
    unName :: (Maybe Ident)}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/langs/coq/syntax.Name")

-- | A non-negative arbitrary-precision integer
newtype Natural = 
  Natural {
    -- | A non-negative arbitrary-precision integer
    unNatural :: Integer}
  deriving (Eq, Ord, Read, Show)

_Natural = (Core.Name "hydra/langs/coq/syntax.Natural")

data NaturalArg = 
  NaturalArg {
    naturalArgNatural :: Natural,
    naturalArgTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_NaturalArg = (Core.Name "hydra/langs/coq/syntax.NaturalArg")

_NaturalArg_natural = (Core.FieldName "natural")

_NaturalArg_term = (Core.FieldName "term")

data NormalApplication = 
  NormalApplication {
    normalApplicationLhs :: Term1,
    normalApplicationRhs :: [Arg]}
  deriving (Eq, Ord, Read, Show)

_NormalApplication = (Core.Name "hydra/langs/coq/syntax.NormalApplication")

_NormalApplication_lhs = (Core.FieldName "lhs")

_NormalApplication_rhs = (Core.FieldName "rhs")

newtype Number = 
  Number {
    unNumber :: Double}
  deriving (Eq, Ord, Read, Show)

_Number = (Core.Name "hydra/langs/coq/syntax.Number")

data OneTerm = 
  OneTermExplicit QualidAnnotated |
  OneTermTerm1 Term1
  deriving (Eq, Ord, Read, Show)

_OneTerm = (Core.Name "hydra/langs/coq/syntax.OneTerm")

_OneTerm_explicit = (Core.FieldName "explicit")

_OneTerm_term1 = (Core.FieldName "term1")

data OpenBinders = 
  OpenBindersType TypeBinders |
  OpenBindersBinders [Binder]
  deriving (Eq, Ord, Read, Show)

_OpenBinders = (Core.Name "hydra/langs/coq/syntax.OpenBinders")

_OpenBinders_type = (Core.FieldName "type")

_OpenBinders_binders = (Core.FieldName "binders")

data Pattern = 
  PatternPattern Pattern10 |
  PatternTerm (Maybe Term)
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/langs/coq/syntax.Pattern")

_Pattern_pattern = (Core.FieldName "pattern")

_Pattern_term = (Core.FieldName "term")

data Pattern0 = 
  Pattern0Qualid Qualid |
  Pattern0QualIdAndPattern QualidAndPattern |
  Pattern0Placeholder  |
  Pattern0Parens [Pattern] |
  Pattern0Number Number |
  Pattern0String String_
  deriving (Eq, Ord, Read, Show)

_Pattern0 = (Core.Name "hydra/langs/coq/syntax.Pattern0")

_Pattern0_qualid = (Core.FieldName "qualid")

_Pattern0_qualIdAndPattern = (Core.FieldName "qualIdAndPattern")

_Pattern0_placeholder = (Core.FieldName "placeholder")

_Pattern0_parens = (Core.FieldName "parens")

_Pattern0_number = (Core.FieldName "number")

_Pattern0_string = (Core.FieldName "string")

data Pattern1 = 
  Pattern1 {
    pattern1Pattern :: Pattern0,
    pattern1Scope :: (Maybe ScopeKey)}
  deriving (Eq, Ord, Read, Show)

_Pattern1 = (Core.Name "hydra/langs/coq/syntax.Pattern1")

_Pattern1_pattern = (Core.FieldName "pattern")

_Pattern1_scope = (Core.FieldName "scope")

data Pattern10 = 
  Pattern10As Pattern10_As |
  Pattern10Patterns Pattern10_Patterns |
  Pattern10Qualiid Pattern10_Qualid
  deriving (Eq, Ord, Read, Show)

_Pattern10 = (Core.Name "hydra/langs/coq/syntax.Pattern10")

_Pattern10_as = (Core.FieldName "as")

_Pattern10_patterns = (Core.FieldName "patterns")

_Pattern10_qualiid = (Core.FieldName "qualiid")

data Pattern10_As = 
  Pattern10_As {
    pattern10_AsPattern :: Pattern1,
    pattern10_AsAs :: Name}
  deriving (Eq, Ord, Read, Show)

_Pattern10_As = (Core.Name "hydra/langs/coq/syntax.Pattern10.As")

_Pattern10_As_pattern = (Core.FieldName "pattern")

_Pattern10_As_as = (Core.FieldName "as")

data Pattern10_Patterns = 
  Pattern10_Patterns {
    pattern10_PatternsPattern :: Pattern1,
    pattern10_PatternsPatterns :: [Pattern1]}
  deriving (Eq, Ord, Read, Show)

_Pattern10_Patterns = (Core.Name "hydra/langs/coq/syntax.Pattern10.Patterns")

_Pattern10_Patterns_pattern = (Core.FieldName "pattern")

_Pattern10_Patterns_patterns = (Core.FieldName "patterns")

data Pattern10_Qualid = 
  Pattern10_Qualid {
    pattern10_QualidQualid :: Qualid,
    pattern10_QualidPatterns :: [Pattern1]}
  deriving (Eq, Ord, Read, Show)

_Pattern10_Qualid = (Core.Name "hydra/langs/coq/syntax.Pattern10.Qualid")

_Pattern10_Qualid_qualid = (Core.FieldName "qualid")

_Pattern10_Qualid_patterns = (Core.FieldName "patterns")

data PrimitiveNotations = 
  PrimitiveNotationsNumber Number |
  PrimitiveNotationsString String_
  deriving (Eq, Ord, Read, Show)

_PrimitiveNotations = (Core.Name "hydra/langs/coq/syntax.PrimitiveNotations")

_PrimitiveNotations_number = (Core.FieldName "number")

_PrimitiveNotations_string = (Core.FieldName "string")

-- | A qualified identifier
data Qualid = 
  Qualid {
    qualidId :: Ident,
    qualidFieldIds :: [FieldIdent]}
  deriving (Eq, Ord, Read, Show)

_Qualid = (Core.Name "hydra/langs/coq/syntax.Qualid")

_Qualid_id = (Core.FieldName "id")

_Qualid_fieldIds = (Core.FieldName "fieldIds")

data QualidAndPattern = 
  QualidAndPattern {
    qualidAndPatternQualid :: Qualid,
    qualidAndPatternPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_QualidAndPattern = (Core.Name "hydra/langs/coq/syntax.QualidAndPattern")

_QualidAndPattern_qualid = (Core.FieldName "qualid")

_QualidAndPattern_pattern = (Core.FieldName "pattern")

data QualidAnnotated = 
  QualidAnnotated {
    qualidAnnotatedQualid :: Qualid,
    qualidAnnotatedUnivAnnot :: (Maybe UnivAnnot)}
  deriving (Eq, Ord, Read, Show)

_QualidAnnotated = (Core.Name "hydra/langs/coq/syntax.QualidAnnotated")

_QualidAnnotated_qualid = (Core.FieldName "qualid")

_QualidAnnotated_univAnnot = (Core.FieldName "univAnnot")

data ReturnAs = 
  ReturnAs {
    returnAsAs :: (Maybe Name),
    returnAsReturn :: Term100}
  deriving (Eq, Ord, Read, Show)

_ReturnAs = (Core.Name "hydra/langs/coq/syntax.ReturnAs")

_ReturnAs_as = (Core.FieldName "as")

_ReturnAs_return = (Core.FieldName "return")

newtype ScopeKey = 
  ScopeKey {
    unScopeKey :: Ident}
  deriving (Eq, Ord, Read, Show)

_ScopeKey = (Core.Name "hydra/langs/coq/syntax.ScopeKey")

-- | The types of types are called sorts.
data Sort = 
  -- | The sort 𝖲𝖾𝗍 intends to be the type of small sets.
  SortSet  |
  -- | The sort 𝖯𝗋𝗈𝗉 intends to be the type of logical propositions.
  SortProp  |
  -- | The sort 𝖲𝖯𝗋𝗈𝗉 is like 𝖯𝗋𝗈𝗉 but the propositions in 𝖲𝖯𝗋𝗈𝗉 are known to have irrelevant proofs (all proofs are equal).
  SortSProp  |
  SortType  |
  SortTypeWithAnyUniverse  |
  SortTypeWithUniverse Universe
  deriving (Eq, Ord, Read, Show)

_Sort = (Core.Name "hydra/langs/coq/syntax.Sort")

_Sort_set = (Core.FieldName "set")

_Sort_prop = (Core.FieldName "prop")

_Sort_sProp = (Core.FieldName "sProp")

_Sort_type = (Core.FieldName "type")

_Sort_typeWithAnyUniverse = (Core.FieldName "typeWithAnyUniverse")

_Sort_typeWithUniverse = (Core.FieldName "typeWithUniverse")

newtype String_ = 
  String_ {
    unString :: String}
  deriving (Eq, Ord, Read, Show)

_String = (Core.Name "hydra/langs/coq/syntax.String")

data Term = 
  TermForallOrFun ForallOrFun |
  TermLet Let |
  TermIf If |
  TermFix Fix |
  TermCofix Cofix |
  TermTerm100 Term100
  deriving (Eq, Ord, Read, Show)

_Term = (Core.Name "hydra/langs/coq/syntax.Term")

_Term_forallOrFun = (Core.FieldName "forallOrFun")

_Term_let = (Core.FieldName "let")

_Term_if = (Core.FieldName "if")

_Term_fix = (Core.FieldName "fix")

_Term_cofix = (Core.FieldName "cofix")

_Term_term100 = (Core.FieldName "term100")

data Term0 = 
  Term0QualidAnnotated QualidAnnotated |
  Term0Sort Sort |
  Term0PrimitiveNotations PrimitiveNotations |
  Term0Evar ExistentialVariable |
  Term0Match Match |
  Term0Record  |
  Term0Generalizing  |
  Term0Ltac  |
  Term0Parens Term
  deriving (Eq, Ord, Read, Show)

_Term0 = (Core.Name "hydra/langs/coq/syntax.Term0")

_Term0_qualidAnnotated = (Core.FieldName "qualidAnnotated")

_Term0_sort = (Core.FieldName "sort")

_Term0_primitiveNotations = (Core.FieldName "primitiveNotations")

_Term0_evar = (Core.FieldName "evar")

_Term0_match = (Core.FieldName "match")

_Term0_record = (Core.FieldName "record")

_Term0_generalizing = (Core.FieldName "generalizing")

_Term0_ltac = (Core.FieldName "ltac")

_Term0_parens = (Core.FieldName "parens")

data Term1 = 
  Term1Projection  |
  Term1Scope  |
  Term1Term0 Term0
  deriving (Eq, Ord, Read, Show)

_Term1 = (Core.Name "hydra/langs/coq/syntax.Term1")

_Term1_projection = (Core.FieldName "projection")

_Term1_scope = (Core.FieldName "scope")

_Term1_term0 = (Core.FieldName "term0")

data Term10 = 
  Term10Application Application |
  Term10OneTerm OneTerm
  deriving (Eq, Ord, Read, Show)

_Term10 = (Core.Name "hydra/langs/coq/syntax.Term10")

_Term10_application = (Core.FieldName "application")

_Term10_oneTerm = (Core.FieldName "oneTerm")

data Term100 = 
  Term100Cast TypeCast |
  Term100Term10 Term10
  deriving (Eq, Ord, Read, Show)

_Term100 = (Core.Name "hydra/langs/coq/syntax.Term100")

_Term100_cast = (Core.FieldName "cast")

_Term100_term10 = (Core.FieldName "term10")

newtype Type = 
  Type {
    unType :: Term}
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/langs/coq/syntax.Type")

data TypeCast = 
  TypeCast {
    typeCastTerm :: Term10,
    typeCastType :: Type,
    typeCastOperator :: TypeCastOperator}
  deriving (Eq, Ord, Read, Show)

_TypeCast = (Core.Name "hydra/langs/coq/syntax.TypeCast")

_TypeCast_term = (Core.FieldName "term")

_TypeCast_type = (Core.FieldName "type")

_TypeCast_operator = (Core.FieldName "operator")

data TypeCastOperator = 
  -- | The expression term10 : type is a type cast expression. It enforces the type of term10 to be type.
  TypeCastOperatorNormal  |
  -- | term10 <: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute).
  TypeCastOperatorVmCompute  |
  -- | term10 <<: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute).
  TypeCastOperatorNativeCompute 
  deriving (Eq, Ord, Read, Show)

_TypeCastOperator = (Core.Name "hydra/langs/coq/syntax.TypeCastOperator")

_TypeCastOperator_normal = (Core.FieldName "normal")

_TypeCastOperator_vmCompute = (Core.FieldName "vmCompute")

_TypeCastOperator_nativeCompute = (Core.FieldName "nativeCompute")

data TypeBinders = 
  TypeBinders {
    typeBindersNames :: [Name],
    typeBindersType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeBinders = (Core.Name "hydra/langs/coq/syntax.TypeBinders")

_TypeBinders_names = (Core.FieldName "names")

_TypeBinders_type = (Core.FieldName "type")

data TypeclassConstraint = 
  TypeclassConstraint {
    typeclassConstraintName :: (Maybe Name),
    typeclassConstraintGeneralizing :: Bool,
    typeclassConstraintTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_TypeclassConstraint = (Core.Name "hydra/langs/coq/syntax.TypeclassConstraint")

_TypeclassConstraint_name = (Core.FieldName "name")

_TypeclassConstraint_generalizing = (Core.FieldName "generalizing")

_TypeclassConstraint_term = (Core.FieldName "term")

newtype UnivAnnot = 
  UnivAnnot {
    unUnivAnnot :: [UniverseLevel]}
  deriving (Eq, Ord, Read, Show)

_UnivAnnot = (Core.Name "hydra/langs/coq/syntax.UnivAnnot")

data Universe = 
  UniverseMax [Universe_Expr] |
  UniverseExpr Universe_Expr
  deriving (Eq, Ord, Read, Show)

_Universe = (Core.Name "hydra/langs/coq/syntax.Universe")

_Universe_max = (Core.FieldName "max")

_Universe_expr = (Core.FieldName "expr")

data Universe_Expr = 
  Universe_Expr {
    universe_ExprName :: UniverseName,
    universe_ExprNumber :: (Maybe Natural)}
  deriving (Eq, Ord, Read, Show)

_Universe_Expr = (Core.Name "hydra/langs/coq/syntax.Universe.Expr")

_Universe_Expr_name = (Core.FieldName "name")

_Universe_Expr_number = (Core.FieldName "number")

data UniverseLevel = 
  UniverseLevelSet  |
  UniverseLevelProp  |
  UniverseLevelType  |
  UniverseLevelIgnored  |
  UniverseLevelQualid Qualid
  deriving (Eq, Ord, Read, Show)

_UniverseLevel = (Core.Name "hydra/langs/coq/syntax.UniverseLevel")

_UniverseLevel_set = (Core.FieldName "set")

_UniverseLevel_prop = (Core.FieldName "prop")

_UniverseLevel_type = (Core.FieldName "type")

_UniverseLevel_ignored = (Core.FieldName "ignored")

_UniverseLevel_qualid = (Core.FieldName "qualid")

data UniverseName = 
  UniverseNameQualid Qualid |
  UniverseNameSet  |
  UniverseNameProp 
  deriving (Eq, Ord, Read, Show)

_UniverseName = (Core.Name "hydra/langs/coq/syntax.UniverseName")

_UniverseName_qualid = (Core.FieldName "qualid")

_UniverseName_set = (Core.FieldName "set")

_UniverseName_prop = (Core.FieldName "prop")