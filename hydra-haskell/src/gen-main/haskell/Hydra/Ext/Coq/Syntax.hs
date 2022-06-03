module Hydra.Ext.Coq.Syntax where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

data Application 
  = ApplicationNormal ApplicationNormal
  | ApplicationAnnotated ApplicationAnnotated
  deriving (Eq, Ord, Read, Show)

_Application = (Core.Name "hydra/ext/coq/syntax.Application")

_Application_normal = (Core.FieldName "normal")

_Application_annotated = (Core.FieldName "annotated")

data ApplicationNormal 
  = ApplicationNormal {
    applicationNormalLhs :: Term1,
    applicationNormalRhs :: [Arg]}
  deriving (Eq, Ord, Read, Show)

_ApplicationNormal = (Core.Name "hydra/ext/coq/syntax.ApplicationNormal")

_ApplicationNormal_lhs = (Core.FieldName "lhs")

_ApplicationNormal_rhs = (Core.FieldName "rhs")

data ApplicationAnnotated 
  = ApplicationAnnotated {
    applicationAnnotatedAnnot :: QualidAnnotated,
    applicationAnnotatedTerms :: [Term1]}
  deriving (Eq, Ord, Read, Show)

_ApplicationAnnotated = (Core.Name "hydra/ext/coq/syntax.ApplicationAnnotated")

_ApplicationAnnotated_annot = (Core.FieldName "annot")

_ApplicationAnnotated_terms = (Core.FieldName "terms")

data Arg 
  = ArgIdent ArgIdent
  | ArgNatural ArgNatural
  | ArgTerm Term1
  deriving (Eq, Ord, Read, Show)

_Arg = (Core.Name "hydra/ext/coq/syntax.Arg")

_Arg_ident = (Core.FieldName "ident")

_Arg_natural = (Core.FieldName "natural")

_Arg_term = (Core.FieldName "term")

data ArgIdent 
  = ArgIdent {
    argIdentIdent :: Ident,
    argIdentTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_ArgIdent = (Core.Name "hydra/ext/coq/syntax.ArgIdent")

_ArgIdent_ident = (Core.FieldName "ident")

_ArgIdent_term = (Core.FieldName "term")

data ArgNatural 
  = ArgNatural {
    argNaturalNatural :: Natural,
    argNaturalTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_ArgNatural = (Core.Name "hydra/ext/coq/syntax.ArgNatural")

_ArgNatural_natural = (Core.FieldName "natural")

_ArgNatural_term = (Core.FieldName "term")

data Binder 
  = BinderName Name
  | BinderType TypeBinders
  | BinderTerm LetBinder
  | BinderImplicit ImplicitBinders
  | BinderGeneralizing GeneralizingBinder
  | BinderPattern Pattern0
  deriving (Eq, Ord, Read, Show)

_Binder = (Core.Name "hydra/ext/coq/syntax.Binder")

_Binder_name = (Core.FieldName "name")

_Binder_type = (Core.FieldName "type")

_Binder_term = (Core.FieldName "term")

_Binder_implicit = (Core.FieldName "implicit")

_Binder_generalizing = (Core.FieldName "generalizing")

_Binder_pattern = (Core.FieldName "pattern")

data CaseItem 
  = CaseItem {
    caseItemTerm :: Term100,
    caseItemAs :: (Maybe Name),
    caseItemIn :: (Maybe Pattern)}
  deriving (Eq, Ord, Read, Show)

_CaseItem = (Core.Name "hydra/ext/coq/syntax.CaseItem")

_CaseItem_term = (Core.FieldName "term")

_CaseItem_as = (Core.FieldName "as")

_CaseItem_in = (Core.FieldName "in")

data Cofix 
  = Cofix {
    cofixBody :: CofixBody,
    cofixQual :: (Maybe CofixQual)}
  deriving (Eq, Ord, Read, Show)

_Cofix = (Core.Name "hydra/ext/coq/syntax.Cofix")

_Cofix_body = (Core.FieldName "body")

_Cofix_qual = (Core.FieldName "qual")

data CofixBody 
  = CofixBody {
    cofixBodyIdent :: Ident,
    cofixBodyBinders :: [Binder],
    cofixBodyType :: (Maybe Type),
    cofixBodyTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_CofixBody = (Core.Name "hydra/ext/coq/syntax.CofixBody")

_CofixBody_ident = (Core.FieldName "ident")

_CofixBody_binders = (Core.FieldName "binders")

_CofixBody_type = (Core.FieldName "type")

_CofixBody_term = (Core.FieldName "term")

data CofixQual 
  = CofixQualIn Term
  | CofixQualWith CofixWith
  deriving (Eq, Ord, Read, Show)

_CofixQual = (Core.Name "hydra/ext/coq/syntax.CofixQual")

_CofixQual_in = (Core.FieldName "in")

_CofixQual_with = (Core.FieldName "with")

data CofixWith 
  = CofixWith {
    cofixWithWith :: [CofixBody],
    cofixWithFor :: (Maybe Ident)}
  deriving (Eq, Ord, Read, Show)

_CofixWith = (Core.Name "hydra/ext/coq/syntax.CofixWith")

_CofixWith_with = (Core.FieldName "with")

_CofixWith_for = (Core.FieldName "for")

data Equation 
  = Equation {
    equationPattern :: [[Pattern]],
    equationTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_Equation = (Core.Name "hydra/ext/coq/syntax.Equation")

_Equation_pattern = (Core.FieldName "pattern")

_Equation_term = (Core.FieldName "term")

data ExistentialVariable 
  = ExistentialVariable {
    existentialVariableIdent :: Ident,
    existentialVariableVariant :: ExistentialVariableVariant}
  deriving (Eq, Ord, Read, Show)

_ExistentialVariable = (Core.Name "hydra/ext/coq/syntax.ExistentialVariable")

_ExistentialVariable_ident = (Core.FieldName "ident")

_ExistentialVariable_variant = (Core.FieldName "variant")

data ExistentialVariableVariant 
  = ExistentialVariableVariantPlaceholder 
  | ExistentialVariableVariantInside1 
  | ExistentialVariableVariantInside2 
  | ExistentialVariableVariantOutside (Maybe ArgIdent)
  deriving (Eq, Ord, Read, Show)

_ExistentialVariableVariant = (Core.Name "hydra/ext/coq/syntax.ExistentialVariableVariant")

_ExistentialVariableVariant_placeholder = (Core.FieldName "placeholder")

_ExistentialVariableVariant_inside1 = (Core.FieldName "inside1")

_ExistentialVariableVariant_inside2 = (Core.FieldName "inside2")

_ExistentialVariableVariant_outside = (Core.FieldName "outside")

newtype FieldIdent 
  = FieldIdent {
    unFieldIdent :: Ident}
  deriving (Eq, Ord, Read, Show)

_FieldIdent = (Core.Name "hydra/ext/coq/syntax.FieldIdent")

data Fix 
  = FixDecl FixDecl
  | FixQual (Maybe FixQual)
  deriving (Eq, Ord, Read, Show)

_Fix = (Core.Name "hydra/ext/coq/syntax.Fix")

_Fix_decl = (Core.FieldName "decl")

_Fix_qual = (Core.FieldName "qual")

data FixAnnot 
  = FixAnnotStruct Ident
  | FixAnnotWf FixAnnotWf
  | FixAnnotMeasure FixAnnotMeasure
  deriving (Eq, Ord, Read, Show)

_FixAnnot = (Core.Name "hydra/ext/coq/syntax.FixAnnot")

_FixAnnot_struct = (Core.FieldName "struct")

_FixAnnot_wf = (Core.FieldName "wf")

_FixAnnot_measure = (Core.FieldName "measure")

data FixAnnotMeasure 
  = FixAnnotMeasure {
    fixAnnotMeasureTerm :: OneTerm,
    fixAnnotMeasureIdent :: (Maybe Ident),
    fixAnnotMeasureTerm2 :: (Maybe OneTerm)}
  deriving (Eq, Ord, Read, Show)

_FixAnnotMeasure = (Core.Name "hydra/ext/coq/syntax.FixAnnotMeasure")

_FixAnnotMeasure_term = (Core.FieldName "term")

_FixAnnotMeasure_ident = (Core.FieldName "ident")

_FixAnnotMeasure_term2 = (Core.FieldName "term2")

data FixAnnotWf 
  = FixAnnotWf {
    fixAnnotWfTerm :: OneTerm,
    fixAnnotWfIdent :: Ident}
  deriving (Eq, Ord, Read, Show)

_FixAnnotWf = (Core.Name "hydra/ext/coq/syntax.FixAnnotWf")

_FixAnnotWf_term = (Core.FieldName "term")

_FixAnnotWf_ident = (Core.FieldName "ident")

data FixDecl 
  = FixDecl {
    fixDeclIdent :: Ident,
    fixDeclBinders :: [Binder],
    fixDeclAnnot :: (Maybe FixAnnot),
    fixDeclType :: (Maybe Type),
    fixDeclTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_FixDecl = (Core.Name "hydra/ext/coq/syntax.FixDecl")

_FixDecl_ident = (Core.FieldName "ident")

_FixDecl_binders = (Core.FieldName "binders")

_FixDecl_annot = (Core.FieldName "annot")

_FixDecl_type = (Core.FieldName "type")

_FixDecl_term = (Core.FieldName "term")

data FixQual 
  = FixQualIn Term
  | FixQualWith FixWith
  deriving (Eq, Ord, Read, Show)

_FixQual = (Core.Name "hydra/ext/coq/syntax.FixQual")

_FixQual_in = (Core.FieldName "in")

_FixQual_with = (Core.FieldName "with")

data FixWith 
  = FixWith {
    fixWithDecls :: [FixDecl],
    fixWithFor :: (Maybe Ident)}
  deriving (Eq, Ord, Read, Show)

_FixWith = (Core.Name "hydra/ext/coq/syntax.FixWith")

_FixWith_decls = (Core.FieldName "decls")

_FixWith_for = (Core.FieldName "for")

data Forall 
  = Forall {
    forallBinders :: OpenBinders,
    forallType :: Type}
  deriving (Eq, Ord, Read, Show)

_Forall = (Core.Name "hydra/ext/coq/syntax.Forall")

_Forall_binders = (Core.FieldName "binders")

_Forall_type = (Core.FieldName "type")

data ForallOrFun 
  = ForallOrFunForall Forall
  | ForallOrFunFun Fun
  deriving (Eq, Ord, Read, Show)

_ForallOrFun = (Core.Name "hydra/ext/coq/syntax.ForallOrFun")

_ForallOrFun_forall = (Core.FieldName "forall")

_ForallOrFun_fun = (Core.FieldName "fun")

data Fun 
  = Fun {
    funBinders :: OpenBinders,
    funBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Fun = (Core.Name "hydra/ext/coq/syntax.Fun")

_Fun_binders = (Core.FieldName "binders")

_Fun_body = (Core.FieldName "body")

data GeneralizingBinder 
  = GeneralizingBinderExplicit TypeclassConstraint
  | GeneralizingBinderImplicitMaximallyInserted TypeclassConstraint
  | GeneralizingBinderImplicitNonMaximallyInserted TypeclassConstraint
  deriving (Eq, Ord, Read, Show)

_GeneralizingBinder = (Core.Name "hydra/ext/coq/syntax.GeneralizingBinder")

_GeneralizingBinder_explicit = (Core.FieldName "explicit")

_GeneralizingBinder_implicitMaximallyInserted = (Core.FieldName "implicitMaximallyInserted")

_GeneralizingBinder_implicitNonMaximallyInserted = (Core.FieldName "implicitNonMaximallyInserted")

newtype Ident 
  = Ident {
    unIdent :: String}
  deriving (Eq, Ord, Read, Show)

_Ident = (Core.Name "hydra/ext/coq/syntax.Ident")

-- Pattern match on boolean values
data If 
  = If {
    ifCondition :: Term,
    ifReturnAs :: (Maybe ReturnAs),
    ifThen :: Term,
    ifElse :: Term}
  deriving (Eq, Ord, Read, Show)

_If = (Core.Name "hydra/ext/coq/syntax.If")

_If_condition = (Core.FieldName "condition")

_If_returnAs = (Core.FieldName "returnAs")

_If_then = (Core.FieldName "then")

_If_else = (Core.FieldName "else")

-- In the context of a function definition, these forms specify that name is an implicit argument.
data ImplicitBinders 
  = ImplicitBindersMaximallyInserted TypeBinders
  | ImplicitBindersNonMaximallyInserted TypeBinders
  deriving (Eq, Ord, Read, Show)

_ImplicitBinders = (Core.Name "hydra/ext/coq/syntax.ImplicitBinders")

_ImplicitBinders_maximallyInserted = (Core.FieldName "maximallyInserted")

_ImplicitBinders_nonMaximallyInserted = (Core.FieldName "nonMaximallyInserted")

-- A let-in definition
data Let 
  = Let {
    letBindings :: LetBindings,
    letIn :: Term}
  deriving (Eq, Ord, Read, Show)

_Let = (Core.Name "hydra/ext/coq/syntax.Let")

_Let_bindings = (Core.FieldName "bindings")

_Let_in = (Core.FieldName "in")

-- Some constructions allow the binding of a variable to value. This is called a “let-binder”.
data LetBinder 
  = LetBinder {
    letBinderName :: Name,
    letBinderType :: (Maybe Type),
    letBinderTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_LetBinder = (Core.Name "hydra/ext/coq/syntax.LetBinder")

_LetBinder_name = (Core.FieldName "name")

_LetBinder_type = (Core.FieldName "type")

_LetBinder_term = (Core.FieldName "term")

data LetBindings 
  = LetBindingsNamed LetNamed
  | LetBindingsDestructuring LetDestructuring
  deriving (Eq, Ord, Read, Show)

_LetBindings = (Core.Name "hydra/ext/coq/syntax.LetBindings")

_LetBindings_named = (Core.FieldName "named")

_LetBindings_destructuring = (Core.FieldName "destructuring")

data LetNamed 
  = LetNamed {
    letNamedBinder :: LetBinder,
    letNamedBinders :: [Binder]}
  deriving (Eq, Ord, Read, Show)

_LetNamed = (Core.Name "hydra/ext/coq/syntax.LetNamed")

_LetNamed_binder = (Core.FieldName "binder")

_LetNamed_binders = (Core.FieldName "binders")

data LetDestructuring 
  = LetDestructuringVariant1 LetDestructuringVariant1
  | LetDestructuringVariant2 LetDestructuringVariant2
  | LetDestructuringVariant3 LetDestructuringVariant3
  deriving (Eq, Ord, Read, Show)

_LetDestructuring = (Core.Name "hydra/ext/coq/syntax.LetDestructuring")

_LetDestructuring_variant1 = (Core.FieldName "variant1")

_LetDestructuring_variant2 = (Core.FieldName "variant2")

_LetDestructuring_variant3 = (Core.FieldName "variant3")

data LetDestructuringVariant1 
  = LetDestructuringVariant1 {
    letDestructuringVariant1Names :: [Name],
    letDestructuringVariant1ReturnAs :: (Maybe ReturnAs),
    letDestructuringVariant1Term :: Term}
  deriving (Eq, Ord, Read, Show)

_LetDestructuringVariant1 = (Core.Name "hydra/ext/coq/syntax.LetDestructuringVariant1")

_LetDestructuringVariant1_names = (Core.FieldName "names")

_LetDestructuringVariant1_returnAs = (Core.FieldName "returnAs")

_LetDestructuringVariant1_term = (Core.FieldName "term")

data LetDestructuringVariant2 
  = LetDestructuringVariant2 {
    letDestructuringVariant2Pattern :: Pattern,
    letDestructuringVariant2Term :: Term,
    letDestructuringVariant2Return :: (Maybe Term100)}
  deriving (Eq, Ord, Read, Show)

_LetDestructuringVariant2 = (Core.Name "hydra/ext/coq/syntax.LetDestructuringVariant2")

_LetDestructuringVariant2_pattern = (Core.FieldName "pattern")

_LetDestructuringVariant2_term = (Core.FieldName "term")

_LetDestructuringVariant2_return = (Core.FieldName "return")

data LetDestructuringVariant3 
  = LetDestructuringVariant3 {
    letDestructuringVariant3Pattern1 :: Pattern,
    letDestructuringVariant3Pattern2 :: Pattern,
    letDestructuringVariant3Term :: Term,
    letDestructuringVariant3Return :: Term100}
  deriving (Eq, Ord, Read, Show)

_LetDestructuringVariant3 = (Core.Name "hydra/ext/coq/syntax.LetDestructuringVariant3")

_LetDestructuringVariant3_pattern1 = (Core.FieldName "pattern1")

_LetDestructuringVariant3_pattern2 = (Core.FieldName "pattern2")

_LetDestructuringVariant3_term = (Core.FieldName "term")

_LetDestructuringVariant3_return = (Core.FieldName "return")

data Match 
  = Match {
    matchCaseItems :: [CaseItem],
    matchReturn :: (Maybe Term100),
    matchPipe :: Bool,
    matchEquations :: [Equation]}
  deriving (Eq, Ord, Read, Show)

_Match = (Core.Name "hydra/ext/coq/syntax.Match")

_Match_caseItems = (Core.FieldName "caseItems")

_Match_return = (Core.FieldName "return")

_Match_pipe = (Core.FieldName "pipe")

_Match_equations = (Core.FieldName "equations")

newtype Name 
  = Name {
    unName :: (Maybe Ident)}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/ext/coq/syntax.Name")

-- A non-negative arbitrary-precision integer
newtype Natural 
  = Natural {
    unNatural :: Integer}
  deriving (Eq, Ord, Read, Show)

_Natural = (Core.Name "hydra/ext/coq/syntax.Natural")

newtype Number 
  = Number {
    unNumber :: Double}
  deriving (Eq, Ord, Read, Show)

_Number = (Core.Name "hydra/ext/coq/syntax.Number")

data OneTerm 
  = OneTermExplicit QualidAnnotated
  | OneTermTerm1 Term1
  deriving (Eq, Ord, Read, Show)

_OneTerm = (Core.Name "hydra/ext/coq/syntax.OneTerm")

_OneTerm_explicit = (Core.FieldName "explicit")

_OneTerm_term1 = (Core.FieldName "term1")

data OpenBinders 
  = OpenBindersType TypeBinders
  | OpenBindersBinders [Binder]
  deriving (Eq, Ord, Read, Show)

_OpenBinders = (Core.Name "hydra/ext/coq/syntax.OpenBinders")

_OpenBinders_type = (Core.FieldName "type")

_OpenBinders_binders = (Core.FieldName "binders")

data Pattern 
  = PatternPattern Pattern10
  | PatternTerm (Maybe Term)
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/ext/coq/syntax.Pattern")

_Pattern_pattern = (Core.FieldName "pattern")

_Pattern_term = (Core.FieldName "term")

data Pattern0 
  = Pattern0Qualid Qualid
  | Pattern0QualIdAndPattern QualidAndPattern
  | Pattern0Placeholder 
  | Pattern0Parens [Pattern]
  | Pattern0Number Number
  | Pattern0String String
  deriving (Eq, Ord, Read, Show)

_Pattern0 = (Core.Name "hydra/ext/coq/syntax.Pattern0")

_Pattern0_qualid = (Core.FieldName "qualid")

_Pattern0_qualIdAndPattern = (Core.FieldName "qualIdAndPattern")

_Pattern0_placeholder = (Core.FieldName "placeholder")

_Pattern0_parens = (Core.FieldName "parens")

_Pattern0_number = (Core.FieldName "number")

_Pattern0_string = (Core.FieldName "string")

data Pattern1 
  = Pattern1 {
    pattern1Pattern :: Pattern0,
    pattern1Scope :: (Maybe ScopeKey)}
  deriving (Eq, Ord, Read, Show)

_Pattern1 = (Core.Name "hydra/ext/coq/syntax.Pattern1")

_Pattern1_pattern = (Core.FieldName "pattern")

_Pattern1_scope = (Core.FieldName "scope")

data Pattern10 
  = Pattern10As Pattern10As
  | Pattern10Patterns Pattern10Patterns
  | Pattern10Qualiid Pattern10Qualid
  deriving (Eq, Ord, Read, Show)

_Pattern10 = (Core.Name "hydra/ext/coq/syntax.Pattern10")

_Pattern10_as = (Core.FieldName "as")

_Pattern10_patterns = (Core.FieldName "patterns")

_Pattern10_qualiid = (Core.FieldName "qualiid")

data Pattern10As 
  = Pattern10As {
    pattern10AsPattern :: Pattern1,
    pattern10AsAs :: Name}
  deriving (Eq, Ord, Read, Show)

_Pattern10As = (Core.Name "hydra/ext/coq/syntax.Pattern10As")

_Pattern10As_pattern = (Core.FieldName "pattern")

_Pattern10As_as = (Core.FieldName "as")

data Pattern10Patterns 
  = Pattern10Patterns {
    pattern10PatternsPattern :: Pattern1,
    pattern10PatternsPatterns :: [Pattern1]}
  deriving (Eq, Ord, Read, Show)

_Pattern10Patterns = (Core.Name "hydra/ext/coq/syntax.Pattern10Patterns")

_Pattern10Patterns_pattern = (Core.FieldName "pattern")

_Pattern10Patterns_patterns = (Core.FieldName "patterns")

data Pattern10Qualid 
  = Pattern10Qualid {
    pattern10QualidQualid :: Qualid,
    pattern10QualidPatterns :: [Pattern1]}
  deriving (Eq, Ord, Read, Show)

_Pattern10Qualid = (Core.Name "hydra/ext/coq/syntax.Pattern10Qualid")

_Pattern10Qualid_qualid = (Core.FieldName "qualid")

_Pattern10Qualid_patterns = (Core.FieldName "patterns")

data PrimitiveNotations 
  = PrimitiveNotationsNumber Number
  | PrimitiveNotationsString String
  deriving (Eq, Ord, Read, Show)

_PrimitiveNotations = (Core.Name "hydra/ext/coq/syntax.PrimitiveNotations")

_PrimitiveNotations_number = (Core.FieldName "number")

_PrimitiveNotations_string = (Core.FieldName "string")

-- A qualified identifier
data Qualid 
  = Qualid {
    qualidId :: Ident,
    qualidFieldIds :: [FieldIdent]}
  deriving (Eq, Ord, Read, Show)

_Qualid = (Core.Name "hydra/ext/coq/syntax.Qualid")

_Qualid_id = (Core.FieldName "id")

_Qualid_fieldIds = (Core.FieldName "fieldIds")

data QualidAndPattern 
  = QualidAndPattern {
    qualidAndPatternQualid :: Qualid,
    qualidAndPatternPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_QualidAndPattern = (Core.Name "hydra/ext/coq/syntax.QualidAndPattern")

_QualidAndPattern_qualid = (Core.FieldName "qualid")

_QualidAndPattern_pattern = (Core.FieldName "pattern")

data QualidAnnotated 
  = QualidAnnotated {
    qualidAnnotatedQualid :: Qualid,
    qualidAnnotatedUnivAnnot :: (Maybe UnivAnnot)}
  deriving (Eq, Ord, Read, Show)

_QualidAnnotated = (Core.Name "hydra/ext/coq/syntax.QualidAnnotated")

_QualidAnnotated_qualid = (Core.FieldName "qualid")

_QualidAnnotated_univAnnot = (Core.FieldName "univAnnot")

data ReturnAs 
  = ReturnAs {
    returnAsAs :: (Maybe Name),
    returnAsReturn :: Term100}
  deriving (Eq, Ord, Read, Show)

_ReturnAs = (Core.Name "hydra/ext/coq/syntax.ReturnAs")

_ReturnAs_as = (Core.FieldName "as")

_ReturnAs_return = (Core.FieldName "return")

newtype ScopeKey 
  = ScopeKey {
    unScopeKey :: Ident}
  deriving (Eq, Ord, Read, Show)

_ScopeKey = (Core.Name "hydra/ext/coq/syntax.ScopeKey")

-- The types of types are called sorts.
data Sort 
  = SortSet 
  | SortProp 
  | SortSProp 
  | SortType 
  | SortTypeWithAnyUniverse 
  | SortTypeWithUniverse Universe
  deriving (Eq, Ord, Read, Show)

_Sort = (Core.Name "hydra/ext/coq/syntax.Sort")

_Sort_set = (Core.FieldName "set")

_Sort_prop = (Core.FieldName "prop")

_Sort_sProp = (Core.FieldName "sProp")

_Sort_type = (Core.FieldName "type")

_Sort_typeWithAnyUniverse = (Core.FieldName "typeWithAnyUniverse")

_Sort_typeWithUniverse = (Core.FieldName "typeWithUniverse")

newtype String_ 
  = String {
    unString :: String}
  deriving (Eq, Ord, Read, Show)

_String = (Core.Name "hydra/ext/coq/syntax.String")

data Term 
  = TermForallOrFun ForallOrFun
  | TermLet Let
  | TermIf If
  | TermFix Fix
  | TermCofix Cofix
  | TermTerm100 Term100
  deriving (Eq, Ord, Read, Show)

_Term = (Core.Name "hydra/ext/coq/syntax.Term")

_Term_forallOrFun = (Core.FieldName "forallOrFun")

_Term_let = (Core.FieldName "let")

_Term_if = (Core.FieldName "if")

_Term_fix = (Core.FieldName "fix")

_Term_cofix = (Core.FieldName "cofix")

_Term_term100 = (Core.FieldName "term100")

data Term0 
  = Term0QualidAnnotated QualidAnnotated
  | Term0Sort Sort
  | Term0PrimitiveNotations PrimitiveNotations
  | Term0Evar ExistentialVariable
  | Term0Match Match
  | Term0Record 
  | Term0Generalizing 
  | Term0Ltac 
  | Term0Parens Term
  deriving (Eq, Ord, Read, Show)

_Term0 = (Core.Name "hydra/ext/coq/syntax.Term0")

_Term0_qualidAnnotated = (Core.FieldName "qualidAnnotated")

_Term0_sort = (Core.FieldName "sort")

_Term0_primitiveNotations = (Core.FieldName "primitiveNotations")

_Term0_evar = (Core.FieldName "evar")

_Term0_match = (Core.FieldName "match")

_Term0_record = (Core.FieldName "record")

_Term0_generalizing = (Core.FieldName "generalizing")

_Term0_ltac = (Core.FieldName "ltac")

_Term0_parens = (Core.FieldName "parens")

data Term1 
  = Term1Projection 
  | Term1Scope 
  | Term1Term0 Term0
  deriving (Eq, Ord, Read, Show)

_Term1 = (Core.Name "hydra/ext/coq/syntax.Term1")

_Term1_projection = (Core.FieldName "projection")

_Term1_scope = (Core.FieldName "scope")

_Term1_term0 = (Core.FieldName "term0")

data Term10 
  = Term10Application Application
  | Term10OneTerm OneTerm
  deriving (Eq, Ord, Read, Show)

_Term10 = (Core.Name "hydra/ext/coq/syntax.Term10")

_Term10_application = (Core.FieldName "application")

_Term10_oneTerm = (Core.FieldName "oneTerm")

data Term100 
  = Term100Cast TypeCast
  | Term100Term10 Term10
  deriving (Eq, Ord, Read, Show)

_Term100 = (Core.Name "hydra/ext/coq/syntax.Term100")

_Term100_cast = (Core.FieldName "cast")

_Term100_term10 = (Core.FieldName "term10")

newtype Type 
  = Type {
    unType :: Term}
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/ext/coq/syntax.Type")

data TypeCast 
  = TypeCast {
    typeCastTerm :: Term10,
    typeCastType :: Type,
    typeCastOperator :: TypeCastOperator}
  deriving (Eq, Ord, Read, Show)

_TypeCast = (Core.Name "hydra/ext/coq/syntax.TypeCast")

_TypeCast_term = (Core.FieldName "term")

_TypeCast_type = (Core.FieldName "type")

_TypeCast_operator = (Core.FieldName "operator")

data TypeCastOperator 
  = TypeCastOperatorNormal 
  | TypeCastOperatorVmCompute 
  | TypeCastOperatorNativeCompute 
  deriving (Eq, Ord, Read, Show)

_TypeCastOperator = (Core.Name "hydra/ext/coq/syntax.TypeCastOperator")

_TypeCastOperator_normal = (Core.FieldName "normal")

_TypeCastOperator_vmCompute = (Core.FieldName "vmCompute")

_TypeCastOperator_nativeCompute = (Core.FieldName "nativeCompute")

data TypeBinders 
  = TypeBinders {
    typeBindersNames :: [Name],
    typeBindersType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeBinders = (Core.Name "hydra/ext/coq/syntax.TypeBinders")

_TypeBinders_names = (Core.FieldName "names")

_TypeBinders_type = (Core.FieldName "type")

data TypeclassConstraint 
  = TypeclassConstraint {
    typeclassConstraintName :: (Maybe Name),
    typeclassConstraintGeneralizing :: Bool,
    typeclassConstraintTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_TypeclassConstraint = (Core.Name "hydra/ext/coq/syntax.TypeclassConstraint")

_TypeclassConstraint_name = (Core.FieldName "name")

_TypeclassConstraint_generalizing = (Core.FieldName "generalizing")

_TypeclassConstraint_term = (Core.FieldName "term")

newtype UnivAnnot 
  = UnivAnnot {
    unUnivAnnot :: [UniverseLevel]}
  deriving (Eq, Ord, Read, Show)

_UnivAnnot = (Core.Name "hydra/ext/coq/syntax.UnivAnnot")

data Universe 
  = UniverseMax [UniverseExpr]
  | UniverseExpr UniverseExpr
  deriving (Eq, Ord, Read, Show)

_Universe = (Core.Name "hydra/ext/coq/syntax.Universe")

_Universe_max = (Core.FieldName "max")

_Universe_expr = (Core.FieldName "expr")

data UniverseExpr 
  = UniverseExpr {
    universeExprName :: UniverseName,
    universeExprNumber :: (Maybe Natural)}
  deriving (Eq, Ord, Read, Show)

_UniverseExpr = (Core.Name "hydra/ext/coq/syntax.UniverseExpr")

_UniverseExpr_name = (Core.FieldName "name")

_UniverseExpr_number = (Core.FieldName "number")

data UniverseLevel 
  = UniverseLevelSet 
  | UniverseLevelProp 
  | UniverseLevelType 
  | UniverseLevelIgnored 
  | UniverseLevelQualid Qualid
  deriving (Eq, Ord, Read, Show)

_UniverseLevel = (Core.Name "hydra/ext/coq/syntax.UniverseLevel")

_UniverseLevel_set = (Core.FieldName "set")

_UniverseLevel_prop = (Core.FieldName "prop")

_UniverseLevel_type = (Core.FieldName "type")

_UniverseLevel_ignored = (Core.FieldName "ignored")

_UniverseLevel_qualid = (Core.FieldName "qualid")

data UniverseName 
  = UniverseNameQualid Qualid
  | UniverseNameSet 
  | UniverseNameProp 
  deriving (Eq, Ord, Read, Show)

_UniverseName = (Core.Name "hydra/ext/coq/syntax.UniverseName")

_UniverseName_qualid = (Core.FieldName "qualid")

_UniverseName_set = (Core.FieldName "set")

_UniverseName_prop = (Core.FieldName "prop")