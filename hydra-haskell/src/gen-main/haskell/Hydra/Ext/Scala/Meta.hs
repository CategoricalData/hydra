{-# LANGUAGE DeriveGeneric #-}
module Hydra.Ext.Scala.Meta
  ( Case(..)
  , CaseTree(..)
  , Ctor_Primary(..)
  , Ctor_Secondary(..)
  , Ctor(..)
  , Decl_Val(..)
  , Decl_Var(..)
  , Decl_Def(..)
  , Decl_Type(..)
  , Decl_Given(..)
  , Decl(..)
  , Defn_Val(..)
  , Defn_Var(..)
  , Defn_Given(..)
  , Defn_Enum(..)
  , Defn_EnumCase(..)
  , Defn_RepeatedEnumCase(..)
  , Defn_GivenAlias(..)
  , Defn_ExtensionGroup(..)
  , Defn_Def(..)
  , Defn_Macro(..)
  , Defn_Type(..)
  , Defn_Class(..)
  , Defn_Trait(..)
  , Defn_Object(..)
  , Defn(..)
  , Enumerator_Generator(..)
  , Enumerator_CaseGenerator(..)
  , Enumerator_Val(..)
  , Enumerator_Guard(..)
  , Enumerator(..)
  , Export(..)
  , Import(..)
  , ImportExportStat(..)
  , Importee_Given(..)
  , Importee_Name(..)
  , Importee_Rename(..)
  , Importee_Unimport(..)
  , Importee(..)
  , Importer(..)
  , Init(..)
  , Lit(..)
  , Member_Term(..)
  , Member_Type(..)
  , Member(..)
  , Mod_Annot(..)
  , Mod_Private(..)
  , Mod_Protected(..)
  , Mod(..)
  , Name(..)
  , Pat_Var(..)
  , Pat_Bind(..)
  , Pat_Alternative(..)
  , Pat_Tuple(..)
  , Pat_Repeated(..)
  , Pat_Extract(..)
  , Pat_ExtractInfix(..)
  , Pat_Interpolate(..)
  , Pat_Xml(..)
  , Pat_Typed(..)
  , Pat_Macro(..)
  , Pat_Given(..)
  , Pat(..)
  , Pkg_Object(..)
  , Pkg(..)
  , PredefString
  , Quasi
  , Ref(..)
  , ScalaSymbol(..)
  , Self
  , Source(..)
  , Stat(..)
  , Template(..)
  , Term_Ref(..)
  , Term_This
  , Term_Super(..)
  , Term_Name(..)
  , Term_Anonymous
  , Term_Select(..)
  , Term_Interpolate(..)
  , Term_Xml(..)
  , Term_Apply(..)
  , Term_ApplyUsing(..)
  , Term_ApplyType(..)
  , Term_ApplyInfix(..)
  , Term_ApplyUnary(..)
  , Term_Assign(..)
  , Term_Return(..)
  , Term_Throw(..)
  , Term_Ascribe(..)
  , Term_Annotate(..)
  , Term_Tuple(..)
  , Term_Block(..)
  , Term_EndMarker(..)
  , Term_If(..)
  , Term_QuotedMacroExpr(..)
  , Term_QuotedMacroType(..)
  , Term_SplicedMacroExpr(..)
  , Term_Match(..)
  , Term_Try(..)
  , Term_TryWithHandler(..)
  , Term_FunctionTerm(..)
  , Term_ContextFunction(..)
  , Term_Function(..)
  , Term_PolyFunction(..)
  , Term_PartialFunction(..)
  , Term_While(..)
  , Term_Do(..)
  , Term_For(..)
  , Term_ForYield(..)
  , Term_New(..)
  , Term_NewAnonymous(..)
  , Term_Placeholder
  , Term_Eta(..)
  , Term_Repeated(..)
  , Term_Param(..)
  , Term(..)
  , Tree(..)
  , Type_Ref(..)
  , Type_Name(..)
  , Type_AnonymousName
  , Type_Select(..)
  , Type_Project(..)
  , Type_Singleton(..)
  , Type_Apply(..)
  , Type_ApplyInfix(..)
  , Type_FunctionType(..)
  , Type_Function(..)
  , Type_PolyFunction(..)
  , Type_ContextFunction(..)
  , Type_ImplicitFunction(..)
  , Type_Tuple(..)
  , Type_With(..)
  , Type_And(..)
  , Type_Or(..)
  , Type_Refine(..)
  , Type_Existential(..)
  , Type_Annotate(..)
  , Type_Lambda(..)
  , Type_Macro(..)
  , Type_Method(..)
  , Type_Placeholder(..)
  , Type_Bounds(..)
  , Type_ByName
  , Type_Repeated(..)
  , Type_Var(..)
  , Type_TypedParam(..)
  , Type_Param(..)
  , Type_Match(..)
  , Type(..)
  , TypeCase(..)
  , _Case
  , _CaseTree
  , _CaseTree_case
  , _CaseTree_typeCase
  , _Case_body
  , _Case_cond
  , _Case_pat
  , _Ctor
  , _Ctor_Primary
  , _Ctor_Primary_mods
  , _Ctor_Primary_name
  , _Ctor_Primary_paramss
  , _Ctor_Secondary
  , _Ctor_Secondary_init
  , _Ctor_Secondary_mods
  , _Ctor_Secondary_name
  , _Ctor_Secondary_paramss
  , _Ctor_Secondary_stats
  , _Ctor_primary
  , _Ctor_secondary
  , _Decl
  , _Decl_Def
  , _Decl_Def_decltpe
  , _Decl_Def_mods
  , _Decl_Def_name
  , _Decl_Def_paramss
  , _Decl_Def_tparams
  , _Decl_Given
  , _Decl_Given_decltpe
  , _Decl_Given_mods
  , _Decl_Given_name
  , _Decl_Given_sparams
  , _Decl_Given_tparams
  , _Decl_Type
  , _Decl_Type_bounds
  , _Decl_Type_mods
  , _Decl_Type_name
  , _Decl_Type_tparams
  , _Decl_Val
  , _Decl_Val_decltpe
  , _Decl_Val_mods
  , _Decl_Val_pats
  , _Decl_Var
  , _Decl_Var_decltpe
  , _Decl_Var_mods
  , _Decl_Var_pats
  , _Decl_def
  , _Decl_given
  , _Decl_type
  , _Decl_val
  , _Decl_var
  , _Defn
  , _Defn_Class
  , _Defn_Class_ctor
  , _Defn_Class_mods
  , _Defn_Class_name
  , _Defn_Class_template
  , _Defn_Class_tparams
  , _Defn_Def
  , _Defn_Def_body
  , _Defn_Def_decltpe
  , _Defn_Def_mods
  , _Defn_Def_name
  , _Defn_Def_paramss
  , _Defn_Def_tparams
  , _Defn_Enum
  , _Defn_EnumCase
  , _Defn_EnumCase_ctor
  , _Defn_EnumCase_inits
  , _Defn_EnumCase_mods
  , _Defn_EnumCase_name
  , _Defn_EnumCase_tparams
  , _Defn_Enum_ctor
  , _Defn_Enum_mods
  , _Defn_Enum_name
  , _Defn_Enum_template
  , _Defn_Enum_tparams
  , _Defn_ExtensionGroup
  , _Defn_ExtensionGroup_body
  , _Defn_ExtensionGroup_paramss
  , _Defn_ExtensionGroup_tparams
  , _Defn_Given
  , _Defn_GivenAlias
  , _Defn_GivenAlias_body
  , _Defn_GivenAlias_decltpe
  , _Defn_GivenAlias_mods
  , _Defn_GivenAlias_name
  , _Defn_GivenAlias_sparams
  , _Defn_GivenAlias_tparams
  , _Defn_Given_mods
  , _Defn_Given_name
  , _Defn_Given_sparams
  , _Defn_Given_templ
  , _Defn_Given_tparams
  , _Defn_Macro
  , _Defn_Macro_body
  , _Defn_Macro_decltpe
  , _Defn_Macro_mods
  , _Defn_Macro_name
  , _Defn_Macro_paramss
  , _Defn_Macro_tparams
  , _Defn_Object
  , _Defn_Object_name
  , _Defn_RepeatedEnumCase
  , _Defn_RepeatedEnumCase_cases
  , _Defn_RepeatedEnumCase_mods
  , _Defn_Trait
  , _Defn_Trait_ctor
  , _Defn_Trait_mods
  , _Defn_Trait_name
  , _Defn_Trait_template
  , _Defn_Trait_tparams
  , _Defn_Type
  , _Defn_Type_body
  , _Defn_Type_mods
  , _Defn_Type_name
  , _Defn_Type_tparams
  , _Defn_Val
  , _Defn_Val_decltpe
  , _Defn_Val_mods
  , _Defn_Val_pats
  , _Defn_Val_rhs
  , _Defn_Var
  , _Defn_Var_decltpe
  , _Defn_Var_mods
  , _Defn_Var_pats
  , _Defn_Var_rhs
  , _Defn_class
  , _Defn_def
  , _Defn_enum
  , _Defn_enumCase
  , _Defn_extensionGroup
  , _Defn_given
  , _Defn_givenAlias
  , _Defn_macro
  , _Defn_object
  , _Defn_repeatedEnumCase
  , _Defn_trait
  , _Defn_type
  , _Defn_val
  , _Defn_var
  , _Enumerator
  , _Enumerator_CaseGenerator
  , _Enumerator_CaseGenerator_pat
  , _Enumerator_CaseGenerator_rhs
  , _Enumerator_Generator
  , _Enumerator_Generator_pat
  , _Enumerator_Generator_rhs
  , _Enumerator_Guard
  , _Enumerator_Guard_cond
  , _Enumerator_Val
  , _Enumerator_Val_pat
  , _Enumerator_Val_rhs
  , _Enumerator_caseGenerator
  , _Enumerator_generator
  , _Enumerator_guard
  , _Enumerator_val
  , _Export
  , _Export_importers
  , _Import
  , _ImportExportStat
  , _ImportExportStat_export
  , _ImportExportStat_import
  , _Import_importers
  , _Importee
  , _Importee_Given
  , _Importee_Given_tpe
  , _Importee_Name
  , _Importee_Name_name
  , _Importee_Rename
  , _Importee_Rename_name
  , _Importee_Rename_rename
  , _Importee_Unimport
  , _Importee_Unimport_name
  , _Importee_given
  , _Importee_givenAll
  , _Importee_name
  , _Importee_rename
  , _Importee_unimport
  , _Importee_wildcard
  , _Importer
  , _Importer_importees
  , _Importer_ref
  , _Init
  , _Init_argss
  , _Init_name
  , _Init_tpe
  , _Lit
  , _Lit_boolean
  , _Lit_byte
  , _Lit_char
  , _Lit_double
  , _Lit_float
  , _Lit_int
  , _Lit_long
  , _Lit_null
  , _Lit_short
  , _Lit_string
  , _Lit_symbol
  , _Lit_unit
  , _Member
  , _Member_Term
  , _Member_Term_object
  , _Member_Term_pkg
  , _Member_Type
  , _Member_Type_name
  , _Member_self
  , _Member_term
  , _Member_termParam
  , _Member_type
  , _Member_typeParam
  , _Mod
  , _Mod_Annot
  , _Mod_Annot_init
  , _Mod_Private
  , _Mod_Private_within
  , _Mod_Protected
  , _Mod_Protected_within
  , _Mod_abstract
  , _Mod_annot
  , _Mod_case
  , _Mod_contravariant
  , _Mod_covariant
  , _Mod_final
  , _Mod_implicit
  , _Mod_infix
  , _Mod_inline
  , _Mod_lazy
  , _Mod_opaque
  , _Mod_open
  , _Mod_override
  , _Mod_private
  , _Mod_protected
  , _Mod_sealed
  , _Mod_super
  , _Mod_transparent
  , _Mod_using
  , _Mod_valParam
  , _Mod_varParam
  , _Name
  , _Name_anonymous
  , _Name_indeterminate
  , _Name_value
  , _Pat
  , _Pat_Alternative
  , _Pat_Alternative_lhs
  , _Pat_Alternative_rhs
  , _Pat_Bind
  , _Pat_Bind_lhs
  , _Pat_Bind_rhs
  , _Pat_Extract
  , _Pat_ExtractInfix
  , _Pat_ExtractInfix_lhs
  , _Pat_ExtractInfix_op
  , _Pat_ExtractInfix_rhs
  , _Pat_Extract_args
  , _Pat_Extract_fun
  , _Pat_Given
  , _Pat_Given_tpe
  , _Pat_Interpolate
  , _Pat_Interpolate_parts
  , _Pat_Interpolate_prefix
  , _Pat_Macro
  , _Pat_Macro_body
  , _Pat_Repeated
  , _Pat_Repeated_name
  , _Pat_Tuple
  , _Pat_Tuple_args
  , _Pat_Typed
  , _Pat_Typed_lhs
  , _Pat_Typed_rhs
  , _Pat_Var
  , _Pat_Var_name
  , _Pat_Xml
  , _Pat_Xml_args
  , _Pat_Xml_parts
  , _Pat_alternative
  , _Pat_bind
  , _Pat_extract
  , _Pat_extractInfix
  , _Pat_given
  , _Pat_interpolate
  , _Pat_macro
  , _Pat_repeated
  , _Pat_seqWildcard
  , _Pat_tuple
  , _Pat_typed
  , _Pat_var
  , _Pat_wildcard
  , _Pat_xml
  , _Pkg
  , _Pkg_Object
  , _Pkg_Object_mods
  , _Pkg_Object_name
  , _Pkg_Object_template
  , _Pkg_name
  , _Pkg_ref
  , _Pkg_stats
  , _PredefString
  , _Quasi
  , _Ref
  , _Ref_init
  , _Ref_name
  , _ScalaSymbol
  , _ScalaSymbol_name
  , _Self
  , _Source
  , _Source_stats
  , _Stat
  , _Stat_decl
  , _Stat_defn
  , _Stat_importExportStat
  , _Stat_term
  , _Template
  , _Template_early
  , _Template_inits
  , _Template_self
  , _Template_stats
  , _Term
  , _Term_Annotate
  , _Term_Annotate_annots
  , _Term_Annotate_expr
  , _Term_Anonymous
  , _Term_Apply
  , _Term_ApplyInfix
  , _Term_ApplyInfix_args
  , _Term_ApplyInfix_lhs
  , _Term_ApplyInfix_op
  , _Term_ApplyInfix_targs
  , _Term_ApplyType
  , _Term_ApplyType_args
  , _Term_ApplyType_lhs
  , _Term_ApplyType_op
  , _Term_ApplyType_targs
  , _Term_ApplyUnary
  , _Term_ApplyUnary_arg
  , _Term_ApplyUnary_op
  , _Term_ApplyUsing
  , _Term_ApplyUsing_fun
  , _Term_ApplyUsing_targs
  , _Term_Apply_args
  , _Term_Apply_fun
  , _Term_Ascribe
  , _Term_Ascribe_expr
  , _Term_Ascribe_tpe
  , _Term_Assign
  , _Term_Assign_lhs
  , _Term_Assign_rhs
  , _Term_Block
  , _Term_Block_stats
  , _Term_ContextFunction
  , _Term_ContextFunction_body
  , _Term_ContextFunction_params
  , _Term_Do
  , _Term_Do_body
  , _Term_Do_expr
  , _Term_EndMarker
  , _Term_EndMarker_name
  , _Term_Eta
  , _Term_Eta_expr
  , _Term_For
  , _Term_ForYield
  , _Term_ForYield_enums
  , _Term_For_enums
  , _Term_Function
  , _Term_FunctionTerm
  , _Term_FunctionTerm_Function
  , _Term_FunctionTerm_contextFunction
  , _Term_Function_body
  , _Term_Function_params
  , _Term_If
  , _Term_If_cond
  , _Term_If_elsep
  , _Term_If_thenp
  , _Term_Interpolate
  , _Term_Interpolate_args
  , _Term_Interpolate_parts
  , _Term_Interpolate_prefix
  , _Term_Match
  , _Term_Match_cases
  , _Term_Match_expr
  , _Term_Name
  , _Term_Name_value
  , _Term_New
  , _Term_NewAnonymous
  , _Term_NewAnonymous_templ
  , _Term_New_init
  , _Term_Param
  , _Term_Param_mods
  , _Term_Param_name
  , _Term_PartialFunction
  , _Term_PartialFunction_cases
  , _Term_Placeholder
  , _Term_PolyFunction
  , _Term_PolyFunction_body
  , _Term_PolyFunction_tparams
  , _Term_QuotedMacroExpr
  , _Term_QuotedMacroExpr_body
  , _Term_QuotedMacroType
  , _Term_QuotedMacroType_tpe
  , _Term_Ref
  , _Term_Ref_anonymous
  , _Term_Ref_applyUnary
  , _Term_Ref_name
  , _Term_Ref_select
  , _Term_Ref_super
  , _Term_Ref_this
  , _Term_Repeated
  , _Term_Repeated_expr
  , _Term_Return
  , _Term_Return_expr
  , _Term_Select
  , _Term_Select_name
  , _Term_Select_qual
  , _Term_SplicedMacroExpr
  , _Term_SplicedMacroExpr_body
  , _Term_Super
  , _Term_Super_superp
  , _Term_Super_thisp
  , _Term_This
  , _Term_Throw
  , _Term_Throw_expr
  , _Term_Try
  , _Term_TryWithHandler
  , _Term_TryWithHandler_catchp
  , _Term_TryWithHandler_expr
  , _Term_TryWithHandler_finallyp
  , _Term_Try_catchp
  , _Term_Try_expr
  , _Term_Try_finallyp
  , _Term_Tuple
  , _Term_Tuple_args
  , _Term_While
  , _Term_While_body
  , _Term_While_expr
  , _Term_Xml
  , _Term_Xml_args
  , _Term_Xml_parts
  , _Term_annotate
  , _Term_apply
  , _Term_applyInfix
  , _Term_applyType
  , _Term_applyUsing
  , _Term_ascribe
  , _Term_assign
  , _Term_block
  , _Term_do
  , _Term_endMarker
  , _Term_eta
  , _Term_for
  , _Term_forYield
  , _Term_functionTerm
  , _Term_if
  , _Term_interpolate
  , _Term_lit
  , _Term_match
  , _Term_new
  , _Term_newAnonymous
  , _Term_partialFunction
  , _Term_placeholder
  , _Term_polyFunction
  , _Term_quotedMacroExpr
  , _Term_quotedMacroType
  , _Term_ref
  , _Term_repeated
  , _Term_return
  , _Term_splicedMacroExpr
  , _Term_throw
  , _Term_try
  , _Term_tryWithHandler
  , _Term_tuple
  , _Term_while
  , _Term_xml
  , _Tree
  , _Tree_bounds
  , _Tree_caseTree
  , _Tree_ctor
  , _Tree_enumerator
  , _Tree_importee
  , _Tree_importer
  , _Tree_member
  , _Tree_mod
  , _Tree_pat
  , _Tree_quasi
  , _Tree_ref
  , _Tree_source
  , _Tree_stat
  , _Tree_template
  , _Tree_type
  , _Type
  , _TypeCase
  , _TypeCase_body
  , _TypeCase_pat
  , _Type_And
  , _Type_And_lhs
  , _Type_And_rhs
  , _Type_Annotate
  , _Type_Annotate_annots
  , _Type_Annotate_tpe
  , _Type_AnonymousName
  , _Type_Apply
  , _Type_ApplyInfix
  , _Type_ApplyInfix_lhs
  , _Type_ApplyInfix_op
  , _Type_ApplyInfix_rhs
  , _Type_Apply_args
  , _Type_Apply_tpe
  , _Type_Bounds
  , _Type_Bounds_hi
  , _Type_Bounds_lo
  , _Type_ByName
  , _Type_ContextFunction
  , _Type_ContextFunction_params
  , _Type_ContextFunction_res
  , _Type_Existential
  , _Type_Existential_stats
  , _Type_Existential_tpe
  , _Type_Function
  , _Type_FunctionType
  , _Type_FunctionType_contextFunction
  , _Type_FunctionType_function
  , _Type_Function_params
  , _Type_Function_res
  , _Type_ImplicitFunction
  , _Type_ImplicitFunction_params
  , _Type_ImplicitFunction_res
  , _Type_Lambda
  , _Type_Lambda_tparams
  , _Type_Lambda_tpe
  , _Type_Macro
  , _Type_Macro_body
  , _Type_Match
  , _Type_Match_cases
  , _Type_Match_tpe
  , _Type_Method
  , _Type_Method_paramss
  , _Type_Method_tpe
  , _Type_Name
  , _Type_Name_value
  , _Type_Or
  , _Type_Or_lhs
  , _Type_Or_rhs
  , _Type_Param
  , _Type_Param_cbounds
  , _Type_Param_mods
  , _Type_Param_name
  , _Type_Param_tbounds
  , _Type_Param_tparams
  , _Type_Param_vbounds
  , _Type_Placeholder
  , _Type_Placeholder_bounds
  , _Type_PolyFunction
  , _Type_PolyFunction_tparams
  , _Type_PolyFunction_tpe
  , _Type_Project
  , _Type_Project_name
  , _Type_Project_qual
  , _Type_Ref
  , _Type_Ref_name
  , _Type_Ref_project
  , _Type_Ref_select
  , _Type_Ref_singleton
  , _Type_Refine
  , _Type_Refine_stats
  , _Type_Refine_tpe
  , _Type_Repeated
  , _Type_Repeated_tpe
  , _Type_Select
  , _Type_Select_name
  , _Type_Select_qual
  , _Type_Singleton
  , _Type_Singleton_ref
  , _Type_Tuple
  , _Type_Tuple_args
  , _Type_TypedParam
  , _Type_TypedParam_name
  , _Type_TypedParam_typ
  , _Type_Var
  , _Type_Var_name
  , _Type_With
  , _Type_With_lhs
  , _Type_With_rhs
  , _Type_and
  , _Type_annotate
  , _Type_anonymousName
  , _Type_apply
  , _Type_applyInfix
  , _Type_byName
  , _Type_existential
  , _Type_functionType
  , _Type_implicitFunction
  , _Type_lambda
  , _Type_macro
  , _Type_match
  , _Type_method
  , _Type_or
  , _Type_placeholder
  , _Type_polyFunction
  , _Type_ref
  , _Type_refine
  , _Type_repeated
  , _Type_tuple
  , _Type_typedParam
  , _Type_var
  , _Type_with
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

data Case
  = Case
    -- | @type hydra/ext/scala/meta.Pat
    { casePat :: Pat
    -- | @type optional: hydra/ext/scala/meta.Term
    , caseCond :: Maybe Term
    -- | @type hydra/ext/scala/meta.Term
    , caseBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data CaseTree
  -- | @type hydra/ext/scala/meta.Case
  = CaseTreeCase Case
  -- | @type hydra/ext/scala/meta.TypeCase
  | CaseTreeTypeCase TypeCase deriving (Eq, Generic, Ord, Read, Show)

data Ctor_Primary
  = Ctor_Primary
    -- | @type list: hydra/ext/scala/meta.Mod
    { ctorPrimaryMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Name
    , ctorPrimaryName :: Name
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , ctorPrimaryParamss :: [[Term_Param]] } deriving (Eq, Generic, Ord, Read, Show)

data Ctor_Secondary
  = Ctor_Secondary
    -- | @type list: hydra/ext/scala/meta.Mod
    { ctorSecondaryMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Name
    , ctorSecondaryName :: Name
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , ctorSecondaryParamss :: [[Term_Param]]
    -- | @type hydra/ext/scala/meta.Init
    , ctorSecondaryInit :: Init
    -- | @type list: hydra/ext/scala/meta.Stat
    , ctorSecondaryStats :: [Stat] } deriving (Eq, Generic, Ord, Read, Show)

data Ctor
  -- | @type hydra/ext/scala/meta.Ctor.Primary
  = CtorPrimary Ctor_Primary
  -- | @type hydra/ext/scala/meta.Ctor.Secondary
  | CtorSecondary Ctor_Secondary deriving (Eq, Generic, Ord, Read, Show)

data Decl_Val
  = Decl_Val
    -- | @type list: hydra/ext/scala/meta.Mod
    { declValMods :: [Mod]
    -- | @type list: hydra/ext/scala/meta.Pat
    , declValPats :: [Pat]
    -- | @type hydra/ext/scala/meta.Type
    , declValDecltpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Decl_Var
  = Decl_Var
    -- | @type list: hydra/ext/scala/meta.Mod
    { declVarMods :: [Mod]
    -- | @type list: hydra/ext/scala/meta.Pat
    , declVarPats :: [Pat]
    -- | @type hydra/ext/scala/meta.Type
    , declVarDecltpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Decl_Def
  = Decl_Def
    -- | @type list: hydra/ext/scala/meta.Mod
    { declDefMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Term.Name
    , declDefName :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , declDefTparams :: [Type_Param]
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , declDefParamss :: [[Term_Param]]
    -- | @type hydra/ext/scala/meta.Type
    , declDefDecltpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Decl_Type
  = Decl_Type
    -- | @type list: hydra/ext/scala/meta.Mod
    { declTypeMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Type.Name
    , declTypeName :: Type_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , declTypeTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Type.Bounds
    , declTypeBounds :: Type_Bounds } deriving (Eq, Generic, Ord, Read, Show)

data Decl_Given
  = Decl_Given
    -- | @type list: hydra/ext/scala/meta.Mod
    { declGivenMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Term.Name
    , declGivenName :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , declGivenTparams :: [Type_Param]
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , declGivenSparams :: [[Term_Param]]
    -- | @type hydra/ext/scala/meta.Type
    , declGivenDecltpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Decl
  -- | @type hydra/ext/scala/meta.Decl.Val
  = DeclVal Decl_Val
  -- | @type hydra/ext/scala/meta.Decl.Var
  | DeclVar Decl_Var
  -- | @type hydra/ext/scala/meta.Decl.Def
  | DeclDef Decl_Def
  -- | @type hydra/ext/scala/meta.Decl.Type
  | DeclType Decl_Type
  -- | @type hydra/ext/scala/meta.Decl.Given
  | DeclGiven Decl_Given deriving (Eq, Generic, Ord, Read, Show)

data Defn_Val
  = Defn_Val
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnValMods :: [Mod]
    -- | @type list: hydra/ext/scala/meta.Pat
    , defnValPats :: [Pat]
    -- | @type optional: hydra/ext/scala/meta.Type
    , defnValDecltpe :: Maybe Type
    -- | @type hydra/ext/scala/meta.Term
    , defnValRhs :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Var
  = Defn_Var
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnVarMods :: [Mod]
    -- | @type list: hydra/ext/scala/meta.Pat
    , defnVarPats :: [Pat]
    -- | @type hydra/ext/scala/meta.Type
    , defnVarDecltpe :: Type
    -- | @type optional: hydra/ext/scala/meta.Term
    , defnVarRhs :: Maybe Term } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Given
  = Defn_Given
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnGivenMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Name
    , defnGivenName :: Name
    {-| @type list:
                list: hydra/ext/scala/meta.Type.Param -}
    , defnGivenTparams :: [[Type_Param]]
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , defnGivenSparams :: [[Term_Param]]
    -- | @type hydra/ext/scala/meta.Template
    , defnGivenTempl :: Template } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Enum
  = Defn_Enum
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnEnumMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Type.Name
    , defnEnumName :: Type_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , defnEnumTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Ctor.Primary
    , defnEnumCtor :: Ctor_Primary
    -- | @type hydra/ext/scala/meta.Template
    , defnEnumTemplate :: Template } deriving (Eq, Generic, Ord, Read, Show)

data Defn_EnumCase
  = Defn_EnumCase
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnEnumCaseMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Term.Name
    , defnEnumCaseName :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , defnEnumCaseTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Ctor.Primary
    , defnEnumCaseCtor :: Ctor_Primary
    -- | @type list: hydra/ext/scala/meta.Init
    , defnEnumCaseInits :: [Init] } deriving (Eq, Generic, Ord, Read, Show)

data Defn_RepeatedEnumCase
  = Defn_RepeatedEnumCase
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnRepeatedEnumCaseMods :: [Mod]
    -- | @type list: hydra/ext/scala/meta.Term.Name
    , defnRepeatedEnumCaseCases :: [Term_Name] } deriving (Eq, Generic, Ord, Read, Show)

data Defn_GivenAlias
  = Defn_GivenAlias
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnGivenAliasMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Name
    , defnGivenAliasName :: Name
    {-| @type list:
                list: hydra/ext/scala/meta.Type.Param -}
    , defnGivenAliasTparams :: [[Type_Param]]
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , defnGivenAliasSparams :: [[Term_Param]]
    -- | @type hydra/ext/scala/meta.Type
    , defnGivenAliasDecltpe :: Type
    -- | @type hydra/ext/scala/meta.Term
    , defnGivenAliasBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Defn_ExtensionGroup
  = Defn_ExtensionGroup
    -- | @type list: hydra/ext/scala/meta.Type.Param
    { defnExtensionGroupTparams :: [Type_Param]
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , defnExtensionGroupParamss :: [[Term_Param]]
    -- | @type hydra/ext/scala/meta.Stat
    , defnExtensionGroupBody :: Stat } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Def
  = Defn_Def
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnDefMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Term.Name
    , defnDefName :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , defnDefTparams :: [Type_Param]
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , defnDefParamss :: [[Term_Param]]
    , defnDefDecltpe :: ()
    -- | @type hydra/ext/scala/meta.Term
    , defnDefBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Macro
  = Defn_Macro
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnMacroMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Term.Name
    , defnMacroName :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , defnMacroTparams :: [Type_Param]
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    , defnMacroParamss :: [[Term_Param]]
    -- | @type optional: hydra/ext/scala/meta.Type
    , defnMacroDecltpe :: Maybe Type
    -- | @type hydra/ext/scala/meta.Term
    , defnMacroBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Type
  = Defn_Type
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnTypeMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Type.Name
    , defnTypeName :: Type_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , defnTypeTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Type
    , defnTypeBody :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Class
  = Defn_Class
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnClassMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Type.Name
    , defnClassName :: Type_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , defnClassTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Ctor.Primary
    , defnClassCtor :: Ctor_Primary
    -- | @type hydra/ext/scala/meta.Template
    , defnClassTemplate :: Template } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Trait
  = Defn_Trait
    -- | @type list: hydra/ext/scala/meta.Mod
    { defnTraitMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Type.Name
    , defnTraitName :: Type_Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , defnTraitTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Ctor.Primary
    , defnTraitCtor :: Ctor_Primary
    -- | @type hydra/ext/scala/meta.Template
    , defnTraitTemplate :: Template } deriving (Eq, Generic, Ord, Read, Show)

data Defn_Object
  = Defn_Object
    -- | @type hydra/ext/scala/meta.Term.Name
    { defnObjectName :: Term_Name } deriving (Eq, Generic, Ord, Read, Show)

data Defn
  -- | @type hydra/ext/scala/meta.Defn.Val
  = DefnVal Defn_Val
  -- | @type hydra/ext/scala/meta.Defn.Var
  | DefnVar Defn_Var
  -- | @type hydra/ext/scala/meta.Defn.Given
  | DefnGiven Defn_Given
  -- | @type hydra/ext/scala/meta.Defn.Enum
  | DefnEnum Defn_Enum
  -- | @type hydra/ext/scala/meta.Defn.EnumCase
  | DefnEnumCase Defn_EnumCase
  -- | @type hydra/ext/scala/meta.Defn.RepeatedEnumCase
  | DefnRepeatedEnumCase Defn_RepeatedEnumCase
  -- | @type hydra/ext/scala/meta.Defn.GivenAlias
  | DefnGivenAlias Defn_GivenAlias
  -- | @type hydra/ext/scala/meta.Defn.ExtensionGroup
  | DefnExtensionGroup Defn_ExtensionGroup
  -- | @type hydra/ext/scala/meta.Defn.Def
  | DefnDef Defn_Def
  -- | @type hydra/ext/scala/meta.Defn.Macro
  | DefnMacro Defn_Macro
  -- | @type hydra/ext/scala/meta.Defn.Type
  | DefnType Defn_Type
  -- | @type hydra/ext/scala/meta.Defn.Class
  | DefnClass Defn_Class
  -- | @type hydra/ext/scala/meta.Defn.Trait
  | DefnTrait Defn_Trait
  -- | @type hydra/ext/scala/meta.Defn.Object
  | DefnObject Defn_Object deriving (Eq, Generic, Ord, Read, Show)

data Enumerator_Generator
  = Enumerator_Generator
    -- | @type hydra/ext/scala/meta.Pat
    { enumeratorGeneratorPat :: Pat
    -- | @type hydra/ext/scala/meta.Term
    , enumeratorGeneratorRhs :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Enumerator_CaseGenerator
  = Enumerator_CaseGenerator
    -- | @type hydra/ext/scala/meta.Pat
    { enumeratorCaseGeneratorPat :: Pat
    -- | @type hydra/ext/scala/meta.Term
    , enumeratorCaseGeneratorRhs :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Enumerator_Val
  = Enumerator_Val
    -- | @type hydra/ext/scala/meta.Pat
    { enumeratorValPat :: Pat
    -- | @type hydra/ext/scala/meta.Term
    , enumeratorValRhs :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Enumerator_Guard
  = Enumerator_Guard
    -- | @type hydra/ext/scala/meta.Term
    { enumeratorGuardCond :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Enumerator
  -- | @type hydra/ext/scala/meta.Enumerator.Generator
  = EnumeratorGenerator Enumerator_Generator
  -- | @type hydra/ext/scala/meta.Enumerator.CaseGenerator
  | EnumeratorCaseGenerator Enumerator_CaseGenerator
  -- | @type hydra/ext/scala/meta.Enumerator.Val
  | EnumeratorVal Enumerator_Val
  -- | @type hydra/ext/scala/meta.Enumerator.Guard
  | EnumeratorGuard Enumerator_Guard deriving (Eq, Generic, Ord, Read, Show)

data Export
  = Export
    -- | @type list: hydra/ext/scala/meta.Importer
    { exportImporters :: [Importer] } deriving (Eq, Generic, Ord, Read, Show)

data Import
  = Import
    -- | @type list: hydra/ext/scala/meta.Importer
    { importImporters :: [Importer] } deriving (Eq, Generic, Ord, Read, Show)

data ImportExportStat
  -- | @type hydra/ext/scala/meta.Import
  = ImportExportStatImport Import
  -- | @type hydra/ext/scala/meta.Export
  | ImportExportStatExport Export deriving (Eq, Generic, Ord, Read, Show)

data Importee_Given
  = Importee_Given
    -- | @type hydra/ext/scala/meta.Type
    { importeeGivenTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Importee_Name
  = Importee_Name
    -- | @type hydra/ext/scala/meta.Name
    { importeeNameName :: Name } deriving (Eq, Generic, Ord, Read, Show)

data Importee_Rename
  = Importee_Rename
    -- | @type hydra/ext/scala/meta.Name
    { importeeRenameName :: Name
    -- | @type hydra/ext/scala/meta.Name
    , importeeRenameRename :: Name } deriving (Eq, Generic, Ord, Read, Show)

data Importee_Unimport
  = Importee_Unimport
    -- | @type hydra/ext/scala/meta.Name
    { importeeUnimportName :: Name } deriving (Eq, Generic, Ord, Read, Show)

data Importee
  = ImporteeWildcard
  -- | @type hydra/ext/scala/meta.Importee.Given
  | ImporteeGiven Importee_Given
  | ImporteeGivenAll
  -- | @type hydra/ext/scala/meta.Importee.Name
  | ImporteeName Importee_Name
  -- | @type hydra/ext/scala/meta.Importee.Rename
  | ImporteeRename Importee_Rename
  -- | @type hydra/ext/scala/meta.Importee.Unimport
  | ImporteeUnimport Importee_Unimport deriving (Eq, Generic, Ord, Read, Show)

data Importer
  = Importer
    -- | @type hydra/ext/scala/meta.Term.Ref
    { importerRef :: Term_Ref
    -- | @type list: hydra/ext/scala/meta.Importee
    , importerImportees :: [Importee] } deriving (Eq, Generic, Ord, Read, Show)

data Init
  = Init
    -- | @type hydra/ext/scala/meta.Type
    { initTpe :: Type
    -- | @type hydra/ext/scala/meta.Name
    , initName :: Name
    {-| @type list:
                list: hydra/ext/scala/meta.Term -}
    , initArgss :: [[Term]] } deriving (Eq, Generic, Ord, Read, Show)

data Lit
  = LitNull
  -- | @type integer
  | LitInt Int
  {-| @type float:
              precision:
                bits: 64 -}
  | LitDouble Double
  -- | @type float
  | LitFloat Float
  {-| @type integer:
              precision:
                bits: 8 -}
  | LitByte Integer
  {-| @type integer:
              precision:
                bits: 16 -}
  | LitShort Integer
  {-| @type integer:
              precision:
                bits: 16
              signed: false -}
  | LitChar Integer
  {-| @type integer:
              precision:
                bits: 64 -}
  | LitLong Int64
  -- | @type boolean
  | LitBoolean Bool
  | LitUnit
  -- | @type string
  | LitString String
  -- | @type hydra/ext/scala/meta.ScalaSymbol
  | LitSymbol ScalaSymbol deriving (Eq, Generic, Ord, Read, Show)

data Member_Term
  -- | @type hydra/ext/scala/meta.Pkg
  = Member_TermPkg Pkg
  -- | @type hydra/ext/scala/meta.Pkg.Object
  | Member_TermObject Pkg_Object deriving (Eq, Generic, Ord, Read, Show)

data Member_Type
  = Member_Type
    -- | @type hydra/ext/scala/meta.Type.Name
    { memberTypeName :: Type_Name } deriving (Eq, Generic, Ord, Read, Show)

data Member
  -- | @type hydra/ext/scala/meta.Member.Term
  = MemberTerm Member_Term
  -- | @type hydra/ext/scala/meta.Member.Type
  | MemberType Member_Type
  -- | @type hydra/ext/scala/meta.Term.Param
  | MemberTermParam Term_Param
  -- | @type hydra/ext/scala/meta.Type.Param
  | MemberTypeParam Type_Param
  -- | @type hydra/ext/scala/meta.Self
  | MemberSelf Self deriving (Eq, Generic, Ord, Read, Show)

data Mod_Annot
  = Mod_Annot
    -- | @type hydra/ext/scala/meta.Init
    { modAnnotInit :: Init } deriving (Eq, Generic, Ord, Read, Show)

data Mod_Private
  = Mod_Private
    -- | @type hydra/ext/scala/meta.Ref
    { modPrivateWithin :: Ref } deriving (Eq, Generic, Ord, Read, Show)

data Mod_Protected
  = Mod_Protected
    -- | @type hydra/ext/scala/meta.Ref
    { modProtectedWithin :: Ref } deriving (Eq, Generic, Ord, Read, Show)

data Mod
  -- | @type hydra/ext/scala/meta.Mod.Annot
  = ModAnnot Mod_Annot
  -- | @type hydra/ext/scala/meta.Mod.Private
  | ModPrivate Mod_Private
  -- | @type hydra/ext/scala/meta.Mod.Protected
  | ModProtected Mod_Protected
  | ModImplicit
  | ModFinal
  | ModSealed
  | ModOpen
  | ModSuper
  | ModOverride
  | ModCase
  | ModAbstract
  | ModCovariant
  | ModContravariant
  | ModLazy
  | ModValParam
  | ModVarParam
  | ModInfix
  | ModInline
  | ModUsing
  | ModOpaque
  | ModTransparent deriving (Eq, Generic, Ord, Read, Show)

data Name
  -- | @type string
  = NameValue String
  | NameAnonymous
  -- | @type hydra/ext/scala/meta.PredefString
  | NameIndeterminate PredefString deriving (Eq, Generic, Ord, Read, Show)

data Pat_Var
  = Pat_Var
    -- | @type hydra/ext/scala/meta.Term.Name
    { patVarName :: Term_Name } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Bind
  = Pat_Bind
    -- | @type hydra/ext/scala/meta.Pat
    { patBindLhs :: Pat
    -- | @type hydra/ext/scala/meta.Pat
    , patBindRhs :: Pat } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Alternative
  = Pat_Alternative
    -- | @type hydra/ext/scala/meta.Pat
    { patAlternativeLhs :: Pat
    -- | @type hydra/ext/scala/meta.Pat
    , patAlternativeRhs :: Pat } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Tuple
  = Pat_Tuple
    -- | @type list: hydra/ext/scala/meta.Pat
    { patTupleArgs :: [Pat] } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Repeated
  = Pat_Repeated
    -- | @type hydra/ext/scala/meta.Term.Name
    { patRepeatedName :: Term_Name } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Extract
  = Pat_Extract
    -- | @type hydra/ext/scala/meta.Term
    { patExtractFun :: Term
    -- | @type list: hydra/ext/scala/meta.Pat
    , patExtractArgs :: [Pat] } deriving (Eq, Generic, Ord, Read, Show)

data Pat_ExtractInfix
  = Pat_ExtractInfix
    -- | @type hydra/ext/scala/meta.Pat
    { patExtractInfixLhs :: Pat
    -- | @type hydra/ext/scala/meta.Term.Name
    , patExtractInfixOp :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Pat
    , patExtractInfixRhs :: [Pat] } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Interpolate
  = Pat_Interpolate
    -- | @type hydra/ext/scala/meta.Term.Name
    { patInterpolatePrefix :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Lit
    , patInterpolateParts :: [Lit] } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Xml
  = Pat_Xml
    -- | @type list: hydra/ext/scala/meta.Lit
    { patXmlParts :: [Lit]
    -- | @type list: hydra/ext/scala/meta.Pat
    , patXmlArgs :: [Pat] } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Typed
  = Pat_Typed
    -- | @type hydra/ext/scala/meta.Pat
    { patTypedLhs :: Pat
    -- | @type hydra/ext/scala/meta.Type
    , patTypedRhs :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Macro
  = Pat_Macro
    -- | @type hydra/ext/scala/meta.Term
    { patMacroBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Pat_Given
  = Pat_Given
    -- | @type hydra/ext/scala/meta.Type
    { patGivenTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Pat
  -- | @type hydra/ext/scala/meta.Pat.Var
  = PatVar Pat_Var
  | PatWildcard
  | PatSeqWildcard
  -- | @type hydra/ext/scala/meta.Pat.Bind
  | PatBind Pat_Bind
  -- | @type hydra/ext/scala/meta.Pat.Alternative
  | PatAlternative Pat_Alternative
  -- | @type hydra/ext/scala/meta.Pat.Tuple
  | PatTuple Pat_Tuple
  -- | @type hydra/ext/scala/meta.Pat.Repeated
  | PatRepeated Pat_Repeated
  -- | @type hydra/ext/scala/meta.Pat.Extract
  | PatExtract Pat_Extract
  -- | @type hydra/ext/scala/meta.Pat.ExtractInfix
  | PatExtractInfix Pat_ExtractInfix
  -- | @type hydra/ext/scala/meta.Pat.Interpolate
  | PatInterpolate Pat_Interpolate
  -- | @type hydra/ext/scala/meta.Pat.Xml
  | PatXml Pat_Xml
  -- | @type hydra/ext/scala/meta.Pat.Typed
  | PatTyped Pat_Typed
  -- | @type hydra/ext/scala/meta.Pat.Macro
  | PatMacro Pat_Macro
  -- | @type hydra/ext/scala/meta.Pat.Given
  | PatGiven Pat_Given deriving (Eq, Generic, Ord, Read, Show)

data Pkg_Object
  = Pkg_Object
    -- | @type list: hydra/ext/scala/meta.Mod
    { pkgObjectMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Term.Name
    , pkgObjectName :: Term_Name
    -- | @type hydra/ext/scala/meta.Template
    , pkgObjectTemplate :: Template } deriving (Eq, Generic, Ord, Read, Show)

data Pkg
  = Pkg
    -- | @type hydra/ext/scala/meta.Term.Name
    { pkgName :: Term_Name
    -- | @type hydra/ext/scala/meta.Term.Ref
    , pkgRef :: Term_Ref
    -- | @type list: hydra/ext/scala/meta.Stat
    , pkgStats :: [Stat] } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type PredefString = String

type Quasi = ()

data Ref
  -- | @type hydra/ext/scala/meta.Name
  = RefName Name
  -- | @type hydra/ext/scala/meta.Init
  | RefInit Init deriving (Eq, Generic, Ord, Read, Show)

data ScalaSymbol
  = ScalaSymbol
    -- | @type string
    { scalaSymbolName :: String } deriving (Eq, Generic, Ord, Read, Show)

type Self = ()

data Source
  = Source
    -- | @type list: hydra/ext/scala/meta.Stat
    { sourceStats :: [Stat] } deriving (Eq, Generic, Ord, Read, Show)

data Stat
  -- | @type hydra/ext/scala/meta.Term
  = StatTerm Term
  -- | @type hydra/ext/scala/meta.Decl
  | StatDecl Decl
  -- | @type hydra/ext/scala/meta.Defn
  | StatDefn Defn
  -- | @type hydra/ext/scala/meta.ImportExportStat
  | StatImportExportStat ImportExportStat deriving (Eq, Generic, Ord, Read, Show)

data Template
  = Template
    -- | @type list: hydra/ext/scala/meta.Stat
    { templateEarly :: [Stat]
    -- | @type list: hydra/ext/scala/meta.Init
    , templateInits :: [Init]
    -- | @type hydra/ext/scala/meta.Self
    , templateSelf :: Self
    -- | @type list: hydra/ext/scala/meta.Stat
    , templateStats :: [Stat] } deriving (Eq, Generic, Ord, Read, Show)

data Term_Ref
  -- | @type hydra/ext/scala/meta.Term.This
  = Term_RefThis Term_This
  -- | @type hydra/ext/scala/meta.Term.Super
  | Term_RefSuper Term_Super
  -- | @type hydra/ext/scala/meta.Term.Name
  | Term_RefName Term_Name
  -- | @type hydra/ext/scala/meta.Term.Anonymous
  | Term_RefAnonymous Term_Anonymous
  -- | @type hydra/ext/scala/meta.Term.Select
  | Term_RefSelect Term_Select
  -- | @type hydra/ext/scala/meta.Term.ApplyUnary
  | Term_RefApplyUnary Term_ApplyUnary deriving (Eq, Generic, Ord, Read, Show)

type Term_This = ()

data Term_Super
  = Term_Super
    -- | @type hydra/ext/scala/meta.Name
    { termSuperThisp :: Name
    -- | @type hydra/ext/scala/meta.Name
    , termSuperSuperp :: Name } deriving (Eq, Generic, Ord, Read, Show)

data Term_Name
  = Term_Name
    -- | @type hydra/ext/scala/meta.PredefString
    { termNameValue :: PredefString } deriving (Eq, Generic, Ord, Read, Show)

type Term_Anonymous = ()

data Term_Select
  = Term_Select
    -- | @type hydra/ext/scala/meta.Term
    { termSelectQual :: Term
    -- | @type hydra/ext/scala/meta.Term.Name
    , termSelectName :: Term_Name } deriving (Eq, Generic, Ord, Read, Show)

data Term_Interpolate
  = Term_Interpolate
    -- | @type hydra/ext/scala/meta.Term.Name
    { termInterpolatePrefix :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Lit
    , termInterpolateParts :: [Lit]
    -- | @type list: hydra/ext/scala/meta.Term
    , termInterpolateArgs :: [Term] } deriving (Eq, Generic, Ord, Read, Show)

data Term_Xml
  = Term_Xml
    -- | @type list: hydra/ext/scala/meta.Lit
    { termXmlParts :: [Lit]
    -- | @type list: hydra/ext/scala/meta.Term
    , termXmlArgs :: [Term] } deriving (Eq, Generic, Ord, Read, Show)

data Term_Apply
  = Term_Apply
    -- | @type hydra/ext/scala/meta.Term
    { termApplyFun :: Term
    -- | @type list: hydra/ext/scala/meta.Term
    , termApplyArgs :: [Term] } deriving (Eq, Generic, Ord, Read, Show)

data Term_ApplyUsing
  = Term_ApplyUsing
    -- | @type hydra/ext/scala/meta.Term
    { termApplyUsingFun :: Term
    -- | @type list: hydra/ext/scala/meta.Type
    , termApplyUsingTargs :: [Type] } deriving (Eq, Generic, Ord, Read, Show)

data Term_ApplyType
  = Term_ApplyType
    -- | @type hydra/ext/scala/meta.Term
    { termApplyTypeLhs :: Term
    -- | @type hydra/ext/scala/meta.Term.Name
    , termApplyTypeOp :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Type
    , termApplyTypeTargs :: [Type]
    -- | @type list: hydra/ext/scala/meta.Term
    , termApplyTypeArgs :: [Term] } deriving (Eq, Generic, Ord, Read, Show)

data Term_ApplyInfix
  = Term_ApplyInfix
    -- | @type hydra/ext/scala/meta.Term
    { termApplyInfixLhs :: Term
    -- | @type hydra/ext/scala/meta.Term.Name
    , termApplyInfixOp :: Term_Name
    -- | @type list: hydra/ext/scala/meta.Type
    , termApplyInfixTargs :: [Type]
    -- | @type list: hydra/ext/scala/meta.Term
    , termApplyInfixArgs :: [Term] } deriving (Eq, Generic, Ord, Read, Show)

data Term_ApplyUnary
  = Term_ApplyUnary
    -- | @type hydra/ext/scala/meta.Term.Name
    { termApplyUnaryOp :: Term_Name
    -- | @type hydra/ext/scala/meta.Term
    , termApplyUnaryArg :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Assign
  = Term_Assign
    -- | @type hydra/ext/scala/meta.Term
    { termAssignLhs :: Term
    -- | @type hydra/ext/scala/meta.Term
    , termAssignRhs :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Return
  = Term_Return
    -- | @type hydra/ext/scala/meta.Term
    { termReturnExpr :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Throw
  = Term_Throw
    -- | @type hydra/ext/scala/meta.Term
    { termThrowExpr :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Ascribe
  = Term_Ascribe
    -- | @type hydra/ext/scala/meta.Term
    { termAscribeExpr :: Term
    -- | @type hydra/ext/scala/meta.Type
    , termAscribeTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Term_Annotate
  = Term_Annotate
    -- | @type hydra/ext/scala/meta.Term
    { termAnnotateExpr :: Term
    -- | @type list: hydra/ext/scala/meta.Mod.Annot
    , termAnnotateAnnots :: [Mod_Annot] } deriving (Eq, Generic, Ord, Read, Show)

data Term_Tuple
  = Term_Tuple
    -- | @type list: hydra/ext/scala/meta.Term
    { termTupleArgs :: [Term] } deriving (Eq, Generic, Ord, Read, Show)

data Term_Block
  = Term_Block
    -- | @type list: hydra/ext/scala/meta.Stat
    { termBlockStats :: [Stat] } deriving (Eq, Generic, Ord, Read, Show)

data Term_EndMarker
  = Term_EndMarker
    -- | @type hydra/ext/scala/meta.Term.Name
    { termEndMarkerName :: Term_Name } deriving (Eq, Generic, Ord, Read, Show)

data Term_If
  = Term_If
    -- | @type hydra/ext/scala/meta.Term
    { termIfCond :: Term
    -- | @type hydra/ext/scala/meta.Term
    , termIfThenp :: Term
    -- | @type hydra/ext/scala/meta.Term
    , termIfElsep :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_QuotedMacroExpr
  = Term_QuotedMacroExpr
    -- | @type hydra/ext/scala/meta.Term
    { termQuotedMacroExprBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_QuotedMacroType
  = Term_QuotedMacroType
    -- | @type hydra/ext/scala/meta.Type
    { termQuotedMacroTypeTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Term_SplicedMacroExpr
  = Term_SplicedMacroExpr
    -- | @type hydra/ext/scala/meta.Term
    { termSplicedMacroExprBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Match
  = Term_Match
    -- | @type hydra/ext/scala/meta.Term
    { termMatchExpr :: Term
    -- | @type list: hydra/ext/scala/meta.Case
    , termMatchCases :: [Case] } deriving (Eq, Generic, Ord, Read, Show)

data Term_Try
  = Term_Try
    -- | @type hydra/ext/scala/meta.Term
    { termTryExpr :: Term
    -- | @type list: hydra/ext/scala/meta.Case
    , termTryCatchp :: [Case]
    -- | @type optional: hydra/ext/scala/meta.Term
    , termTryFinallyp :: Maybe Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_TryWithHandler
  = Term_TryWithHandler
    -- | @type hydra/ext/scala/meta.Term
    { termTryWithHandlerExpr :: Term
    -- | @type hydra/ext/scala/meta.Term
    , termTryWithHandlerCatchp :: Term
    -- | @type optional: hydra/ext/scala/meta.Term
    , termTryWithHandlerFinallyp :: Maybe Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_FunctionTerm
  -- | @type hydra/ext/scala/meta.Term.ContextFunction
  = Term_FunctionTermContextFunction Term_ContextFunction
  -- | @type hydra/ext/scala/meta.Term.Function
  | Term_FunctionTermFunction Term_Function deriving (Eq, Generic, Ord, Read, Show)

data Term_ContextFunction
  = Term_ContextFunction
    -- | @type list: hydra/ext/scala/meta.Term.Param
    { termContextFunctionParams :: [Term_Param]
    -- | @type hydra/ext/scala/meta.Term
    , termContextFunctionBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Function
  = Term_Function
    -- | @type list: hydra/ext/scala/meta.Term.Param
    { termFunctionParams :: [Term_Param]
    -- | @type hydra/ext/scala/meta.Term
    , termFunctionBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_PolyFunction
  = Term_PolyFunction
    -- | @type list: hydra/ext/scala/meta.Type.Param
    { termPolyFunctionTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Term
    , termPolyFunctionBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_PartialFunction
  = Term_PartialFunction
    -- | @type list: hydra/ext/scala/meta.Case
    { termPartialFunctionCases :: [Case] } deriving (Eq, Generic, Ord, Read, Show)

data Term_While
  = Term_While
    -- | @type hydra/ext/scala/meta.Term
    { termWhileExpr :: Term
    -- | @type hydra/ext/scala/meta.Term
    , termWhileBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Do
  = Term_Do
    -- | @type hydra/ext/scala/meta.Term
    { termDoBody :: Term
    -- | @type hydra/ext/scala/meta.Term
    , termDoExpr :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_For
  = Term_For
    -- | @type list: hydra/ext/scala/meta.Enumerator
    { termForEnums :: [Enumerator] } deriving (Eq, Generic, Ord, Read, Show)

data Term_ForYield
  = Term_ForYield
    -- | @type list: hydra/ext/scala/meta.Enumerator
    { termForYieldEnums :: [Enumerator] } deriving (Eq, Generic, Ord, Read, Show)

data Term_New
  = Term_New
    -- | @type hydra/ext/scala/meta.Init
    { termNewInit :: Init } deriving (Eq, Generic, Ord, Read, Show)

data Term_NewAnonymous
  = Term_NewAnonymous
    -- | @type hydra/ext/scala/meta.Template
    { termNewAnonymousTempl :: Template } deriving (Eq, Generic, Ord, Read, Show)

type Term_Placeholder = ()

data Term_Eta
  = Term_Eta
    -- | @type hydra/ext/scala/meta.Term
    { termEtaExpr :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Repeated
  = Term_Repeated
    -- | @type hydra/ext/scala/meta.Term
    { termRepeatedExpr :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Term_Param
  = Term_Param
    -- | @type list: hydra/ext/scala/meta.Mod
    { termParamMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Name
    , termParamName :: Name } deriving (Eq, Generic, Ord, Read, Show)

data Term
  -- | @type hydra/ext/scala/meta.Lit
  = TermLit Lit
  -- | @type hydra/ext/scala/meta.Term.Ref
  | TermRef Term_Ref
  -- | @type hydra/ext/scala/meta.Term.Interpolate
  | TermInterpolate Term_Interpolate
  -- | @type hydra/ext/scala/meta.Term.Xml
  | TermXml Term_Xml
  -- | @type hydra/ext/scala/meta.Term.Apply
  | TermApply Term_Apply
  -- | @type hydra/ext/scala/meta.Term.ApplyUsing
  | TermApplyUsing Term_ApplyUsing
  -- | @type hydra/ext/scala/meta.Term.ApplyType
  | TermApplyType Term_ApplyType
  -- | @type hydra/ext/scala/meta.Term.ApplyInfix
  | TermApplyInfix Term_ApplyInfix
  -- | @type hydra/ext/scala/meta.Term.Assign
  | TermAssign Term_Assign
  -- | @type hydra/ext/scala/meta.Term.Return
  | TermReturn Term_Return
  -- | @type hydra/ext/scala/meta.Term.Throw
  | TermThrow Term_Throw
  -- | @type hydra/ext/scala/meta.Term.Ascribe
  | TermAscribe Term_Ascribe
  -- | @type hydra/ext/scala/meta.Term.Annotate
  | TermAnnotate Term_Annotate
  -- | @type hydra/ext/scala/meta.Term.Tuple
  | TermTuple Term_Tuple
  -- | @type hydra/ext/scala/meta.Term.Block
  | TermBlock Term_Block
  -- | @type hydra/ext/scala/meta.Term.EndMarker
  | TermEndMarker Term_EndMarker
  -- | @type hydra/ext/scala/meta.Term.If
  | TermIf Term_If
  -- | @type hydra/ext/scala/meta.Term.QuotedMacroExpr
  | TermQuotedMacroExpr Term_QuotedMacroExpr
  -- | @type hydra/ext/scala/meta.Term.QuotedMacroType
  | TermQuotedMacroType Term_QuotedMacroType
  -- | @type hydra/ext/scala/meta.Term.SplicedMacroExpr
  | TermSplicedMacroExpr Term_SplicedMacroExpr
  -- | @type hydra/ext/scala/meta.Term.Match
  | TermMatch Term_Match
  -- | @type hydra/ext/scala/meta.Term.Try
  | TermTry Term_Try
  -- | @type hydra/ext/scala/meta.Term.TryWithHandler
  | TermTryWithHandler Term_TryWithHandler
  -- | @type hydra/ext/scala/meta.Term.FunctionTerm
  | TermFunctionTerm Term_FunctionTerm
  -- | @type hydra/ext/scala/meta.Term.PolyFunction
  | TermPolyFunction Term_PolyFunction
  -- | @type hydra/ext/scala/meta.Term.PartialFunction
  | TermPartialFunction Term_PartialFunction
  -- | @type hydra/ext/scala/meta.Term.While
  | TermWhile Term_While
  -- | @type hydra/ext/scala/meta.Term.Do
  | TermDo Term_Do
  -- | @type hydra/ext/scala/meta.Term.For
  | TermFor Term_For
  -- | @type hydra/ext/scala/meta.Term.ForYield
  | TermForYield Term_ForYield
  -- | @type hydra/ext/scala/meta.Term.New
  | TermNew Term_New
  -- | @type hydra/ext/scala/meta.Term.NewAnonymous
  | TermNewAnonymous Term_NewAnonymous
  -- | @type hydra/ext/scala/meta.Term.Placeholder
  | TermPlaceholder Term_Placeholder
  -- | @type hydra/ext/scala/meta.Term.Eta
  | TermEta Term_Eta
  -- | @type hydra/ext/scala/meta.Term.Repeated
  | TermRepeated Term_Repeated deriving (Eq, Generic, Ord, Read, Show)

data Tree
  -- | @type hydra/ext/scala/meta.Ref
  = TreeRef Ref
  -- | @type hydra/ext/scala/meta.Stat
  | TreeStat Stat
  -- | @type hydra/ext/scala/meta.Type
  | TreeType Type
  -- | @type hydra/ext/scala/meta.Type.Bounds
  | TreeBounds Type_Bounds
  -- | @type hydra/ext/scala/meta.Pat
  | TreePat Pat
  -- | @type hydra/ext/scala/meta.Member
  | TreeMember Member
  -- | @type hydra/ext/scala/meta.Ctor
  | TreeCtor Ctor
  -- | @type hydra/ext/scala/meta.Template
  | TreeTemplate Template
  -- | @type hydra/ext/scala/meta.Mod
  | TreeMod Mod
  -- | @type hydra/ext/scala/meta.Enumerator
  | TreeEnumerator Enumerator
  -- | @type hydra/ext/scala/meta.Importer
  | TreeImporter Importer
  -- | @type hydra/ext/scala/meta.Importee
  | TreeImportee Importee
  -- | @type hydra/ext/scala/meta.CaseTree
  | TreeCaseTree CaseTree
  -- | @type hydra/ext/scala/meta.Source
  | TreeSource Source
  -- | @type hydra/ext/scala/meta.Quasi
  | TreeQuasi Quasi deriving (Eq, Generic, Ord, Read, Show)

data Type_Ref
  -- | @type hydra/ext/scala/meta.Type.Name
  = Type_RefName Type_Name
  -- | @type hydra/ext/scala/meta.Type.Select
  | Type_RefSelect Type_Select
  -- | @type hydra/ext/scala/meta.Type.Project
  | Type_RefProject Type_Project
  -- | @type hydra/ext/scala/meta.Type.Singleton
  | Type_RefSingleton Type_Singleton deriving (Eq, Generic, Ord, Read, Show)

data Type_Name
  = Type_Name
    -- | @type string
    { typeNameValue :: String } deriving (Eq, Generic, Ord, Read, Show)

type Type_AnonymousName = ()

data Type_Select
  = Type_Select
    -- | @type hydra/ext/scala/meta.Term.Ref
    { typeSelectQual :: Term_Ref
    -- | @type hydra/ext/scala/meta.Type.Name
    , typeSelectName :: Type_Name } deriving (Eq, Generic, Ord, Read, Show)

data Type_Project
  = Type_Project
    -- | @type hydra/ext/scala/meta.Type
    { typeProjectQual :: Type
    -- | @type hydra/ext/scala/meta.Type.Name
    , typeProjectName :: Type_Name } deriving (Eq, Generic, Ord, Read, Show)

data Type_Singleton
  = Type_Singleton
    -- | @type hydra/ext/scala/meta.Term.Ref
    { typeSingletonRef :: Term_Ref } deriving (Eq, Generic, Ord, Read, Show)

data Type_Apply
  = Type_Apply
    -- | @type hydra/ext/scala/meta.Type
    { typeApplyTpe :: Type
    -- | @type list: hydra/ext/scala/meta.Type
    , typeApplyArgs :: [Type] } deriving (Eq, Generic, Ord, Read, Show)

data Type_ApplyInfix
  = Type_ApplyInfix
    -- | @type hydra/ext/scala/meta.Type
    { typeApplyInfixLhs :: Type
    -- | @type hydra/ext/scala/meta.Type.Name
    , typeApplyInfixOp :: Type_Name
    -- | @type hydra/ext/scala/meta.Type
    , typeApplyInfixRhs :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_FunctionType
  -- | @type hydra/ext/scala/meta.Type.Function
  = Type_FunctionTypeFunction Type_Function
  -- | @type hydra/ext/scala/meta.Type.ContextFunction
  | Type_FunctionTypeContextFunction Type_ContextFunction deriving (Eq, Generic, Ord, Read, Show)

data Type_Function
  = Type_Function
    -- | @type list: hydra/ext/scala/meta.Type
    { typeFunctionParams :: [Type]
    -- | @type hydra/ext/scala/meta.Type
    , typeFunctionRes :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_PolyFunction
  = Type_PolyFunction
    -- | @type list: hydra/ext/scala/meta.Type.Param
    { typePolyFunctionTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Type
    , typePolyFunctionTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_ContextFunction
  = Type_ContextFunction
    -- | @type list: hydra/ext/scala/meta.Type
    { typeContextFunctionParams :: [Type]
    -- | @type hydra/ext/scala/meta.Type
    , typeContextFunctionRes :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_ImplicitFunction
  = Type_ImplicitFunction
    -- | @type list: hydra/ext/scala/meta.Type
    { typeImplicitFunctionParams :: [Type]
    -- | @type hydra/ext/scala/meta.Type
    , typeImplicitFunctionRes :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Tuple
  = Type_Tuple
    -- | @type list: hydra/ext/scala/meta.Type
    { typeTupleArgs :: [Type] } deriving (Eq, Generic, Ord, Read, Show)

data Type_With
  = Type_With
    -- | @type hydra/ext/scala/meta.Type
    { typeWithLhs :: Type
    -- | @type hydra/ext/scala/meta.Type
    , typeWithRhs :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_And
  = Type_And
    -- | @type hydra/ext/scala/meta.Type
    { typeAndLhs :: Type
    -- | @type hydra/ext/scala/meta.Type
    , typeAndRhs :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Or
  = Type_Or
    -- | @type hydra/ext/scala/meta.Type
    { typeOrLhs :: Type
    -- | @type hydra/ext/scala/meta.Type
    , typeOrRhs :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Refine
  = Type_Refine
    -- | @type optional: hydra/ext/scala/meta.Type
    { typeRefineTpe :: Maybe Type
    -- | @type list: hydra/ext/scala/meta.Stat
    , typeRefineStats :: [Stat] } deriving (Eq, Generic, Ord, Read, Show)

data Type_Existential
  = Type_Existential
    -- | @type hydra/ext/scala/meta.Type
    { typeExistentialTpe :: Type
    -- | @type list: hydra/ext/scala/meta.Stat
    , typeExistentialStats :: [Stat] } deriving (Eq, Generic, Ord, Read, Show)

data Type_Annotate
  = Type_Annotate
    -- | @type hydra/ext/scala/meta.Type
    { typeAnnotateTpe :: Type
    -- | @type list: hydra/ext/scala/meta.Mod.Annot
    , typeAnnotateAnnots :: [Mod_Annot] } deriving (Eq, Generic, Ord, Read, Show)

data Type_Lambda
  = Type_Lambda
    -- | @type list: hydra/ext/scala/meta.Type.Param
    { typeLambdaTparams :: [Type_Param]
    -- | @type hydra/ext/scala/meta.Type
    , typeLambdaTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Macro
  = Type_Macro
    -- | @type hydra/ext/scala/meta.Term
    { typeMacroBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data Type_Method
  = Type_Method
    {-| @type list:
                list: hydra/ext/scala/meta.Term.Param -}
    { typeMethodParamss :: [[Term_Param]]
    -- | @type hydra/ext/scala/meta.Type
    , typeMethodTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Placeholder
  = Type_Placeholder
    -- | @type hydra/ext/scala/meta.Type.Bounds
    { typePlaceholderBounds :: Type_Bounds } deriving (Eq, Generic, Ord, Read, Show)

data Type_Bounds
  = Type_Bounds
    -- | @type optional: hydra/ext/scala/meta.Type
    { typeBoundsLo :: Maybe Type
    -- | @type optional: hydra/ext/scala/meta.Type
    , typeBoundsHi :: Maybe Type } deriving (Eq, Generic, Ord, Read, Show)

type Type_ByName = ()

data Type_Repeated
  = Type_Repeated
    -- | @type hydra/ext/scala/meta.Type
    { typeRepeatedTpe :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Var
  = Type_Var
    -- | @type hydra/ext/scala/meta.Type.Name
    { typeVarName :: Type_Name } deriving (Eq, Generic, Ord, Read, Show)

data Type_TypedParam
  = Type_TypedParam
    -- | @type hydra/ext/scala/meta.Name
    { typeTypedParamName :: Name
    -- | @type hydra/ext/scala/meta.Type
    , typeTypedParamTyp :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Param
  = Type_Param
    -- | @type list: hydra/ext/scala/meta.Mod
    { typeParamMods :: [Mod]
    -- | @type hydra/ext/scala/meta.Name
    , typeParamName :: Name
    -- | @type list: hydra/ext/scala/meta.Type.Param
    , typeParamTparams :: [Type_Param]
    -- | @type list: hydra/ext/scala/meta.Type.Bounds
    , typeParamTbounds :: [Type_Bounds]
    -- | @type list: hydra/ext/scala/meta.Type
    , typeParamVbounds :: [Type]
    -- | @type list: hydra/ext/scala/meta.Type
    , typeParamCbounds :: [Type] } deriving (Eq, Generic, Ord, Read, Show)

data Type_Match
  = Type_Match
    -- | @type hydra/ext/scala/meta.Type
    { typeMatchTpe :: Type
    -- | @type list: hydra/ext/scala/meta.TypeCase
    , typeMatchCases :: [TypeCase] } deriving (Eq, Generic, Ord, Read, Show)

data Type
  -- | @type hydra/ext/scala/meta.Type.Ref
  = TypeRef Type_Ref
  -- | @type hydra/ext/scala/meta.Type.AnonymousName
  | TypeAnonymousName Type_AnonymousName
  -- | @type hydra/ext/scala/meta.Type.Apply
  | TypeApply Type_Apply
  -- | @type hydra/ext/scala/meta.Type.ApplyInfix
  | TypeApplyInfix Type_ApplyInfix
  -- | @type hydra/ext/scala/meta.Type.FunctionType
  | TypeFunctionType Type_FunctionType
  -- | @type hydra/ext/scala/meta.Type.PolyFunction
  | TypePolyFunction Type_PolyFunction
  -- | @type hydra/ext/scala/meta.Type.ImplicitFunction
  | TypeImplicitFunction Type_ImplicitFunction
  -- | @type hydra/ext/scala/meta.Type.Tuple
  | TypeTuple Type_Tuple
  -- | @type hydra/ext/scala/meta.Type.With
  | TypeWith Type_With
  -- | @type hydra/ext/scala/meta.Type.And
  | TypeAnd Type_And
  -- | @type hydra/ext/scala/meta.Type.Or
  | TypeOr Type_Or
  -- | @type hydra/ext/scala/meta.Type.Refine
  | TypeRefine Type_Refine
  -- | @type hydra/ext/scala/meta.Type.Existential
  | TypeExistential Type_Existential
  -- | @type hydra/ext/scala/meta.Type.Annotate
  | TypeAnnotate Type_Annotate
  -- | @type hydra/ext/scala/meta.Type.Lambda
  | TypeLambda Type_Lambda
  -- | @type hydra/ext/scala/meta.Type.Macro
  | TypeMacro Type_Macro
  -- | @type hydra/ext/scala/meta.Type.Method
  | TypeMethod Type_Method
  -- | @type hydra/ext/scala/meta.Type.Placeholder
  | TypePlaceholder Type_Placeholder
  -- | @type hydra/ext/scala/meta.Type.ByName
  | TypeByName Type_ByName
  -- | @type hydra/ext/scala/meta.Type.Repeated
  | TypeRepeated Type_Repeated
  -- | @type hydra/ext/scala/meta.Type.Var
  | TypeVar Type_Var
  -- | @type hydra/ext/scala/meta.Type.TypedParam
  | TypeTypedParam Type_TypedParam
  -- | @type hydra/ext/scala/meta.Type.Match
  | TypeMatch Type_Match deriving (Eq, Generic, Ord, Read, Show)

data TypeCase
  = TypeCase
    -- | @type hydra/ext/scala/meta.Type
    { typeCasePat :: Type
    -- | @type hydra/ext/scala/meta.Type
    , typeCaseBody :: Type } deriving (Eq, Generic, Ord, Read, Show)

_Case = "hydra/ext/scala/meta.Case" :: String
_CaseTree = "hydra/ext/scala/meta.CaseTree" :: String
_CaseTree_case = "case" :: String
_CaseTree_typeCase = "typeCase" :: String
_Case_body = "body" :: String
_Case_cond = "cond" :: String
_Case_pat = "pat" :: String
_Ctor = "hydra/ext/scala/meta.Ctor" :: String
_Ctor_Primary = "hydra/ext/scala/meta.Ctor_Primary" :: String
_Ctor_Primary_mods = "mods" :: String
_Ctor_Primary_name = "name" :: String
_Ctor_Primary_paramss = "paramss" :: String
_Ctor_Secondary = "hydra/ext/scala/meta.Ctor_Secondary" :: String
_Ctor_Secondary_init = "init" :: String
_Ctor_Secondary_mods = "mods" :: String
_Ctor_Secondary_name = "name" :: String
_Ctor_Secondary_paramss = "paramss" :: String
_Ctor_Secondary_stats = "stats" :: String
_Ctor_primary = "primary" :: String
_Ctor_secondary = "secondary" :: String
_Decl = "hydra/ext/scala/meta.Decl" :: String
_Decl_Def = "hydra/ext/scala/meta.Decl_Def" :: String
_Decl_Def_decltpe = "decltpe" :: String
_Decl_Def_mods = "mods" :: String
_Decl_Def_name = "name" :: String
_Decl_Def_paramss = "paramss" :: String
_Decl_Def_tparams = "tparams" :: String
_Decl_Given = "hydra/ext/scala/meta.Decl_Given" :: String
_Decl_Given_decltpe = "decltpe" :: String
_Decl_Given_mods = "mods" :: String
_Decl_Given_name = "name" :: String
_Decl_Given_sparams = "sparams" :: String
_Decl_Given_tparams = "tparams" :: String
_Decl_Type = "hydra/ext/scala/meta.Decl_Type" :: String
_Decl_Type_bounds = "bounds" :: String
_Decl_Type_mods = "mods" :: String
_Decl_Type_name = "name" :: String
_Decl_Type_tparams = "tparams" :: String
_Decl_Val = "hydra/ext/scala/meta.Decl_Val" :: String
_Decl_Val_decltpe = "decltpe" :: String
_Decl_Val_mods = "mods" :: String
_Decl_Val_pats = "pats" :: String
_Decl_Var = "hydra/ext/scala/meta.Decl_Var" :: String
_Decl_Var_decltpe = "decltpe" :: String
_Decl_Var_mods = "mods" :: String
_Decl_Var_pats = "pats" :: String
_Decl_def = "def" :: String
_Decl_given = "given" :: String
_Decl_type = "type" :: String
_Decl_val = "val" :: String
_Decl_var = "var" :: String
_Defn = "hydra/ext/scala/meta.Defn" :: String
_Defn_Class = "hydra/ext/scala/meta.Defn_Class" :: String
_Defn_Class_ctor = "ctor" :: String
_Defn_Class_mods = "mods" :: String
_Defn_Class_name = "name" :: String
_Defn_Class_template = "template" :: String
_Defn_Class_tparams = "tparams" :: String
_Defn_Def = "hydra/ext/scala/meta.Defn_Def" :: String
_Defn_Def_body = "body" :: String
_Defn_Def_decltpe = "decltpe" :: String
_Defn_Def_mods = "mods" :: String
_Defn_Def_name = "name" :: String
_Defn_Def_paramss = "paramss" :: String
_Defn_Def_tparams = "tparams" :: String
_Defn_Enum = "hydra/ext/scala/meta.Defn_Enum" :: String
_Defn_EnumCase = "hydra/ext/scala/meta.Defn_EnumCase" :: String
_Defn_EnumCase_ctor = "ctor" :: String
_Defn_EnumCase_inits = "inits" :: String
_Defn_EnumCase_mods = "mods" :: String
_Defn_EnumCase_name = "name" :: String
_Defn_EnumCase_tparams = "tparams" :: String
_Defn_Enum_ctor = "ctor" :: String
_Defn_Enum_mods = "mods" :: String
_Defn_Enum_name = "name" :: String
_Defn_Enum_template = "template" :: String
_Defn_Enum_tparams = "tparams" :: String
_Defn_ExtensionGroup = "hydra/ext/scala/meta.Defn_ExtensionGroup" :: String
_Defn_ExtensionGroup_body = "body" :: String
_Defn_ExtensionGroup_paramss = "paramss" :: String
_Defn_ExtensionGroup_tparams = "tparams" :: String
_Defn_Given = "hydra/ext/scala/meta.Defn_Given" :: String
_Defn_GivenAlias = "hydra/ext/scala/meta.Defn_GivenAlias" :: String
_Defn_GivenAlias_body = "body" :: String
_Defn_GivenAlias_decltpe = "decltpe" :: String
_Defn_GivenAlias_mods = "mods" :: String
_Defn_GivenAlias_name = "name" :: String
_Defn_GivenAlias_sparams = "sparams" :: String
_Defn_GivenAlias_tparams = "tparams" :: String
_Defn_Given_mods = "mods" :: String
_Defn_Given_name = "name" :: String
_Defn_Given_sparams = "sparams" :: String
_Defn_Given_templ = "templ" :: String
_Defn_Given_tparams = "tparams" :: String
_Defn_Macro = "hydra/ext/scala/meta.Defn_Macro" :: String
_Defn_Macro_body = "body" :: String
_Defn_Macro_decltpe = "decltpe" :: String
_Defn_Macro_mods = "mods" :: String
_Defn_Macro_name = "name" :: String
_Defn_Macro_paramss = "paramss" :: String
_Defn_Macro_tparams = "tparams" :: String
_Defn_Object = "hydra/ext/scala/meta.Defn_Object" :: String
_Defn_Object_name = "name" :: String
_Defn_RepeatedEnumCase = "hydra/ext/scala/meta.Defn_RepeatedEnumCase" :: String
_Defn_RepeatedEnumCase_cases = "cases" :: String
_Defn_RepeatedEnumCase_mods = "mods" :: String
_Defn_Trait = "hydra/ext/scala/meta.Defn_Trait" :: String
_Defn_Trait_ctor = "ctor" :: String
_Defn_Trait_mods = "mods" :: String
_Defn_Trait_name = "name" :: String
_Defn_Trait_template = "template" :: String
_Defn_Trait_tparams = "tparams" :: String
_Defn_Type = "hydra/ext/scala/meta.Defn_Type" :: String
_Defn_Type_body = "body" :: String
_Defn_Type_mods = "mods" :: String
_Defn_Type_name = "name" :: String
_Defn_Type_tparams = "tparams" :: String
_Defn_Val = "hydra/ext/scala/meta.Defn_Val" :: String
_Defn_Val_decltpe = "decltpe" :: String
_Defn_Val_mods = "mods" :: String
_Defn_Val_pats = "pats" :: String
_Defn_Val_rhs = "rhs" :: String
_Defn_Var = "hydra/ext/scala/meta.Defn_Var" :: String
_Defn_Var_decltpe = "decltpe" :: String
_Defn_Var_mods = "mods" :: String
_Defn_Var_pats = "pats" :: String
_Defn_Var_rhs = "rhs" :: String
_Defn_class = "class" :: String
_Defn_def = "def" :: String
_Defn_enum = "enum" :: String
_Defn_enumCase = "enumCase" :: String
_Defn_extensionGroup = "extensionGroup" :: String
_Defn_given = "given" :: String
_Defn_givenAlias = "givenAlias" :: String
_Defn_macro = "macro" :: String
_Defn_object = "object" :: String
_Defn_repeatedEnumCase = "repeatedEnumCase" :: String
_Defn_trait = "trait" :: String
_Defn_type = "type" :: String
_Defn_val = "val" :: String
_Defn_var = "var" :: String
_Enumerator = "hydra/ext/scala/meta.Enumerator" :: String
_Enumerator_CaseGenerator = "hydra/ext/scala/meta.Enumerator_CaseGenerator" :: String
_Enumerator_CaseGenerator_pat = "pat" :: String
_Enumerator_CaseGenerator_rhs = "rhs" :: String
_Enumerator_Generator = "hydra/ext/scala/meta.Enumerator_Generator" :: String
_Enumerator_Generator_pat = "pat" :: String
_Enumerator_Generator_rhs = "rhs" :: String
_Enumerator_Guard = "hydra/ext/scala/meta.Enumerator_Guard" :: String
_Enumerator_Guard_cond = "cond" :: String
_Enumerator_Val = "hydra/ext/scala/meta.Enumerator_Val" :: String
_Enumerator_Val_pat = "pat" :: String
_Enumerator_Val_rhs = "rhs" :: String
_Enumerator_caseGenerator = "caseGenerator" :: String
_Enumerator_generator = "generator" :: String
_Enumerator_guard = "guard" :: String
_Enumerator_val = "val" :: String
_Export = "hydra/ext/scala/meta.Export" :: String
_Export_importers = "importers" :: String
_Import = "hydra/ext/scala/meta.Import" :: String
_ImportExportStat = "hydra/ext/scala/meta.ImportExportStat" :: String
_ImportExportStat_export = "export" :: String
_ImportExportStat_import = "import" :: String
_Import_importers = "importers" :: String
_Importee = "hydra/ext/scala/meta.Importee" :: String
_Importee_Given = "hydra/ext/scala/meta.Importee_Given" :: String
_Importee_Given_tpe = "tpe" :: String
_Importee_Name = "hydra/ext/scala/meta.Importee_Name" :: String
_Importee_Name_name = "name" :: String
_Importee_Rename = "hydra/ext/scala/meta.Importee_Rename" :: String
_Importee_Rename_name = "name" :: String
_Importee_Rename_rename = "rename" :: String
_Importee_Unimport = "hydra/ext/scala/meta.Importee_Unimport" :: String
_Importee_Unimport_name = "name" :: String
_Importee_given = "given" :: String
_Importee_givenAll = "givenAll" :: String
_Importee_name = "name" :: String
_Importee_rename = "rename" :: String
_Importee_unimport = "unimport" :: String
_Importee_wildcard = "wildcard" :: String
_Importer = "hydra/ext/scala/meta.Importer" :: String
_Importer_importees = "importees" :: String
_Importer_ref = "ref" :: String
_Init = "hydra/ext/scala/meta.Init" :: String
_Init_argss = "argss" :: String
_Init_name = "name" :: String
_Init_tpe = "tpe" :: String
_Lit = "hydra/ext/scala/meta.Lit" :: String
_Lit_boolean = "boolean" :: String
_Lit_byte = "byte" :: String
_Lit_char = "char" :: String
_Lit_double = "double" :: String
_Lit_float = "float" :: String
_Lit_int = "int" :: String
_Lit_long = "long" :: String
_Lit_null = "null" :: String
_Lit_short = "short" :: String
_Lit_string = "string" :: String
_Lit_symbol = "symbol" :: String
_Lit_unit = "unit" :: String
_Member = "hydra/ext/scala/meta.Member" :: String
_Member_Term = "hydra/ext/scala/meta.Member_Term" :: String
_Member_Term_object = "object" :: String
_Member_Term_pkg = "pkg" :: String
_Member_Type = "hydra/ext/scala/meta.Member_Type" :: String
_Member_Type_name = "name" :: String
_Member_self = "self" :: String
_Member_term = "term" :: String
_Member_termParam = "termParam" :: String
_Member_type = "type" :: String
_Member_typeParam = "typeParam" :: String
_Mod = "hydra/ext/scala/meta.Mod" :: String
_Mod_Annot = "hydra/ext/scala/meta.Mod_Annot" :: String
_Mod_Annot_init = "init" :: String
_Mod_Private = "hydra/ext/scala/meta.Mod_Private" :: String
_Mod_Private_within = "within" :: String
_Mod_Protected = "hydra/ext/scala/meta.Mod_Protected" :: String
_Mod_Protected_within = "within" :: String
_Mod_abstract = "abstract" :: String
_Mod_annot = "annot" :: String
_Mod_case = "case" :: String
_Mod_contravariant = "contravariant" :: String
_Mod_covariant = "covariant" :: String
_Mod_final = "final" :: String
_Mod_implicit = "implicit" :: String
_Mod_infix = "infix" :: String
_Mod_inline = "inline" :: String
_Mod_lazy = "lazy" :: String
_Mod_opaque = "opaque" :: String
_Mod_open = "open" :: String
_Mod_override = "override" :: String
_Mod_private = "private" :: String
_Mod_protected = "protected" :: String
_Mod_sealed = "sealed" :: String
_Mod_super = "super" :: String
_Mod_transparent = "transparent" :: String
_Mod_using = "using" :: String
_Mod_valParam = "valParam" :: String
_Mod_varParam = "varParam" :: String
_Name = "hydra/ext/scala/meta.Name" :: String
_Name_anonymous = "anonymous" :: String
_Name_indeterminate = "indeterminate" :: String
_Name_value = "value" :: String
_Pat = "hydra/ext/scala/meta.Pat" :: String
_Pat_Alternative = "hydra/ext/scala/meta.Pat_Alternative" :: String
_Pat_Alternative_lhs = "lhs" :: String
_Pat_Alternative_rhs = "rhs" :: String
_Pat_Bind = "hydra/ext/scala/meta.Pat_Bind" :: String
_Pat_Bind_lhs = "lhs" :: String
_Pat_Bind_rhs = "rhs" :: String
_Pat_Extract = "hydra/ext/scala/meta.Pat_Extract" :: String
_Pat_ExtractInfix = "hydra/ext/scala/meta.Pat_ExtractInfix" :: String
_Pat_ExtractInfix_lhs = "lhs" :: String
_Pat_ExtractInfix_op = "op" :: String
_Pat_ExtractInfix_rhs = "rhs" :: String
_Pat_Extract_args = "args" :: String
_Pat_Extract_fun = "fun" :: String
_Pat_Given = "hydra/ext/scala/meta.Pat_Given" :: String
_Pat_Given_tpe = "tpe" :: String
_Pat_Interpolate = "hydra/ext/scala/meta.Pat_Interpolate" :: String
_Pat_Interpolate_parts = "parts" :: String
_Pat_Interpolate_prefix = "prefix" :: String
_Pat_Macro = "hydra/ext/scala/meta.Pat_Macro" :: String
_Pat_Macro_body = "body" :: String
_Pat_Repeated = "hydra/ext/scala/meta.Pat_Repeated" :: String
_Pat_Repeated_name = "name" :: String
_Pat_Tuple = "hydra/ext/scala/meta.Pat_Tuple" :: String
_Pat_Tuple_args = "args" :: String
_Pat_Typed = "hydra/ext/scala/meta.Pat_Typed" :: String
_Pat_Typed_lhs = "lhs" :: String
_Pat_Typed_rhs = "rhs" :: String
_Pat_Var = "hydra/ext/scala/meta.Pat_Var" :: String
_Pat_Var_name = "name" :: String
_Pat_Xml = "hydra/ext/scala/meta.Pat_Xml" :: String
_Pat_Xml_args = "args" :: String
_Pat_Xml_parts = "parts" :: String
_Pat_alternative = "alternative" :: String
_Pat_bind = "bind" :: String
_Pat_extract = "extract" :: String
_Pat_extractInfix = "extractInfix" :: String
_Pat_given = "given" :: String
_Pat_interpolate = "interpolate" :: String
_Pat_macro = "macro" :: String
_Pat_repeated = "repeated" :: String
_Pat_seqWildcard = "seqWildcard" :: String
_Pat_tuple = "tuple" :: String
_Pat_typed = "typed" :: String
_Pat_var = "var" :: String
_Pat_wildcard = "wildcard" :: String
_Pat_xml = "xml" :: String
_Pkg = "hydra/ext/scala/meta.Pkg" :: String
_Pkg_Object = "hydra/ext/scala/meta.Pkg_Object" :: String
_Pkg_Object_mods = "mods" :: String
_Pkg_Object_name = "name" :: String
_Pkg_Object_template = "template" :: String
_Pkg_name = "name" :: String
_Pkg_ref = "ref" :: String
_Pkg_stats = "stats" :: String
_PredefString = "hydra/ext/scala/meta.PredefString" :: String
_Quasi = "hydra/ext/scala/meta.Quasi" :: String
_Ref = "hydra/ext/scala/meta.Ref" :: String
_Ref_init = "init" :: String
_Ref_name = "name" :: String
_ScalaSymbol = "hydra/ext/scala/meta.ScalaSymbol" :: String
_ScalaSymbol_name = "name" :: String
_Self = "hydra/ext/scala/meta.Self" :: String
_Source = "hydra/ext/scala/meta.Source" :: String
_Source_stats = "stats" :: String
_Stat = "hydra/ext/scala/meta.Stat" :: String
_Stat_decl = "decl" :: String
_Stat_defn = "defn" :: String
_Stat_importExportStat = "importExportStat" :: String
_Stat_term = "term" :: String
_Template = "hydra/ext/scala/meta.Template" :: String
_Template_early = "early" :: String
_Template_inits = "inits" :: String
_Template_self = "self" :: String
_Template_stats = "stats" :: String
_Term = "hydra/ext/scala/meta.Term" :: String
_Term_Annotate = "hydra/ext/scala/meta.Term_Annotate" :: String
_Term_Annotate_annots = "annots" :: String
_Term_Annotate_expr = "expr" :: String
_Term_Anonymous = "hydra/ext/scala/meta.Term_Anonymous" :: String
_Term_Apply = "hydra/ext/scala/meta.Term_Apply" :: String
_Term_ApplyInfix = "hydra/ext/scala/meta.Term_ApplyInfix" :: String
_Term_ApplyInfix_args = "args" :: String
_Term_ApplyInfix_lhs = "lhs" :: String
_Term_ApplyInfix_op = "op" :: String
_Term_ApplyInfix_targs = "targs" :: String
_Term_ApplyType = "hydra/ext/scala/meta.Term_ApplyType" :: String
_Term_ApplyType_args = "args" :: String
_Term_ApplyType_lhs = "lhs" :: String
_Term_ApplyType_op = "op" :: String
_Term_ApplyType_targs = "targs" :: String
_Term_ApplyUnary = "hydra/ext/scala/meta.Term_ApplyUnary" :: String
_Term_ApplyUnary_arg = "arg" :: String
_Term_ApplyUnary_op = "op" :: String
_Term_ApplyUsing = "hydra/ext/scala/meta.Term_ApplyUsing" :: String
_Term_ApplyUsing_fun = "fun" :: String
_Term_ApplyUsing_targs = "targs" :: String
_Term_Apply_args = "args" :: String
_Term_Apply_fun = "fun" :: String
_Term_Ascribe = "hydra/ext/scala/meta.Term_Ascribe" :: String
_Term_Ascribe_expr = "expr" :: String
_Term_Ascribe_tpe = "tpe" :: String
_Term_Assign = "hydra/ext/scala/meta.Term_Assign" :: String
_Term_Assign_lhs = "lhs" :: String
_Term_Assign_rhs = "rhs" :: String
_Term_Block = "hydra/ext/scala/meta.Term_Block" :: String
_Term_Block_stats = "stats" :: String
_Term_ContextFunction = "hydra/ext/scala/meta.Term_ContextFunction" :: String
_Term_ContextFunction_body = "body" :: String
_Term_ContextFunction_params = "params" :: String
_Term_Do = "hydra/ext/scala/meta.Term_Do" :: String
_Term_Do_body = "body" :: String
_Term_Do_expr = "expr" :: String
_Term_EndMarker = "hydra/ext/scala/meta.Term_EndMarker" :: String
_Term_EndMarker_name = "name" :: String
_Term_Eta = "hydra/ext/scala/meta.Term_Eta" :: String
_Term_Eta_expr = "expr" :: String
_Term_For = "hydra/ext/scala/meta.Term_For" :: String
_Term_ForYield = "hydra/ext/scala/meta.Term_ForYield" :: String
_Term_ForYield_enums = "enums" :: String
_Term_For_enums = "enums" :: String
_Term_Function = "hydra/ext/scala/meta.Term_Function" :: String
_Term_FunctionTerm = "hydra/ext/scala/meta.Term_FunctionTerm" :: String
_Term_FunctionTerm_Function = "Function" :: String
_Term_FunctionTerm_contextFunction = "contextFunction" :: String
_Term_Function_body = "body" :: String
_Term_Function_params = "params" :: String
_Term_If = "hydra/ext/scala/meta.Term_If" :: String
_Term_If_cond = "cond" :: String
_Term_If_elsep = "elsep" :: String
_Term_If_thenp = "thenp" :: String
_Term_Interpolate = "hydra/ext/scala/meta.Term_Interpolate" :: String
_Term_Interpolate_args = "args" :: String
_Term_Interpolate_parts = "parts" :: String
_Term_Interpolate_prefix = "prefix" :: String
_Term_Match = "hydra/ext/scala/meta.Term_Match" :: String
_Term_Match_cases = "cases" :: String
_Term_Match_expr = "expr" :: String
_Term_Name = "hydra/ext/scala/meta.Term_Name" :: String
_Term_Name_value = "value" :: String
_Term_New = "hydra/ext/scala/meta.Term_New" :: String
_Term_NewAnonymous = "hydra/ext/scala/meta.Term_NewAnonymous" :: String
_Term_NewAnonymous_templ = "templ" :: String
_Term_New_init = "init" :: String
_Term_Param = "hydra/ext/scala/meta.Term_Param" :: String
_Term_Param_mods = "mods" :: String
_Term_Param_name = "name" :: String
_Term_PartialFunction = "hydra/ext/scala/meta.Term_PartialFunction" :: String
_Term_PartialFunction_cases = "cases" :: String
_Term_Placeholder = "hydra/ext/scala/meta.Term_Placeholder" :: String
_Term_PolyFunction = "hydra/ext/scala/meta.Term_PolyFunction" :: String
_Term_PolyFunction_body = "body" :: String
_Term_PolyFunction_tparams = "tparams" :: String
_Term_QuotedMacroExpr = "hydra/ext/scala/meta.Term_QuotedMacroExpr" :: String
_Term_QuotedMacroExpr_body = "body" :: String
_Term_QuotedMacroType = "hydra/ext/scala/meta.Term_QuotedMacroType" :: String
_Term_QuotedMacroType_tpe = "tpe" :: String
_Term_Ref = "hydra/ext/scala/meta.Term_Ref" :: String
_Term_Ref_anonymous = "anonymous" :: String
_Term_Ref_applyUnary = "applyUnary" :: String
_Term_Ref_name = "name" :: String
_Term_Ref_select = "select" :: String
_Term_Ref_super = "super" :: String
_Term_Ref_this = "this" :: String
_Term_Repeated = "hydra/ext/scala/meta.Term_Repeated" :: String
_Term_Repeated_expr = "expr" :: String
_Term_Return = "hydra/ext/scala/meta.Term_Return" :: String
_Term_Return_expr = "expr" :: String
_Term_Select = "hydra/ext/scala/meta.Term_Select" :: String
_Term_Select_name = "name" :: String
_Term_Select_qual = "qual" :: String
_Term_SplicedMacroExpr = "hydra/ext/scala/meta.Term_SplicedMacroExpr" :: String
_Term_SplicedMacroExpr_body = "body" :: String
_Term_Super = "hydra/ext/scala/meta.Term_Super" :: String
_Term_Super_superp = "superp" :: String
_Term_Super_thisp = "thisp" :: String
_Term_This = "hydra/ext/scala/meta.Term_This" :: String
_Term_Throw = "hydra/ext/scala/meta.Term_Throw" :: String
_Term_Throw_expr = "expr" :: String
_Term_Try = "hydra/ext/scala/meta.Term_Try" :: String
_Term_TryWithHandler = "hydra/ext/scala/meta.Term_TryWithHandler" :: String
_Term_TryWithHandler_catchp = "catchp" :: String
_Term_TryWithHandler_expr = "expr" :: String
_Term_TryWithHandler_finallyp = "finallyp" :: String
_Term_Try_catchp = "catchp" :: String
_Term_Try_expr = "expr" :: String
_Term_Try_finallyp = "finallyp" :: String
_Term_Tuple = "hydra/ext/scala/meta.Term_Tuple" :: String
_Term_Tuple_args = "args" :: String
_Term_While = "hydra/ext/scala/meta.Term_While" :: String
_Term_While_body = "body" :: String
_Term_While_expr = "expr" :: String
_Term_Xml = "hydra/ext/scala/meta.Term_Xml" :: String
_Term_Xml_args = "args" :: String
_Term_Xml_parts = "parts" :: String
_Term_annotate = "annotate" :: String
_Term_apply = "apply" :: String
_Term_applyInfix = "applyInfix" :: String
_Term_applyType = "applyType" :: String
_Term_applyUsing = "applyUsing" :: String
_Term_ascribe = "ascribe" :: String
_Term_assign = "assign" :: String
_Term_block = "block" :: String
_Term_do = "do" :: String
_Term_endMarker = "endMarker" :: String
_Term_eta = "eta" :: String
_Term_for = "for" :: String
_Term_forYield = "forYield" :: String
_Term_functionTerm = "functionTerm" :: String
_Term_if = "if" :: String
_Term_interpolate = "interpolate" :: String
_Term_lit = "lit" :: String
_Term_match = "match" :: String
_Term_new = "new" :: String
_Term_newAnonymous = "newAnonymous" :: String
_Term_partialFunction = "partialFunction" :: String
_Term_placeholder = "placeholder" :: String
_Term_polyFunction = "polyFunction" :: String
_Term_quotedMacroExpr = "quotedMacroExpr" :: String
_Term_quotedMacroType = "quotedMacroType" :: String
_Term_ref = "ref" :: String
_Term_repeated = "repeated" :: String
_Term_return = "return" :: String
_Term_splicedMacroExpr = "splicedMacroExpr" :: String
_Term_throw = "throw" :: String
_Term_try = "try" :: String
_Term_tryWithHandler = "tryWithHandler" :: String
_Term_tuple = "tuple" :: String
_Term_while = "while" :: String
_Term_xml = "xml" :: String
_Tree = "hydra/ext/scala/meta.Tree" :: String
_Tree_bounds = "bounds" :: String
_Tree_caseTree = "caseTree" :: String
_Tree_ctor = "ctor" :: String
_Tree_enumerator = "enumerator" :: String
_Tree_importee = "importee" :: String
_Tree_importer = "importer" :: String
_Tree_member = "member" :: String
_Tree_mod = "mod" :: String
_Tree_pat = "pat" :: String
_Tree_quasi = "quasi" :: String
_Tree_ref = "ref" :: String
_Tree_source = "source" :: String
_Tree_stat = "stat" :: String
_Tree_template = "template" :: String
_Tree_type = "type" :: String
_Type = "hydra/ext/scala/meta.Type" :: String
_TypeCase = "hydra/ext/scala/meta.TypeCase" :: String
_TypeCase_body = "body" :: String
_TypeCase_pat = "pat" :: String
_Type_And = "hydra/ext/scala/meta.Type_And" :: String
_Type_And_lhs = "lhs" :: String
_Type_And_rhs = "rhs" :: String
_Type_Annotate = "hydra/ext/scala/meta.Type_Annotate" :: String
_Type_Annotate_annots = "annots" :: String
_Type_Annotate_tpe = "tpe" :: String
_Type_AnonymousName = "hydra/ext/scala/meta.Type_AnonymousName" :: String
_Type_Apply = "hydra/ext/scala/meta.Type_Apply" :: String
_Type_ApplyInfix = "hydra/ext/scala/meta.Type_ApplyInfix" :: String
_Type_ApplyInfix_lhs = "lhs" :: String
_Type_ApplyInfix_op = "op" :: String
_Type_ApplyInfix_rhs = "rhs" :: String
_Type_Apply_args = "args" :: String
_Type_Apply_tpe = "tpe" :: String
_Type_Bounds = "hydra/ext/scala/meta.Type_Bounds" :: String
_Type_Bounds_hi = "hi" :: String
_Type_Bounds_lo = "lo" :: String
_Type_ByName = "hydra/ext/scala/meta.Type_ByName" :: String
_Type_ContextFunction = "hydra/ext/scala/meta.Type_ContextFunction" :: String
_Type_ContextFunction_params = "params" :: String
_Type_ContextFunction_res = "res" :: String
_Type_Existential = "hydra/ext/scala/meta.Type_Existential" :: String
_Type_Existential_stats = "stats" :: String
_Type_Existential_tpe = "tpe" :: String
_Type_Function = "hydra/ext/scala/meta.Type_Function" :: String
_Type_FunctionType = "hydra/ext/scala/meta.Type_FunctionType" :: String
_Type_FunctionType_contextFunction = "contextFunction" :: String
_Type_FunctionType_function = "function" :: String
_Type_Function_params = "params" :: String
_Type_Function_res = "res" :: String
_Type_ImplicitFunction = "hydra/ext/scala/meta.Type_ImplicitFunction" :: String
_Type_ImplicitFunction_params = "params" :: String
_Type_ImplicitFunction_res = "res" :: String
_Type_Lambda = "hydra/ext/scala/meta.Type_Lambda" :: String
_Type_Lambda_tparams = "tparams" :: String
_Type_Lambda_tpe = "tpe" :: String
_Type_Macro = "hydra/ext/scala/meta.Type_Macro" :: String
_Type_Macro_body = "body" :: String
_Type_Match = "hydra/ext/scala/meta.Type_Match" :: String
_Type_Match_cases = "cases" :: String
_Type_Match_tpe = "tpe" :: String
_Type_Method = "hydra/ext/scala/meta.Type_Method" :: String
_Type_Method_paramss = "paramss" :: String
_Type_Method_tpe = "tpe" :: String
_Type_Name = "hydra/ext/scala/meta.Type_Name" :: String
_Type_Name_value = "value" :: String
_Type_Or = "hydra/ext/scala/meta.Type_Or" :: String
_Type_Or_lhs = "lhs" :: String
_Type_Or_rhs = "rhs" :: String
_Type_Param = "hydra/ext/scala/meta.Type_Param" :: String
_Type_Param_cbounds = "cbounds" :: String
_Type_Param_mods = "mods" :: String
_Type_Param_name = "name" :: String
_Type_Param_tbounds = "tbounds" :: String
_Type_Param_tparams = "tparams" :: String
_Type_Param_vbounds = "vbounds" :: String
_Type_Placeholder = "hydra/ext/scala/meta.Type_Placeholder" :: String
_Type_Placeholder_bounds = "bounds" :: String
_Type_PolyFunction = "hydra/ext/scala/meta.Type_PolyFunction" :: String
_Type_PolyFunction_tparams = "tparams" :: String
_Type_PolyFunction_tpe = "tpe" :: String
_Type_Project = "hydra/ext/scala/meta.Type_Project" :: String
_Type_Project_name = "name" :: String
_Type_Project_qual = "qual" :: String
_Type_Ref = "hydra/ext/scala/meta.Type_Ref" :: String
_Type_Ref_name = "name" :: String
_Type_Ref_project = "project" :: String
_Type_Ref_select = "select" :: String
_Type_Ref_singleton = "singleton" :: String
_Type_Refine = "hydra/ext/scala/meta.Type_Refine" :: String
_Type_Refine_stats = "stats" :: String
_Type_Refine_tpe = "tpe" :: String
_Type_Repeated = "hydra/ext/scala/meta.Type_Repeated" :: String
_Type_Repeated_tpe = "tpe" :: String
_Type_Select = "hydra/ext/scala/meta.Type_Select" :: String
_Type_Select_name = "name" :: String
_Type_Select_qual = "qual" :: String
_Type_Singleton = "hydra/ext/scala/meta.Type_Singleton" :: String
_Type_Singleton_ref = "ref" :: String
_Type_Tuple = "hydra/ext/scala/meta.Type_Tuple" :: String
_Type_Tuple_args = "args" :: String
_Type_TypedParam = "hydra/ext/scala/meta.Type_TypedParam" :: String
_Type_TypedParam_name = "name" :: String
_Type_TypedParam_typ = "typ" :: String
_Type_Var = "hydra/ext/scala/meta.Type_Var" :: String
_Type_Var_name = "name" :: String
_Type_With = "hydra/ext/scala/meta.Type_With" :: String
_Type_With_lhs = "lhs" :: String
_Type_With_rhs = "rhs" :: String
_Type_and = "and" :: String
_Type_annotate = "annotate" :: String
_Type_anonymousName = "anonymousName" :: String
_Type_apply = "apply" :: String
_Type_applyInfix = "applyInfix" :: String
_Type_byName = "byName" :: String
_Type_existential = "existential" :: String
_Type_functionType = "functionType" :: String
_Type_implicitFunction = "implicitFunction" :: String
_Type_lambda = "lambda" :: String
_Type_macro = "macro" :: String
_Type_match = "match" :: String
_Type_method = "method" :: String
_Type_or = "or" :: String
_Type_placeholder = "placeholder" :: String
_Type_polyFunction = "polyFunction" :: String
_Type_ref = "ref" :: String
_Type_refine = "refine" :: String
_Type_repeated = "repeated" :: String
_Type_tuple = "tuple" :: String
_Type_typedParam = "typedParam" :: String
_Type_var = "var" :: String
_Type_with = "with" :: String
