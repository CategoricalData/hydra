module Hydra.Ext.Java.Syntax where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

newtype Identifier 
  = Identifier {
    unIdentifier :: String
  }
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra/ext/java/syntax.Identifier")

newtype TypeIdentifier 
  = TypeIdentifier {
    unTypeIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_TypeIdentifier = (Core.Name "hydra/ext/java/syntax.TypeIdentifier")

data Literal 
  = LiteralNull 
  | LiteralInteger IntegerLiteral
  | LiteralFloatingPoint FloatingPointLiteral
  | LiteralBoolean Bool
  | LiteralCharacter Int
  | LiteralString StringLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/ext/java/syntax.Literal")

_Literal_null = (Core.FieldName "null")

_Literal_integer = (Core.FieldName "integer")

_Literal_floatingPoint = (Core.FieldName "floatingPoint")

_Literal_boolean = (Core.FieldName "boolean")

_Literal_character = (Core.FieldName "character")

_Literal_string = (Core.FieldName "string")

-- Note: this is an approximation which ignores encoding
newtype IntegerLiteral 
  = IntegerLiteral {
    unIntegerLiteral :: Integer
  }
  deriving (Eq, Ord, Read, Show)

_IntegerLiteral = (Core.Name "hydra/ext/java/syntax.IntegerLiteral")

-- Note: this is an approximation which ignores encoding
newtype FloatingPointLiteral 
  = FloatingPointLiteral {
    unFloatingPointLiteral :: Double
  }
  deriving (Eq, Ord, Read, Show)

_FloatingPointLiteral = (Core.Name "hydra/ext/java/syntax.FloatingPointLiteral")

-- Note: this is an approximation which ignores encoding
newtype StringLiteral 
  = StringLiteral {
    unStringLiteral :: String
  }
  deriving (Eq, Ord, Read, Show)

_StringLiteral = (Core.Name "hydra/ext/java/syntax.StringLiteral")

data Type 
  = TypePrimitive PrimitiveTypeWithAnnotations
  | TypeReference ReferenceType
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/ext/java/syntax.Type")

_Type_primitive = (Core.FieldName "primitive")

_Type_reference = (Core.FieldName "reference")

data PrimitiveTypeWithAnnotations 
  = PrimitiveTypeWithAnnotations {
    primitiveTypeWithAnnotationsType :: PrimitiveType,
    primitiveTypeWithAnnotationsAnnotations :: [Annotation]
  }
  deriving (Eq, Ord, Read, Show)

_PrimitiveTypeWithAnnotations = (Core.Name "hydra/ext/java/syntax.PrimitiveTypeWithAnnotations")

_PrimitiveTypeWithAnnotations_type = (Core.FieldName "type")

_PrimitiveTypeWithAnnotations_annotations = (Core.FieldName "annotations")

data PrimitiveType 
  = PrimitiveTypeNumeric NumericType
  | PrimitiveTypeBoolean 
  deriving (Eq, Ord, Read, Show)

_PrimitiveType = (Core.Name "hydra/ext/java/syntax.PrimitiveType")

_PrimitiveType_numeric = (Core.FieldName "numeric")

_PrimitiveType_boolean = (Core.FieldName "boolean")

data NumericType 
  = NumericTypeIntegral IntegralType
  | NumericTypeFloatingPoint FloatingPointType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra/ext/java/syntax.NumericType")

_NumericType_integral = (Core.FieldName "integral")

_NumericType_floatingPoint = (Core.FieldName "floatingPoint")

data IntegralType 
  = IntegralTypeByte 
  | IntegralTypeShort 
  | IntegralTypeInt 
  | IntegralTypeLong 
  | IntegralTypeChar 
  deriving (Eq, Ord, Read, Show)

_IntegralType = (Core.Name "hydra/ext/java/syntax.IntegralType")

_IntegralType_byte = (Core.FieldName "byte")

_IntegralType_short = (Core.FieldName "short")

_IntegralType_int = (Core.FieldName "int")

_IntegralType_long = (Core.FieldName "long")

_IntegralType_char = (Core.FieldName "char")

data FloatingPointType 
  = FloatingPointTypeFloat 
  | FloatingPointTypeDouble 
  deriving (Eq, Ord, Read, Show)

_FloatingPointType = (Core.Name "hydra/ext/java/syntax.FloatingPointType")

_FloatingPointType_float = (Core.FieldName "float")

_FloatingPointType_double = (Core.FieldName "double")

data ReferenceType 
  = ReferenceTypeClassOrInterface ClassOrInterfaceType
  | ReferenceTypeVariable TypeVariable
  | ReferenceTypeArray ArrayType
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra/ext/java/syntax.ReferenceType")

_ReferenceType_classOrInterface = (Core.FieldName "classOrInterface")

_ReferenceType_variable = (Core.FieldName "variable")

_ReferenceType_array = (Core.FieldName "array")

data ClassOrInterfaceType 
  = ClassOrInterfaceTypeClass ClassType
  | ClassOrInterfaceTypeInterface InterfaceType
  deriving (Eq, Ord, Read, Show)

_ClassOrInterfaceType = (Core.Name "hydra/ext/java/syntax.ClassOrInterfaceType")

_ClassOrInterfaceType_class = (Core.FieldName "class")

_ClassOrInterfaceType_interface = (Core.FieldName "interface")

data ClassType 
  = ClassType {
    classTypeAnnotations :: [Annotation],
    classTypeQualifier :: ClassTypeQualifier,
    classTypeIdentifier :: TypeIdentifier,
    classTypeArguments :: [TypeArgument]
  }
  deriving (Eq, Ord, Read, Show)

_ClassType = (Core.Name "hydra/ext/java/syntax.ClassType")

_ClassType_annotations = (Core.FieldName "annotations")

_ClassType_qualifier = (Core.FieldName "qualifier")

_ClassType_identifier = (Core.FieldName "identifier")

_ClassType_arguments = (Core.FieldName "arguments")

data ClassTypeQualifier 
  = ClassTypeQualifierNone 
  | ClassTypeQualifierPackage PackageName
  | ClassTypeQualifierParent ClassOrInterfaceType
  deriving (Eq, Ord, Read, Show)

_ClassTypeQualifier = (Core.Name "hydra/ext/java/syntax.ClassTypeQualifier")

_ClassTypeQualifier_none = (Core.FieldName "none")

_ClassTypeQualifier_package = (Core.FieldName "package")

_ClassTypeQualifier_parent = (Core.FieldName "parent")

newtype InterfaceType 
  = InterfaceType {
    unInterfaceType :: ClassType
  }
  deriving (Eq, Ord, Read, Show)

_InterfaceType = (Core.Name "hydra/ext/java/syntax.InterfaceType")

data TypeVariable 
  = TypeVariable {
    typeVariableAnnotations :: [Annotation],
    typeVariableIdentifier :: TypeIdentifier
  }
  deriving (Eq, Ord, Read, Show)

_TypeVariable = (Core.Name "hydra/ext/java/syntax.TypeVariable")

_TypeVariable_annotations = (Core.FieldName "annotations")

_TypeVariable_identifier = (Core.FieldName "identifier")

data ArrayType 
  = ArrayType {
    arrayTypeDims :: Dims,
    arrayTypeVariant :: ArrayType_Variant
  }
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra/ext/java/syntax.ArrayType")

_ArrayType_dims = (Core.FieldName "dims")

_ArrayType_variant = (Core.FieldName "variant")

data ArrayType_Variant 
  = ArrayType_VariantPrimitive PrimitiveTypeWithAnnotations
  | ArrayType_VariantClassOrInterface ClassOrInterfaceType
  | ArrayType_VariantVariable TypeVariable
  deriving (Eq, Ord, Read, Show)

_ArrayType_Variant = (Core.Name "hydra/ext/java/syntax.ArrayType.Variant")

_ArrayType_Variant_primitive = (Core.FieldName "primitive")

_ArrayType_Variant_classOrInterface = (Core.FieldName "classOrInterface")

_ArrayType_Variant_variable = (Core.FieldName "variable")

newtype Dims 
  = Dims {
    unDims :: [[Annotation]]
  }
  deriving (Eq, Ord, Read, Show)

_Dims = (Core.Name "hydra/ext/java/syntax.Dims")

data TypeParameter 
  = TypeParameter {
    typeParameterModifiers :: [TypeParameterModifier],
    typeParameterIdentifier :: TypeIdentifier,
    typeParameterBound :: (Maybe TypeBound)
  }
  deriving (Eq, Ord, Read, Show)

_TypeParameter = (Core.Name "hydra/ext/java/syntax.TypeParameter")

_TypeParameter_modifiers = (Core.FieldName "modifiers")

_TypeParameter_identifier = (Core.FieldName "identifier")

_TypeParameter_bound = (Core.FieldName "bound")

newtype TypeParameterModifier 
  = TypeParameterModifier {
    unTypeParameterModifier :: Annotation
  }
  deriving (Eq, Ord, Read, Show)

_TypeParameterModifier = (Core.Name "hydra/ext/java/syntax.TypeParameterModifier")

data TypeBound 
  = TypeBoundVariable TypeVariable
  | TypeBoundClassOrInterface TypeBound_ClassOrInterface
  deriving (Eq, Ord, Read, Show)

_TypeBound = (Core.Name "hydra/ext/java/syntax.TypeBound")

_TypeBound_variable = (Core.FieldName "variable")

_TypeBound_classOrInterface = (Core.FieldName "classOrInterface")

data TypeBound_ClassOrInterface 
  = TypeBound_ClassOrInterface {
    typeBound_ClassOrInterfaceType :: ClassOrInterfaceType,
    typeBound_ClassOrInterfaceAdditional :: [AdditionalBound]
  }
  deriving (Eq, Ord, Read, Show)

_TypeBound_ClassOrInterface = (Core.Name "hydra/ext/java/syntax.TypeBound.ClassOrInterface")

_TypeBound_ClassOrInterface_type = (Core.FieldName "type")

_TypeBound_ClassOrInterface_additional = (Core.FieldName "additional")

newtype AdditionalBound 
  = AdditionalBound {
    unAdditionalBound :: InterfaceType
  }
  deriving (Eq, Ord, Read, Show)

_AdditionalBound = (Core.Name "hydra/ext/java/syntax.AdditionalBound")

data TypeArgument 
  = TypeArgumentReference ReferenceType
  | TypeArgumentWildcard Wildcard
  deriving (Eq, Ord, Read, Show)

_TypeArgument = (Core.Name "hydra/ext/java/syntax.TypeArgument")

_TypeArgument_reference = (Core.FieldName "reference")

_TypeArgument_wildcard = (Core.FieldName "wildcard")

data Wildcard 
  = Wildcard {
    wildcardAnnotations :: [Annotation],
    wildcardWildcard :: (Maybe WildcardBounds)
  }
  deriving (Eq, Ord, Read, Show)

_Wildcard = (Core.Name "hydra/ext/java/syntax.Wildcard")

_Wildcard_annotations = (Core.FieldName "annotations")

_Wildcard_wildcard = (Core.FieldName "wildcard")

data WildcardBounds 
  = WildcardBoundsExtends ReferenceType
  | WildcardBoundsSuper ReferenceType
  deriving (Eq, Ord, Read, Show)

_WildcardBounds = (Core.Name "hydra/ext/java/syntax.WildcardBounds")

_WildcardBounds_extends = (Core.FieldName "extends")

_WildcardBounds_super = (Core.FieldName "super")

data ModuleName 
  = ModuleName {
    moduleNameIdentifier :: Identifier,
    moduleNameName :: (Maybe ModuleName)
  }
  deriving (Eq, Ord, Read, Show)

_ModuleName = (Core.Name "hydra/ext/java/syntax.ModuleName")

_ModuleName_identifier = (Core.FieldName "identifier")

_ModuleName_name = (Core.FieldName "name")

newtype PackageName 
  = PackageName {
    unPackageName :: [Identifier]
  }
  deriving (Eq, Ord, Read, Show)

_PackageName = (Core.Name "hydra/ext/java/syntax.PackageName")

data TypeName 
  = TypeName {
    typeNameIdentifier :: TypeIdentifier,
    typeNameQualifier :: (Maybe PackageOrTypeName)
  }
  deriving (Eq, Ord, Read, Show)

_TypeName = (Core.Name "hydra/ext/java/syntax.TypeName")

_TypeName_identifier = (Core.FieldName "identifier")

_TypeName_qualifier = (Core.FieldName "qualifier")

data ExpressionName 
  = ExpressionName {
    expressionNameIdentifier :: Identifier,
    expressionNameName :: (Maybe AmbiguousName)
  }
  deriving (Eq, Ord, Read, Show)

_ExpressionName = (Core.Name "hydra/ext/java/syntax.ExpressionName")

_ExpressionName_identifier = (Core.FieldName "identifier")

_ExpressionName_name = (Core.FieldName "name")

newtype MethodName 
  = MethodName {
    unMethodName :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_MethodName = (Core.Name "hydra/ext/java/syntax.MethodName")

newtype PackageOrTypeName 
  = PackageOrTypeName {
    unPackageOrTypeName :: [Identifier]
  }
  deriving (Eq, Ord, Read, Show)

_PackageOrTypeName = (Core.Name "hydra/ext/java/syntax.PackageOrTypeName")

newtype AmbiguousName 
  = AmbiguousName {
    unAmbiguousName :: [Identifier]
  }
  deriving (Eq, Ord, Read, Show)

_AmbiguousName = (Core.Name "hydra/ext/java/syntax.AmbiguousName")

data CompilationUnit 
  = CompilationUnitOrdinary OrdinaryCompilationUnit
  | CompilationUnitModular ModularCompilationUnit
  deriving (Eq, Ord, Read, Show)

_CompilationUnit = (Core.Name "hydra/ext/java/syntax.CompilationUnit")

_CompilationUnit_ordinary = (Core.FieldName "ordinary")

_CompilationUnit_modular = (Core.FieldName "modular")

data OrdinaryCompilationUnit 
  = OrdinaryCompilationUnit {
    ordinaryCompilationUnitPackage :: (Maybe PackageDeclaration),
    ordinaryCompilationUnitImports :: [ImportDeclaration],
    ordinaryCompilationUnitTypes :: [TypeDeclaration]
  }
  deriving (Eq, Ord, Read, Show)

_OrdinaryCompilationUnit = (Core.Name "hydra/ext/java/syntax.OrdinaryCompilationUnit")

_OrdinaryCompilationUnit_package = (Core.FieldName "package")

_OrdinaryCompilationUnit_imports = (Core.FieldName "imports")

_OrdinaryCompilationUnit_types = (Core.FieldName "types")

data ModularCompilationUnit 
  = ModularCompilationUnit {
    modularCompilationUnitImports :: [ImportDeclaration],
    modularCompilationUnitModule :: ModuleDeclaration
  }
  deriving (Eq, Ord, Read, Show)

_ModularCompilationUnit = (Core.Name "hydra/ext/java/syntax.ModularCompilationUnit")

_ModularCompilationUnit_imports = (Core.FieldName "imports")

_ModularCompilationUnit_module = (Core.FieldName "module")

data PackageDeclaration 
  = PackageDeclaration {
    packageDeclarationModifiers :: [PackageModifier],
    packageDeclarationIdentifiers :: [Identifier]
  }
  deriving (Eq, Ord, Read, Show)

_PackageDeclaration = (Core.Name "hydra/ext/java/syntax.PackageDeclaration")

_PackageDeclaration_modifiers = (Core.FieldName "modifiers")

_PackageDeclaration_identifiers = (Core.FieldName "identifiers")

newtype PackageModifier 
  = PackageModifier {
    unPackageModifier :: Annotation
  }
  deriving (Eq, Ord, Read, Show)

_PackageModifier = (Core.Name "hydra/ext/java/syntax.PackageModifier")

data ImportDeclaration 
  = ImportDeclarationSingleType SingleTypeImportDeclaration
  | ImportDeclarationTypeImportOnDemand TypeImportOnDemandDeclaration
  | ImportDeclarationSingleStaticImport SingleStaticImportDeclaration
  | ImportDeclarationStaticImportOnDemand StaticImportOnDemandDeclaration
  deriving (Eq, Ord, Read, Show)

_ImportDeclaration = (Core.Name "hydra/ext/java/syntax.ImportDeclaration")

_ImportDeclaration_singleType = (Core.FieldName "singleType")

_ImportDeclaration_typeImportOnDemand = (Core.FieldName "typeImportOnDemand")

_ImportDeclaration_singleStaticImport = (Core.FieldName "singleStaticImport")

_ImportDeclaration_staticImportOnDemand = (Core.FieldName "staticImportOnDemand")

newtype SingleTypeImportDeclaration 
  = SingleTypeImportDeclaration {
    unSingleTypeImportDeclaration :: TypeName
  }
  deriving (Eq, Ord, Read, Show)

_SingleTypeImportDeclaration = (Core.Name "hydra/ext/java/syntax.SingleTypeImportDeclaration")

newtype TypeImportOnDemandDeclaration 
  = TypeImportOnDemandDeclaration {
    unTypeImportOnDemandDeclaration :: PackageOrTypeName
  }
  deriving (Eq, Ord, Read, Show)

_TypeImportOnDemandDeclaration = (Core.Name "hydra/ext/java/syntax.TypeImportOnDemandDeclaration")

data SingleStaticImportDeclaration 
  = SingleStaticImportDeclaration {
    singleStaticImportDeclarationTypeName :: TypeName,
    singleStaticImportDeclarationIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_SingleStaticImportDeclaration = (Core.Name "hydra/ext/java/syntax.SingleStaticImportDeclaration")

_SingleStaticImportDeclaration_typeName = (Core.FieldName "typeName")

_SingleStaticImportDeclaration_identifier = (Core.FieldName "identifier")

newtype StaticImportOnDemandDeclaration 
  = StaticImportOnDemandDeclaration {
    unStaticImportOnDemandDeclaration :: TypeName
  }
  deriving (Eq, Ord, Read, Show)

_StaticImportOnDemandDeclaration = (Core.Name "hydra/ext/java/syntax.StaticImportOnDemandDeclaration")

data TypeDeclaration 
  = TypeDeclarationClass ClassDeclaration
  | TypeDeclarationInterface InterfaceDeclaration
  deriving (Eq, Ord, Read, Show)

_TypeDeclaration = (Core.Name "hydra/ext/java/syntax.TypeDeclaration")

_TypeDeclaration_class = (Core.FieldName "class")

_TypeDeclaration_interface = (Core.FieldName "interface")

data ModuleDeclaration 
  = ModuleDeclaration {
    moduleDeclarationAnnotations :: [Annotation],
    moduleDeclarationOpen :: Bool,
    moduleDeclarationIdentifiers :: [Identifier],
    moduleDeclarationDirectives :: [[ModuleDirective]]
  }
  deriving (Eq, Ord, Read, Show)

_ModuleDeclaration = (Core.Name "hydra/ext/java/syntax.ModuleDeclaration")

_ModuleDeclaration_annotations = (Core.FieldName "annotations")

_ModuleDeclaration_open = (Core.FieldName "open")

_ModuleDeclaration_identifiers = (Core.FieldName "identifiers")

_ModuleDeclaration_directives = (Core.FieldName "directives")

data ModuleDirective 
  = ModuleDirectiveRequires ModuleDirective_Requires
  | ModuleDirectiveExports ModuleDirective_ExportsOrOpens
  | ModuleDirectiveOpens ModuleDirective_ExportsOrOpens
  | ModuleDirectiveUses TypeName
  | ModuleDirectiveProvides ModuleDirective_Provides
  deriving (Eq, Ord, Read, Show)

_ModuleDirective = (Core.Name "hydra/ext/java/syntax.ModuleDirective")

_ModuleDirective_requires = (Core.FieldName "requires")

_ModuleDirective_exports = (Core.FieldName "exports")

_ModuleDirective_opens = (Core.FieldName "opens")

_ModuleDirective_uses = (Core.FieldName "uses")

_ModuleDirective_provides = (Core.FieldName "provides")

data ModuleDirective_Requires 
  = ModuleDirective_Requires {
    moduleDirective_RequiresModifiers :: [RequiresModifier],
    moduleDirective_RequiresModule :: ModuleName
  }
  deriving (Eq, Ord, Read, Show)

_ModuleDirective_Requires = (Core.Name "hydra/ext/java/syntax.ModuleDirective.Requires")

_ModuleDirective_Requires_modifiers = (Core.FieldName "modifiers")

_ModuleDirective_Requires_module = (Core.FieldName "module")

data ModuleDirective_ExportsOrOpens 
  = ModuleDirective_ExportsOrOpens {
    moduleDirective_ExportsOrOpensPackage :: PackageName,
    moduleDirective_ExportsOrOpensModules :: [ModuleName]
  }
  deriving (Eq, Ord, Read, Show)

_ModuleDirective_ExportsOrOpens = (Core.Name "hydra/ext/java/syntax.ModuleDirective.ExportsOrOpens")

_ModuleDirective_ExportsOrOpens_package = (Core.FieldName "package")

_ModuleDirective_ExportsOrOpens_modules = (Core.FieldName "modules")

data ModuleDirective_Provides 
  = ModuleDirective_Provides {
    moduleDirective_ProvidesTo :: TypeName,
    moduleDirective_ProvidesWith :: [TypeName]
  }
  deriving (Eq, Ord, Read, Show)

_ModuleDirective_Provides = (Core.Name "hydra/ext/java/syntax.ModuleDirective.Provides")

_ModuleDirective_Provides_to = (Core.FieldName "to")

_ModuleDirective_Provides_with = (Core.FieldName "with")

data RequiresModifier 
  = RequiresModifierTransitive 
  | RequiresModifierStatic 
  deriving (Eq, Ord, Read, Show)

_RequiresModifier = (Core.Name "hydra/ext/java/syntax.RequiresModifier")

_RequiresModifier_transitive = (Core.FieldName "transitive")

_RequiresModifier_static = (Core.FieldName "static")

data ClassDeclaration 
  = ClassDeclarationNormal NormalClassDeclaration
  | ClassDeclarationEnum EnumDeclaration
  deriving (Eq, Ord, Read, Show)

_ClassDeclaration = (Core.Name "hydra/ext/java/syntax.ClassDeclaration")

_ClassDeclaration_normal = (Core.FieldName "normal")

_ClassDeclaration_enum = (Core.FieldName "enum")

data NormalClassDeclaration 
  = NormalClassDeclaration {
    normalClassDeclarationModifiers :: [ClassModifier],
    normalClassDeclarationIdentifier :: TypeIdentifier,
    normalClassDeclarationParameters :: [TypeParameter],
    normalClassDeclarationExtends :: (Maybe ClassType),
    normalClassDeclarationImplements :: [InterfaceType],
    normalClassDeclarationBody :: ClassBody
  }
  deriving (Eq, Ord, Read, Show)

_NormalClassDeclaration = (Core.Name "hydra/ext/java/syntax.NormalClassDeclaration")

_NormalClassDeclaration_modifiers = (Core.FieldName "modifiers")

_NormalClassDeclaration_identifier = (Core.FieldName "identifier")

_NormalClassDeclaration_parameters = (Core.FieldName "parameters")

_NormalClassDeclaration_extends = (Core.FieldName "extends")

_NormalClassDeclaration_implements = (Core.FieldName "implements")

_NormalClassDeclaration_body = (Core.FieldName "body")

data ClassModifier 
  = ClassModifierAnnotation Annotation
  | ClassModifierPublic 
  | ClassModifierProtected 
  | ClassModifierPrivate 
  | ClassModifierAbstract 
  | ClassModifierStatic 
  | ClassModifierFinal 
  | ClassModifierStrictfp 
  deriving (Eq, Ord, Read, Show)

_ClassModifier = (Core.Name "hydra/ext/java/syntax.ClassModifier")

_ClassModifier_annotation = (Core.FieldName "annotation")

_ClassModifier_public = (Core.FieldName "public")

_ClassModifier_protected = (Core.FieldName "protected")

_ClassModifier_private = (Core.FieldName "private")

_ClassModifier_abstract = (Core.FieldName "abstract")

_ClassModifier_static = (Core.FieldName "static")

_ClassModifier_final = (Core.FieldName "final")

_ClassModifier_strictfp = (Core.FieldName "strictfp")

newtype ClassBody 
  = ClassBody {
    unClassBody :: [ClassBodyDeclaration]
  }
  deriving (Eq, Ord, Read, Show)

_ClassBody = (Core.Name "hydra/ext/java/syntax.ClassBody")

data ClassBodyDeclaration 
  = ClassBodyDeclarationClassMember ClassMemberDeclaration
  | ClassBodyDeclarationInstanceInitializer InstanceInitializer
  | ClassBodyDeclarationStaticInitializer StaticInitializer
  | ClassBodyDeclarationConstructorDeclaration ConstructorDeclaration
  deriving (Eq, Ord, Read, Show)

_ClassBodyDeclaration = (Core.Name "hydra/ext/java/syntax.ClassBodyDeclaration")

_ClassBodyDeclaration_classMember = (Core.FieldName "classMember")

_ClassBodyDeclaration_instanceInitializer = (Core.FieldName "instanceInitializer")

_ClassBodyDeclaration_staticInitializer = (Core.FieldName "staticInitializer")

_ClassBodyDeclaration_constructorDeclaration = (Core.FieldName "constructorDeclaration")

data ClassMemberDeclaration 
  = ClassMemberDeclarationField FieldDeclaration
  | ClassMemberDeclarationMethod MethodDeclaration
  | ClassMemberDeclarationClass ClassDeclaration
  | ClassMemberDeclarationInterface InterfaceDeclaration
  deriving (Eq, Ord, Read, Show)

_ClassMemberDeclaration = (Core.Name "hydra/ext/java/syntax.ClassMemberDeclaration")

_ClassMemberDeclaration_field = (Core.FieldName "field")

_ClassMemberDeclaration_method = (Core.FieldName "method")

_ClassMemberDeclaration_class = (Core.FieldName "class")

_ClassMemberDeclaration_interface = (Core.FieldName "interface")

data FieldDeclaration 
  = FieldDeclaration {
    fieldDeclarationModifiers :: [FieldModifier],
    fieldDeclarationUnannType :: UnannType,
    fieldDeclarationVariableDeclarators :: [VariableDeclarator]
  }
  deriving (Eq, Ord, Read, Show)

_FieldDeclaration = (Core.Name "hydra/ext/java/syntax.FieldDeclaration")

_FieldDeclaration_modifiers = (Core.FieldName "modifiers")

_FieldDeclaration_unannType = (Core.FieldName "unannType")

_FieldDeclaration_variableDeclarators = (Core.FieldName "variableDeclarators")

data FieldModifier 
  = FieldModifierAnnotation Annotation
  | FieldModifierPublic 
  | FieldModifierProtected 
  | FieldModifierPrivate 
  | FieldModifierStatic 
  | FieldModifierFinal 
  | FieldModifierTransient 
  | FieldModifierVolatile 
  deriving (Eq, Ord, Read, Show)

_FieldModifier = (Core.Name "hydra/ext/java/syntax.FieldModifier")

_FieldModifier_annotation = (Core.FieldName "annotation")

_FieldModifier_public = (Core.FieldName "public")

_FieldModifier_protected = (Core.FieldName "protected")

_FieldModifier_private = (Core.FieldName "private")

_FieldModifier_static = (Core.FieldName "static")

_FieldModifier_final = (Core.FieldName "final")

_FieldModifier_transient = (Core.FieldName "transient")

_FieldModifier_volatile = (Core.FieldName "volatile")

data VariableDeclarator 
  = VariableDeclarator {
    variableDeclaratorId :: VariableDeclaratorId,
    variableDeclaratorInitializer :: (Maybe VariableInitializer)
  }
  deriving (Eq, Ord, Read, Show)

_VariableDeclarator = (Core.Name "hydra/ext/java/syntax.VariableDeclarator")

_VariableDeclarator_id = (Core.FieldName "id")

_VariableDeclarator_initializer = (Core.FieldName "initializer")

data VariableDeclaratorId 
  = VariableDeclaratorId {
    variableDeclaratorIdIdentifier :: Identifier,
    variableDeclaratorIdDims :: (Maybe Dims)
  }
  deriving (Eq, Ord, Read, Show)

_VariableDeclaratorId = (Core.Name "hydra/ext/java/syntax.VariableDeclaratorId")

_VariableDeclaratorId_identifier = (Core.FieldName "identifier")

_VariableDeclaratorId_dims = (Core.FieldName "dims")

data VariableInitializer 
  = VariableInitializerExpression Expression
  | VariableInitializerArrayInitializer ArrayInitializer
  deriving (Eq, Ord, Read, Show)

_VariableInitializer = (Core.Name "hydra/ext/java/syntax.VariableInitializer")

_VariableInitializer_expression = (Core.FieldName "expression")

_VariableInitializer_arrayInitializer = (Core.FieldName "arrayInitializer")

-- A Type which does not allow annotations
newtype UnannType 
  = UnannType {
    unUnannType :: Type
  }
  deriving (Eq, Ord, Read, Show)

_UnannType = (Core.Name "hydra/ext/java/syntax.UnannType")

-- A ClassType which does not allow annotations
newtype UnannClassType 
  = UnannClassType {
    unUnannClassType :: ClassType
  }
  deriving (Eq, Ord, Read, Show)

_UnannClassType = (Core.Name "hydra/ext/java/syntax.UnannClassType")

data MethodDeclaration 
  = MethodDeclaration {
    methodDeclarationModifiers :: [MethodModifier],
    methodDeclarationHeader :: MethodHeader,
    methodDeclarationBody :: MethodBody
  }
  deriving (Eq, Ord, Read, Show)

_MethodDeclaration = (Core.Name "hydra/ext/java/syntax.MethodDeclaration")

_MethodDeclaration_modifiers = (Core.FieldName "modifiers")

_MethodDeclaration_header = (Core.FieldName "header")

_MethodDeclaration_body = (Core.FieldName "body")

data MethodModifier 
  = MethodModifierAnnotation Annotation
  | MethodModifierPublic 
  | MethodModifierProtected 
  | MethodModifierPrivate 
  | MethodModifierAbstract 
  | MethodModifierFinal 
  | MethodModifierSynchronized 
  | MethodModifierNative 
  | MethodModifierStrictfb 
  deriving (Eq, Ord, Read, Show)

_MethodModifier = (Core.Name "hydra/ext/java/syntax.MethodModifier")

_MethodModifier_annotation = (Core.FieldName "annotation")

_MethodModifier_public = (Core.FieldName "public")

_MethodModifier_protected = (Core.FieldName "protected")

_MethodModifier_private = (Core.FieldName "private")

_MethodModifier_abstract = (Core.FieldName "abstract")

_MethodModifier_final = (Core.FieldName "final")

_MethodModifier_synchronized = (Core.FieldName "synchronized")

_MethodModifier_native = (Core.FieldName "native")

_MethodModifier_strictfb = (Core.FieldName "strictfb")

data MethodHeader 
  = MethodHeader {
    methodHeaderParameters :: [TypeParameter],
    methodHeaderAnnotations :: [Annotation],
    methodHeaderResult :: Result,
    methodHeaderDeclarator :: MethodDeclarator,
    methodHeaderThrows :: (Maybe Throws)
  }
  deriving (Eq, Ord, Read, Show)

_MethodHeader = (Core.Name "hydra/ext/java/syntax.MethodHeader")

_MethodHeader_parameters = (Core.FieldName "parameters")

_MethodHeader_annotations = (Core.FieldName "annotations")

_MethodHeader_result = (Core.FieldName "result")

_MethodHeader_declarator = (Core.FieldName "declarator")

_MethodHeader_throws = (Core.FieldName "throws")

data Result 
  = ResultType UnannType
  | ResultVoid 
  deriving (Eq, Ord, Read, Show)

_Result = (Core.Name "hydra/ext/java/syntax.Result")

_Result_type = (Core.FieldName "type")

_Result_void = (Core.FieldName "void")

data MethodDeclarator 
  = MethodDeclarator {
    methodDeclaratorIdentifier :: Identifier,
    methodDeclaratorReceiverParameter :: (Maybe ReceiverParameter),
    methodDeclaratorFormalParameters :: [FormalParameter]
  }
  deriving (Eq, Ord, Read, Show)

_MethodDeclarator = (Core.Name "hydra/ext/java/syntax.MethodDeclarator")

_MethodDeclarator_identifier = (Core.FieldName "identifier")

_MethodDeclarator_receiverParameter = (Core.FieldName "receiverParameter")

_MethodDeclarator_formalParameters = (Core.FieldName "formalParameters")

data ReceiverParameter 
  = ReceiverParameter {
    receiverParameterAnnotations :: [Annotation],
    receiverParameterUnannType :: UnannType,
    receiverParameterIdentifier :: (Maybe Identifier)
  }
  deriving (Eq, Ord, Read, Show)

_ReceiverParameter = (Core.Name "hydra/ext/java/syntax.ReceiverParameter")

_ReceiverParameter_annotations = (Core.FieldName "annotations")

_ReceiverParameter_unannType = (Core.FieldName "unannType")

_ReceiverParameter_identifier = (Core.FieldName "identifier")

data FormalParameter 
  = FormalParameterSimple FormalParameter_Simple
  | FormalParameterVariableArity VariableArityParameter
  deriving (Eq, Ord, Read, Show)

_FormalParameter = (Core.Name "hydra/ext/java/syntax.FormalParameter")

_FormalParameter_simple = (Core.FieldName "simple")

_FormalParameter_variableArity = (Core.FieldName "variableArity")

data FormalParameter_Simple 
  = FormalParameter_Simple {
    formalParameter_SimpleModifiers :: [VariableModifier],
    formalParameter_SimpleType :: UnannType,
    formalParameter_SimpleId :: VariableDeclaratorId
  }
  deriving (Eq, Ord, Read, Show)

_FormalParameter_Simple = (Core.Name "hydra/ext/java/syntax.FormalParameter.Simple")

_FormalParameter_Simple_modifiers = (Core.FieldName "modifiers")

_FormalParameter_Simple_type = (Core.FieldName "type")

_FormalParameter_Simple_id = (Core.FieldName "id")

data VariableArityParameter 
  = VariableArityParameter {
    variableArityParameterModifiers :: VariableModifier,
    variableArityParameterType :: UnannType,
    variableArityParameterAnnotations :: [Annotation],
    variableArityParameterIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_VariableArityParameter = (Core.Name "hydra/ext/java/syntax.VariableArityParameter")

_VariableArityParameter_modifiers = (Core.FieldName "modifiers")

_VariableArityParameter_type = (Core.FieldName "type")

_VariableArityParameter_annotations = (Core.FieldName "annotations")

_VariableArityParameter_identifier = (Core.FieldName "identifier")

data VariableModifier 
  = VariableModifierAnnotation Annotation
  | VariableModifierFinal 
  deriving (Eq, Ord, Read, Show)

_VariableModifier = (Core.Name "hydra/ext/java/syntax.VariableModifier")

_VariableModifier_annotation = (Core.FieldName "annotation")

_VariableModifier_final = (Core.FieldName "final")

-- Note: list cannot be empty
newtype Throws 
  = Throws {
    unThrows :: [ExceptionType]
  }
  deriving (Eq, Ord, Read, Show)

_Throws = (Core.Name "hydra/ext/java/syntax.Throws")

data ExceptionType 
  = ExceptionTypeClass ClassType
  | ExceptionTypeVariable TypeVariable
  deriving (Eq, Ord, Read, Show)

_ExceptionType = (Core.Name "hydra/ext/java/syntax.ExceptionType")

_ExceptionType_class = (Core.FieldName "class")

_ExceptionType_variable = (Core.FieldName "variable")

newtype MethodBody 
  = MethodBody {
    unMethodBody :: Block
  }
  deriving (Eq, Ord, Read, Show)

_MethodBody = (Core.Name "hydra/ext/java/syntax.MethodBody")

newtype InstanceInitializer 
  = InstanceInitializer {
    unInstanceInitializer :: Block
  }
  deriving (Eq, Ord, Read, Show)

_InstanceInitializer = (Core.Name "hydra/ext/java/syntax.InstanceInitializer")

newtype StaticInitializer 
  = StaticInitializer {
    unStaticInitializer :: Block
  }
  deriving (Eq, Ord, Read, Show)

_StaticInitializer = (Core.Name "hydra/ext/java/syntax.StaticInitializer")

data ConstructorDeclaration 
  = ConstructorDeclaration {
    constructorDeclarationModifiers :: [ConstructorModifier],
    constructorDeclarationConstructor :: ConstructorDeclarator,
    constructorDeclarationThrows :: (Maybe Throws),
    constructorDeclarationBody :: ConstructorBody
  }
  deriving (Eq, Ord, Read, Show)

_ConstructorDeclaration = (Core.Name "hydra/ext/java/syntax.ConstructorDeclaration")

_ConstructorDeclaration_modifiers = (Core.FieldName "modifiers")

_ConstructorDeclaration_constructor = (Core.FieldName "constructor")

_ConstructorDeclaration_throws = (Core.FieldName "throws")

_ConstructorDeclaration_body = (Core.FieldName "body")

data ConstructorModifier 
  = ConstructorModifierAnnotation Annotation
  | ConstructorModifierPublic 
  | ConstructorModifierProtected 
  | ConstructorModifierPrivate 
  deriving (Eq, Ord, Read, Show)

_ConstructorModifier = (Core.Name "hydra/ext/java/syntax.ConstructorModifier")

_ConstructorModifier_annotation = (Core.FieldName "annotation")

_ConstructorModifier_public = (Core.FieldName "public")

_ConstructorModifier_protected = (Core.FieldName "protected")

_ConstructorModifier_private = (Core.FieldName "private")

data ConstructorDeclarator 
  = ConstructorDeclarator {
    constructorDeclaratorParameters :: [TypeParameter],
    constructorDeclaratorName :: SimpleTypeName,
    constructorDeclaratorReceiverParameter :: (Maybe ReceiverParameter),
    constructorDeclaratorFormalParameters :: [FormalParameter]
  }
  deriving (Eq, Ord, Read, Show)

_ConstructorDeclarator = (Core.Name "hydra/ext/java/syntax.ConstructorDeclarator")

_ConstructorDeclarator_parameters = (Core.FieldName "parameters")

_ConstructorDeclarator_name = (Core.FieldName "name")

_ConstructorDeclarator_receiverParameter = (Core.FieldName "receiverParameter")

_ConstructorDeclarator_formalParameters = (Core.FieldName "formalParameters")

newtype SimpleTypeName 
  = SimpleTypeName {
    unSimpleTypeName :: TypeIdentifier
  }
  deriving (Eq, Ord, Read, Show)

_SimpleTypeName = (Core.Name "hydra/ext/java/syntax.SimpleTypeName")

data ConstructorBody 
  = ConstructorBody {
    constructorBodyInvocation :: (Maybe ExplicitConstructorInvocation),
    constructorBodyStatements :: [BlockStatement]
  }
  deriving (Eq, Ord, Read, Show)

_ConstructorBody = (Core.Name "hydra/ext/java/syntax.ConstructorBody")

_ConstructorBody_invocation = (Core.FieldName "invocation")

_ConstructorBody_statements = (Core.FieldName "statements")

data ExplicitConstructorInvocation 
  = ExplicitConstructorInvocation {
    explicitConstructorInvocationTypeArguments :: [TypeArgument],
    explicitConstructorInvocationArguments :: [Expression],
    explicitConstructorInvocationVariant :: ExplicitConstructorInvocation_Variant
  }
  deriving (Eq, Ord, Read, Show)

_ExplicitConstructorInvocation = (Core.Name "hydra/ext/java/syntax.ExplicitConstructorInvocation")

_ExplicitConstructorInvocation_typeArguments = (Core.FieldName "typeArguments")

_ExplicitConstructorInvocation_arguments = (Core.FieldName "arguments")

_ExplicitConstructorInvocation_variant = (Core.FieldName "variant")

data ExplicitConstructorInvocation_Variant 
  = ExplicitConstructorInvocation_VariantThis 
  | ExplicitConstructorInvocation_VariantSuper (Maybe ExpressionName)
  | ExplicitConstructorInvocation_VariantPrimary Primary
  deriving (Eq, Ord, Read, Show)

_ExplicitConstructorInvocation_Variant = (Core.Name "hydra/ext/java/syntax.ExplicitConstructorInvocation.Variant")

_ExplicitConstructorInvocation_Variant_this = (Core.FieldName "this")

_ExplicitConstructorInvocation_Variant_super = (Core.FieldName "super")

_ExplicitConstructorInvocation_Variant_primary = (Core.FieldName "primary")

data EnumDeclaration 
  = EnumDeclaration {
    enumDeclarationModifiers :: [ClassModifier],
    enumDeclarationIdentifier :: TypeIdentifier,
    enumDeclarationImplements :: [InterfaceType],
    enumDeclarationBody :: EnumBody
  }
  deriving (Eq, Ord, Read, Show)

_EnumDeclaration = (Core.Name "hydra/ext/java/syntax.EnumDeclaration")

_EnumDeclaration_modifiers = (Core.FieldName "modifiers")

_EnumDeclaration_identifier = (Core.FieldName "identifier")

_EnumDeclaration_implements = (Core.FieldName "implements")

_EnumDeclaration_body = (Core.FieldName "body")

newtype EnumBody 
  = EnumBody {
    unEnumBody :: [EnumBody_Element]
  }
  deriving (Eq, Ord, Read, Show)

_EnumBody = (Core.Name "hydra/ext/java/syntax.EnumBody")

data EnumBody_Element 
  = EnumBody_Element {
    enumBody_ElementConstants :: [EnumConstant],
    enumBody_ElementBodyDeclarations :: [ClassBodyDeclaration]
  }
  deriving (Eq, Ord, Read, Show)

_EnumBody_Element = (Core.Name "hydra/ext/java/syntax.EnumBody.Element")

_EnumBody_Element_constants = (Core.FieldName "constants")

_EnumBody_Element_bodyDeclarations = (Core.FieldName "bodyDeclarations")

data EnumConstant 
  = EnumConstant {
    enumConstantModifiers :: [EnumConstantModifier],
    enumConstantIdentifier :: Identifier,
    enumConstantArguments :: [[Expression]],
    enumConstantBody :: (Maybe ClassBody)
  }
  deriving (Eq, Ord, Read, Show)

_EnumConstant = (Core.Name "hydra/ext/java/syntax.EnumConstant")

_EnumConstant_modifiers = (Core.FieldName "modifiers")

_EnumConstant_identifier = (Core.FieldName "identifier")

_EnumConstant_arguments = (Core.FieldName "arguments")

_EnumConstant_body = (Core.FieldName "body")

newtype EnumConstantModifier 
  = EnumConstantModifier {
    unEnumConstantModifier :: Annotation
  }
  deriving (Eq, Ord, Read, Show)

_EnumConstantModifier = (Core.Name "hydra/ext/java/syntax.EnumConstantModifier")

data InterfaceDeclaration 
  = InterfaceDeclarationNormalInterface NormalInterfaceDeclaration
  | InterfaceDeclarationAnnotationType AnnotationTypeDeclaration
  deriving (Eq, Ord, Read, Show)

_InterfaceDeclaration = (Core.Name "hydra/ext/java/syntax.InterfaceDeclaration")

_InterfaceDeclaration_normalInterface = (Core.FieldName "normalInterface")

_InterfaceDeclaration_annotationType = (Core.FieldName "annotationType")

data NormalInterfaceDeclaration 
  = NormalInterfaceDeclaration {
    normalInterfaceDeclarationModifiers :: [InterfaceModifier],
    normalInterfaceDeclarationIdentifier :: TypeIdentifier,
    normalInterfaceDeclarationParameters :: [TypeParameter],
    normalInterfaceDeclarationExtends :: [InterfaceType],
    normalInterfaceDeclarationBody :: InterfaceBody
  }
  deriving (Eq, Ord, Read, Show)

_NormalInterfaceDeclaration = (Core.Name "hydra/ext/java/syntax.NormalInterfaceDeclaration")

_NormalInterfaceDeclaration_modifiers = (Core.FieldName "modifiers")

_NormalInterfaceDeclaration_identifier = (Core.FieldName "identifier")

_NormalInterfaceDeclaration_parameters = (Core.FieldName "parameters")

_NormalInterfaceDeclaration_extends = (Core.FieldName "extends")

_NormalInterfaceDeclaration_body = (Core.FieldName "body")

data InterfaceModifier 
  = InterfaceModifierAnnotation Annotation
  | InterfaceModifierPublic 
  | InterfaceModifierProtected 
  | InterfaceModifierPrivate 
  | InterfaceModifierAbstract 
  | InterfaceModifierStatic 
  | InterfaceModifierStrictfb 
  deriving (Eq, Ord, Read, Show)

_InterfaceModifier = (Core.Name "hydra/ext/java/syntax.InterfaceModifier")

_InterfaceModifier_annotation = (Core.FieldName "annotation")

_InterfaceModifier_public = (Core.FieldName "public")

_InterfaceModifier_protected = (Core.FieldName "protected")

_InterfaceModifier_private = (Core.FieldName "private")

_InterfaceModifier_abstract = (Core.FieldName "abstract")

_InterfaceModifier_static = (Core.FieldName "static")

_InterfaceModifier_strictfb = (Core.FieldName "strictfb")

newtype InterfaceBody 
  = InterfaceBody {
    unInterfaceBody :: [[InterfaceMemberDeclaration]]
  }
  deriving (Eq, Ord, Read, Show)

_InterfaceBody = (Core.Name "hydra/ext/java/syntax.InterfaceBody")

data InterfaceMemberDeclaration 
  = InterfaceMemberDeclarationConstant ConstantDeclaration
  | InterfaceMemberDeclarationInterfaceMethod InterfaceMethodDeclaration
  | InterfaceMemberDeclarationClass ClassDeclaration
  | InterfaceMemberDeclarationInterface InterfaceDeclaration
  deriving (Eq, Ord, Read, Show)

_InterfaceMemberDeclaration = (Core.Name "hydra/ext/java/syntax.InterfaceMemberDeclaration")

_InterfaceMemberDeclaration_constant = (Core.FieldName "constant")

_InterfaceMemberDeclaration_interfaceMethod = (Core.FieldName "interfaceMethod")

_InterfaceMemberDeclaration_class = (Core.FieldName "class")

_InterfaceMemberDeclaration_interface = (Core.FieldName "interface")

data ConstantDeclaration 
  = ConstantDeclaration {
    constantDeclarationModifiers :: [ConstantModifier],
    constantDeclarationType :: UnannType,
    constantDeclarationVariables :: [VariableDeclarator]
  }
  deriving (Eq, Ord, Read, Show)

_ConstantDeclaration = (Core.Name "hydra/ext/java/syntax.ConstantDeclaration")

_ConstantDeclaration_modifiers = (Core.FieldName "modifiers")

_ConstantDeclaration_type = (Core.FieldName "type")

_ConstantDeclaration_variables = (Core.FieldName "variables")

data ConstantModifier 
  = ConstantModifierAnnotation Annotation
  | ConstantModifierPublic 
  | ConstantModifierStatic 
  | ConstantModifierFinal 
  deriving (Eq, Ord, Read, Show)

_ConstantModifier = (Core.Name "hydra/ext/java/syntax.ConstantModifier")

_ConstantModifier_annotation = (Core.FieldName "annotation")

_ConstantModifier_public = (Core.FieldName "public")

_ConstantModifier_static = (Core.FieldName "static")

_ConstantModifier_final = (Core.FieldName "final")

data InterfaceMethodDeclaration 
  = InterfaceMethodDeclaration {
    interfaceMethodDeclarationModifiers :: [InterfaceMethodModifier],
    interfaceMethodDeclarationHeader :: MethodHeader,
    interfaceMethodDeclarationBody :: MethodBody
  }
  deriving (Eq, Ord, Read, Show)

_InterfaceMethodDeclaration = (Core.Name "hydra/ext/java/syntax.InterfaceMethodDeclaration")

_InterfaceMethodDeclaration_modifiers = (Core.FieldName "modifiers")

_InterfaceMethodDeclaration_header = (Core.FieldName "header")

_InterfaceMethodDeclaration_body = (Core.FieldName "body")

data InterfaceMethodModifier 
  = InterfaceMethodModifierAnnotation Annotation
  | InterfaceMethodModifierPublic 
  | InterfaceMethodModifierPrivate 
  | InterfaceMethodModifierAbstract 
  | InterfaceMethodModifierDefault 
  | InterfaceMethodModifierStatic 
  | InterfaceMethodModifierStrictfp 
  deriving (Eq, Ord, Read, Show)

_InterfaceMethodModifier = (Core.Name "hydra/ext/java/syntax.InterfaceMethodModifier")

_InterfaceMethodModifier_annotation = (Core.FieldName "annotation")

_InterfaceMethodModifier_public = (Core.FieldName "public")

_InterfaceMethodModifier_private = (Core.FieldName "private")

_InterfaceMethodModifier_abstract = (Core.FieldName "abstract")

_InterfaceMethodModifier_default = (Core.FieldName "default")

_InterfaceMethodModifier_static = (Core.FieldName "static")

_InterfaceMethodModifier_strictfp = (Core.FieldName "strictfp")

data AnnotationTypeDeclaration 
  = AnnotationTypeDeclaration {
    annotationTypeDeclarationModifiers :: [InterfaceModifier],
    annotationTypeDeclarationIdentifier :: TypeIdentifier,
    annotationTypeDeclarationBody :: AnnotationTypeBody
  }
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeDeclaration = (Core.Name "hydra/ext/java/syntax.AnnotationTypeDeclaration")

_AnnotationTypeDeclaration_modifiers = (Core.FieldName "modifiers")

_AnnotationTypeDeclaration_identifier = (Core.FieldName "identifier")

_AnnotationTypeDeclaration_body = (Core.FieldName "body")

newtype AnnotationTypeBody 
  = AnnotationTypeBody {
    unAnnotationTypeBody :: [[AnnotationTypeMemberDeclaration]]
  }
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeBody = (Core.Name "hydra/ext/java/syntax.AnnotationTypeBody")

data AnnotationTypeMemberDeclaration 
  = AnnotationTypeMemberDeclarationAnnotationType AnnotationTypeElementDeclaration
  | AnnotationTypeMemberDeclarationConstant ConstantDeclaration
  | AnnotationTypeMemberDeclarationClass ClassDeclaration
  | AnnotationTypeMemberDeclarationInterface InterfaceDeclaration
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeMemberDeclaration = (Core.Name "hydra/ext/java/syntax.AnnotationTypeMemberDeclaration")

_AnnotationTypeMemberDeclaration_annotationType = (Core.FieldName "annotationType")

_AnnotationTypeMemberDeclaration_constant = (Core.FieldName "constant")

_AnnotationTypeMemberDeclaration_class = (Core.FieldName "class")

_AnnotationTypeMemberDeclaration_interface = (Core.FieldName "interface")

data AnnotationTypeElementDeclaration 
  = AnnotationTypeElementDeclaration {
    annotationTypeElementDeclarationModifiers :: [AnnotationTypeElementModifier],
    annotationTypeElementDeclarationType :: UnannType,
    annotationTypeElementDeclarationIdentifier :: Identifier,
    annotationTypeElementDeclarationDims :: (Maybe Dims),
    annotationTypeElementDeclarationDefault :: (Maybe DefaultValue)
  }
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeElementDeclaration = (Core.Name "hydra/ext/java/syntax.AnnotationTypeElementDeclaration")

_AnnotationTypeElementDeclaration_modifiers = (Core.FieldName "modifiers")

_AnnotationTypeElementDeclaration_type = (Core.FieldName "type")

_AnnotationTypeElementDeclaration_identifier = (Core.FieldName "identifier")

_AnnotationTypeElementDeclaration_dims = (Core.FieldName "dims")

_AnnotationTypeElementDeclaration_default = (Core.FieldName "default")

data AnnotationTypeElementModifier 
  = AnnotationTypeElementModifierPublic Annotation
  | AnnotationTypeElementModifierAbstract 
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeElementModifier = (Core.Name "hydra/ext/java/syntax.AnnotationTypeElementModifier")

_AnnotationTypeElementModifier_public = (Core.FieldName "public")

_AnnotationTypeElementModifier_abstract = (Core.FieldName "abstract")

newtype DefaultValue 
  = DefaultValue {
    unDefaultValue :: ElementValue
  }
  deriving (Eq, Ord, Read, Show)

_DefaultValue = (Core.Name "hydra/ext/java/syntax.DefaultValue")

data Annotation 
  = AnnotationNormal NormalAnnotation
  | AnnotationMarker MarkerAnnotation
  | AnnotationSingleElement SingleElementAnnotation
  deriving (Eq, Ord, Read, Show)

_Annotation = (Core.Name "hydra/ext/java/syntax.Annotation")

_Annotation_normal = (Core.FieldName "normal")

_Annotation_marker = (Core.FieldName "marker")

_Annotation_singleElement = (Core.FieldName "singleElement")

data NormalAnnotation 
  = NormalAnnotation {
    normalAnnotationTypeName :: TypeName,
    normalAnnotationPairs :: [ElementValuePair]
  }
  deriving (Eq, Ord, Read, Show)

_NormalAnnotation = (Core.Name "hydra/ext/java/syntax.NormalAnnotation")

_NormalAnnotation_typeName = (Core.FieldName "typeName")

_NormalAnnotation_pairs = (Core.FieldName "pairs")

data ElementValuePair 
  = ElementValuePair {
    elementValuePairKey :: Identifier,
    elementValuePairValue :: ElementValue
  }
  deriving (Eq, Ord, Read, Show)

_ElementValuePair = (Core.Name "hydra/ext/java/syntax.ElementValuePair")

_ElementValuePair_key = (Core.FieldName "key")

_ElementValuePair_value = (Core.FieldName "value")

data ElementValue 
  = ElementValueConditionalExpression ConditionalExpression
  | ElementValueElementValueArrayInitializer ElementValueArrayInitializer
  | ElementValueAnnotation Annotation
  deriving (Eq, Ord, Read, Show)

_ElementValue = (Core.Name "hydra/ext/java/syntax.ElementValue")

_ElementValue_conditionalExpression = (Core.FieldName "conditionalExpression")

_ElementValue_elementValueArrayInitializer = (Core.FieldName "elementValueArrayInitializer")

_ElementValue_annotation = (Core.FieldName "annotation")

newtype ElementValueArrayInitializer 
  = ElementValueArrayInitializer {
    unElementValueArrayInitializer :: [ElementValue]
  }
  deriving (Eq, Ord, Read, Show)

_ElementValueArrayInitializer = (Core.Name "hydra/ext/java/syntax.ElementValueArrayInitializer")

newtype MarkerAnnotation 
  = MarkerAnnotation {
    unMarkerAnnotation :: TypeName
  }
  deriving (Eq, Ord, Read, Show)

_MarkerAnnotation = (Core.Name "hydra/ext/java/syntax.MarkerAnnotation")

data SingleElementAnnotation 
  = SingleElementAnnotation {
    singleElementAnnotationName :: TypeName,
    singleElementAnnotationValue :: (Maybe ElementValue)
  }
  deriving (Eq, Ord, Read, Show)

_SingleElementAnnotation = (Core.Name "hydra/ext/java/syntax.SingleElementAnnotation")

_SingleElementAnnotation_name = (Core.FieldName "name")

_SingleElementAnnotation_value = (Core.FieldName "value")

newtype ArrayInitializer 
  = ArrayInitializer {
    unArrayInitializer :: [[VariableInitializer]]
  }
  deriving (Eq, Ord, Read, Show)

_ArrayInitializer = (Core.Name "hydra/ext/java/syntax.ArrayInitializer")

newtype Block 
  = Block {
    unBlock :: [BlockStatement]
  }
  deriving (Eq, Ord, Read, Show)

_Block = (Core.Name "hydra/ext/java/syntax.Block")

data BlockStatement 
  = BlockStatementLocalVariableDeclaration LocalVariableDeclarationStatement
  | BlockStatementClass ClassDeclaration
  | BlockStatementStatement Statement
  deriving (Eq, Ord, Read, Show)

_BlockStatement = (Core.Name "hydra/ext/java/syntax.BlockStatement")

_BlockStatement_localVariableDeclaration = (Core.FieldName "localVariableDeclaration")

_BlockStatement_class = (Core.FieldName "class")

_BlockStatement_statement = (Core.FieldName "statement")

newtype LocalVariableDeclarationStatement 
  = LocalVariableDeclarationStatement {
    unLocalVariableDeclarationStatement :: LocalVariableDeclaration
  }
  deriving (Eq, Ord, Read, Show)

_LocalVariableDeclarationStatement = (Core.Name "hydra/ext/java/syntax.LocalVariableDeclarationStatement")

data LocalVariableDeclaration 
  = LocalVariableDeclaration {
    localVariableDeclarationModifiers :: [VariableModifier],
    localVariableDeclarationType :: LocalVariableType,
    localVariableDeclarationDeclarators :: [VariableDeclarator]
  }
  deriving (Eq, Ord, Read, Show)

_LocalVariableDeclaration = (Core.Name "hydra/ext/java/syntax.LocalVariableDeclaration")

_LocalVariableDeclaration_modifiers = (Core.FieldName "modifiers")

_LocalVariableDeclaration_type = (Core.FieldName "type")

_LocalVariableDeclaration_declarators = (Core.FieldName "declarators")

data LocalVariableType 
  = LocalVariableTypeType UnannType
  | LocalVariableTypeVar 
  deriving (Eq, Ord, Read, Show)

_LocalVariableType = (Core.Name "hydra/ext/java/syntax.LocalVariableType")

_LocalVariableType_type = (Core.FieldName "type")

_LocalVariableType_var = (Core.FieldName "var")

data Statement 
  = StatementWithoutTrailing StatementWithoutTrailingSubstatement
  | StatementLabeled LabeledStatement
  | StatementIfThen IfThenStatement
  | StatementIfThenElse IfThenElseStatement
  | StatementWhile WhileStatement
  | StatementFor ForStatement
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra/ext/java/syntax.Statement")

_Statement_withoutTrailing = (Core.FieldName "withoutTrailing")

_Statement_labeled = (Core.FieldName "labeled")

_Statement_ifThen = (Core.FieldName "ifThen")

_Statement_ifThenElse = (Core.FieldName "ifThenElse")

_Statement_while = (Core.FieldName "while")

_Statement_for = (Core.FieldName "for")

data StatementNoShortIf 
  = StatementNoShortIfWithoutTrailing StatementWithoutTrailingSubstatement
  | StatementNoShortIfLabeled LabeledStatementNoShortIf
  | StatementNoShortIfIfThenElse IfThenElseStatementNoShortIf
  | StatementNoShortIfWhile WhileStatementNoShortIf
  | StatementNoShortIfFor ForStatementNoShortIf
  deriving (Eq, Ord, Read, Show)

_StatementNoShortIf = (Core.Name "hydra/ext/java/syntax.StatementNoShortIf")

_StatementNoShortIf_withoutTrailing = (Core.FieldName "withoutTrailing")

_StatementNoShortIf_labeled = (Core.FieldName "labeled")

_StatementNoShortIf_ifThenElse = (Core.FieldName "ifThenElse")

_StatementNoShortIf_while = (Core.FieldName "while")

_StatementNoShortIf_for = (Core.FieldName "for")

data StatementWithoutTrailingSubstatement 
  = StatementWithoutTrailingSubstatementBlock Block
  | StatementWithoutTrailingSubstatementEmpty EmptyStatement
  | StatementWithoutTrailingSubstatementExpression ExpressionStatement
  | StatementWithoutTrailingSubstatementAssert AssertStatement
  | StatementWithoutTrailingSubstatementSwitch SwitchStatement
  | StatementWithoutTrailingSubstatementDo DoStatement
  | StatementWithoutTrailingSubstatementBreak BreakStatement
  | StatementWithoutTrailingSubstatementContinue ContinueStatement
  | StatementWithoutTrailingSubstatementReturn ReturnStatement
  | StatementWithoutTrailingSubstatementSynchronized SynchronizedStatement
  | StatementWithoutTrailingSubstatementThrow ThrowStatement
  | StatementWithoutTrailingSubstatementTry TryStatement
  deriving (Eq, Ord, Read, Show)

_StatementWithoutTrailingSubstatement = (Core.Name "hydra/ext/java/syntax.StatementWithoutTrailingSubstatement")

_StatementWithoutTrailingSubstatement_block = (Core.FieldName "block")

_StatementWithoutTrailingSubstatement_empty = (Core.FieldName "empty")

_StatementWithoutTrailingSubstatement_expression = (Core.FieldName "expression")

_StatementWithoutTrailingSubstatement_assert = (Core.FieldName "assert")

_StatementWithoutTrailingSubstatement_switch = (Core.FieldName "switch")

_StatementWithoutTrailingSubstatement_do = (Core.FieldName "do")

_StatementWithoutTrailingSubstatement_break = (Core.FieldName "break")

_StatementWithoutTrailingSubstatement_continue = (Core.FieldName "continue")

_StatementWithoutTrailingSubstatement_return = (Core.FieldName "return")

_StatementWithoutTrailingSubstatement_synchronized = (Core.FieldName "synchronized")

_StatementWithoutTrailingSubstatement_throw = (Core.FieldName "throw")

_StatementWithoutTrailingSubstatement_try = (Core.FieldName "try")

data EmptyStatement 
  = EmptyStatement {}
  deriving (Eq, Ord, Read, Show)

_EmptyStatement = (Core.Name "hydra/ext/java/syntax.EmptyStatement")

data LabeledStatement 
  = LabeledStatement {
    labeledStatementIdentifier :: Identifier,
    labeledStatementStatement :: Statement
  }
  deriving (Eq, Ord, Read, Show)

_LabeledStatement = (Core.Name "hydra/ext/java/syntax.LabeledStatement")

_LabeledStatement_identifier = (Core.FieldName "identifier")

_LabeledStatement_statement = (Core.FieldName "statement")

data LabeledStatementNoShortIf 
  = LabeledStatementNoShortIf {
    labeledStatementNoShortIfIdentifier :: Identifier,
    labeledStatementNoShortIfStatement :: StatementNoShortIf
  }
  deriving (Eq, Ord, Read, Show)

_LabeledStatementNoShortIf = (Core.Name "hydra/ext/java/syntax.LabeledStatementNoShortIf")

_LabeledStatementNoShortIf_identifier = (Core.FieldName "identifier")

_LabeledStatementNoShortIf_statement = (Core.FieldName "statement")

newtype ExpressionStatement 
  = ExpressionStatement {
    unExpressionStatement :: StatementExpression
  }
  deriving (Eq, Ord, Read, Show)

_ExpressionStatement = (Core.Name "hydra/ext/java/syntax.ExpressionStatement")

data StatementExpression 
  = StatementExpressionAssignment Assignment
  | StatementExpressionPreIncrement PreIncrementExpression
  | StatementExpressionPreDecrement PreDecrementExpression
  | StatementExpressionPostIncrement PostIncrementExpression
  | StatementExpressionPostDecrement PostDecrementExpression
  | StatementExpressionMethodInvocation MethodInvocation
  | StatementExpressionClassInstanceCreation ClassInstanceCreationExpression
  deriving (Eq, Ord, Read, Show)

_StatementExpression = (Core.Name "hydra/ext/java/syntax.StatementExpression")

_StatementExpression_assignment = (Core.FieldName "assignment")

_StatementExpression_preIncrement = (Core.FieldName "preIncrement")

_StatementExpression_preDecrement = (Core.FieldName "preDecrement")

_StatementExpression_postIncrement = (Core.FieldName "postIncrement")

_StatementExpression_postDecrement = (Core.FieldName "postDecrement")

_StatementExpression_methodInvocation = (Core.FieldName "methodInvocation")

_StatementExpression_classInstanceCreation = (Core.FieldName "classInstanceCreation")

data IfThenStatement 
  = IfThenStatement {
    ifThenStatementExpression :: (Maybe Expression),
    ifThenStatementStatement :: Statement
  }
  deriving (Eq, Ord, Read, Show)

_IfThenStatement = (Core.Name "hydra/ext/java/syntax.IfThenStatement")

_IfThenStatement_expression = (Core.FieldName "expression")

_IfThenStatement_statement = (Core.FieldName "statement")

data IfThenElseStatement 
  = IfThenElseStatement {
    ifThenElseStatementCond :: (Maybe Expression),
    ifThenElseStatementThen :: StatementNoShortIf,
    ifThenElseStatementElse :: Statement
  }
  deriving (Eq, Ord, Read, Show)

_IfThenElseStatement = (Core.Name "hydra/ext/java/syntax.IfThenElseStatement")

_IfThenElseStatement_cond = (Core.FieldName "cond")

_IfThenElseStatement_then = (Core.FieldName "then")

_IfThenElseStatement_else = (Core.FieldName "else")

data IfThenElseStatementNoShortIf 
  = IfThenElseStatementNoShortIf {
    ifThenElseStatementNoShortIfCond :: (Maybe Expression),
    ifThenElseStatementNoShortIfThen :: StatementNoShortIf,
    ifThenElseStatementNoShortIfElse :: StatementNoShortIf
  }
  deriving (Eq, Ord, Read, Show)

_IfThenElseStatementNoShortIf = (Core.Name "hydra/ext/java/syntax.IfThenElseStatementNoShortIf")

_IfThenElseStatementNoShortIf_cond = (Core.FieldName "cond")

_IfThenElseStatementNoShortIf_then = (Core.FieldName "then")

_IfThenElseStatementNoShortIf_else = (Core.FieldName "else")

data AssertStatement 
  = AssertStatementSingle Expression
  | AssertStatementPair AssertStatement_Pair
  deriving (Eq, Ord, Read, Show)

_AssertStatement = (Core.Name "hydra/ext/java/syntax.AssertStatement")

_AssertStatement_single = (Core.FieldName "single")

_AssertStatement_pair = (Core.FieldName "pair")

data AssertStatement_Pair 
  = AssertStatement_Pair {
    assertStatement_PairFirst :: Expression,
    assertStatement_PairSecond :: Expression
  }
  deriving (Eq, Ord, Read, Show)

_AssertStatement_Pair = (Core.Name "hydra/ext/java/syntax.AssertStatement.Pair")

_AssertStatement_Pair_first = (Core.FieldName "first")

_AssertStatement_Pair_second = (Core.FieldName "second")

data SwitchStatement 
  = SwitchStatement {
    switchStatementCond :: Expression,
    switchStatementBlock :: SwitchBlock
  }
  deriving (Eq, Ord, Read, Show)

_SwitchStatement = (Core.Name "hydra/ext/java/syntax.SwitchStatement")

_SwitchStatement_cond = (Core.FieldName "cond")

_SwitchStatement_block = (Core.FieldName "block")

newtype SwitchBlock 
  = SwitchBlock {
    unSwitchBlock :: [SwitchBlock_Pair]
  }
  deriving (Eq, Ord, Read, Show)

_SwitchBlock = (Core.Name "hydra/ext/java/syntax.SwitchBlock")

data SwitchBlock_Pair 
  = SwitchBlock_Pair {
    switchBlock_PairStatements :: [SwitchBlockStatementGroup],
    switchBlock_PairLabels :: [SwitchLabel]
  }
  deriving (Eq, Ord, Read, Show)

_SwitchBlock_Pair = (Core.Name "hydra/ext/java/syntax.SwitchBlock.Pair")

_SwitchBlock_Pair_statements = (Core.FieldName "statements")

_SwitchBlock_Pair_labels = (Core.FieldName "labels")

data SwitchBlockStatementGroup 
  = SwitchBlockStatementGroup {
    switchBlockStatementGroupLabels :: [SwitchLabel],
    switchBlockStatementGroupStatements :: [BlockStatement]
  }
  deriving (Eq, Ord, Read, Show)

_SwitchBlockStatementGroup = (Core.Name "hydra/ext/java/syntax.SwitchBlockStatementGroup")

_SwitchBlockStatementGroup_labels = (Core.FieldName "labels")

_SwitchBlockStatementGroup_statements = (Core.FieldName "statements")

data SwitchLabel 
  = SwitchLabelConstant ConstantExpression
  | SwitchLabelEnumConstant EnumConstantName
  | SwitchLabelDefault 
  deriving (Eq, Ord, Read, Show)

_SwitchLabel = (Core.Name "hydra/ext/java/syntax.SwitchLabel")

_SwitchLabel_constant = (Core.FieldName "constant")

_SwitchLabel_enumConstant = (Core.FieldName "enumConstant")

_SwitchLabel_default = (Core.FieldName "default")

newtype EnumConstantName 
  = EnumConstantName {
    unEnumConstantName :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_EnumConstantName = (Core.Name "hydra/ext/java/syntax.EnumConstantName")

data WhileStatement 
  = WhileStatement {
    whileStatementCond :: (Maybe Expression),
    whileStatementBody :: Statement
  }
  deriving (Eq, Ord, Read, Show)

_WhileStatement = (Core.Name "hydra/ext/java/syntax.WhileStatement")

_WhileStatement_cond = (Core.FieldName "cond")

_WhileStatement_body = (Core.FieldName "body")

data WhileStatementNoShortIf 
  = WhileStatementNoShortIf {
    whileStatementNoShortIfCond :: (Maybe Expression),
    whileStatementNoShortIfBody :: StatementNoShortIf
  }
  deriving (Eq, Ord, Read, Show)

_WhileStatementNoShortIf = (Core.Name "hydra/ext/java/syntax.WhileStatementNoShortIf")

_WhileStatementNoShortIf_cond = (Core.FieldName "cond")

_WhileStatementNoShortIf_body = (Core.FieldName "body")

data DoStatement 
  = DoStatement {
    doStatementBody :: Statement,
    doStatementConde :: (Maybe Expression)
  }
  deriving (Eq, Ord, Read, Show)

_DoStatement = (Core.Name "hydra/ext/java/syntax.DoStatement")

_DoStatement_body = (Core.FieldName "body")

_DoStatement_conde = (Core.FieldName "conde")

data ForStatement 
  = ForStatementBasic BasicForStatement
  | ForStatementEnhanced EnhancedForStatement
  deriving (Eq, Ord, Read, Show)

_ForStatement = (Core.Name "hydra/ext/java/syntax.ForStatement")

_ForStatement_basic = (Core.FieldName "basic")

_ForStatement_enhanced = (Core.FieldName "enhanced")

data ForStatementNoShortIf 
  = ForStatementNoShortIfBasic BasicForStatementNoShortIf
  | ForStatementNoShortIfEnhanced EnhancedForStatementNoShortIf
  deriving (Eq, Ord, Read, Show)

_ForStatementNoShortIf = (Core.Name "hydra/ext/java/syntax.ForStatementNoShortIf")

_ForStatementNoShortIf_basic = (Core.FieldName "basic")

_ForStatementNoShortIf_enhanced = (Core.FieldName "enhanced")

data BasicForStatement 
  = BasicForStatement {
    basicForStatementCond :: ForCond,
    basicForStatementBody :: Statement
  }
  deriving (Eq, Ord, Read, Show)

_BasicForStatement = (Core.Name "hydra/ext/java/syntax.BasicForStatement")

_BasicForStatement_cond = (Core.FieldName "cond")

_BasicForStatement_body = (Core.FieldName "body")

data ForCond 
  = ForCond {
    forCondInit :: (Maybe ForInit),
    forCondCond :: (Maybe Expression),
    forCondUpdate :: (Maybe ForUpdate)
  }
  deriving (Eq, Ord, Read, Show)

_ForCond = (Core.Name "hydra/ext/java/syntax.ForCond")

_ForCond_init = (Core.FieldName "init")

_ForCond_cond = (Core.FieldName "cond")

_ForCond_update = (Core.FieldName "update")

data BasicForStatementNoShortIf 
  = BasicForStatementNoShortIf {
    basicForStatementNoShortIfCond :: ForCond,
    basicForStatementNoShortIfBody :: StatementNoShortIf
  }
  deriving (Eq, Ord, Read, Show)

_BasicForStatementNoShortIf = (Core.Name "hydra/ext/java/syntax.BasicForStatementNoShortIf")

_BasicForStatementNoShortIf_cond = (Core.FieldName "cond")

_BasicForStatementNoShortIf_body = (Core.FieldName "body")

data ForInit 
  = ForInitStatements [StatementExpression]
  | ForInitLocalVariable LocalVariableDeclaration
  deriving (Eq, Ord, Read, Show)

_ForInit = (Core.Name "hydra/ext/java/syntax.ForInit")

_ForInit_statements = (Core.FieldName "statements")

_ForInit_localVariable = (Core.FieldName "localVariable")

-- Note: list cannot be empty
newtype ForUpdate 
  = ForUpdate {
    unForUpdate :: [StatementExpression]
  }
  deriving (Eq, Ord, Read, Show)

_ForUpdate = (Core.Name "hydra/ext/java/syntax.ForUpdate")

data EnhancedForStatement 
  = EnhancedForStatement {
    enhancedForStatementCond :: EnhancedForCond,
    enhancedForStatementBody :: Statement
  }
  deriving (Eq, Ord, Read, Show)

_EnhancedForStatement = (Core.Name "hydra/ext/java/syntax.EnhancedForStatement")

_EnhancedForStatement_cond = (Core.FieldName "cond")

_EnhancedForStatement_body = (Core.FieldName "body")

data EnhancedForCond 
  = EnhancedForCond {
    enhancedForCondModifiers :: [VariableModifier],
    enhancedForCondType :: LocalVariableType,
    enhancedForCondId :: VariableDeclaratorId,
    enhancedForCondExpression :: Expression
  }
  deriving (Eq, Ord, Read, Show)

_EnhancedForCond = (Core.Name "hydra/ext/java/syntax.EnhancedForCond")

_EnhancedForCond_modifiers = (Core.FieldName "modifiers")

_EnhancedForCond_type = (Core.FieldName "type")

_EnhancedForCond_id = (Core.FieldName "id")

_EnhancedForCond_expression = (Core.FieldName "expression")

data EnhancedForStatementNoShortIf 
  = EnhancedForStatementNoShortIf {
    enhancedForStatementNoShortIfCond :: EnhancedForCond,
    enhancedForStatementNoShortIfBody :: StatementNoShortIf
  }
  deriving (Eq, Ord, Read, Show)

_EnhancedForStatementNoShortIf = (Core.Name "hydra/ext/java/syntax.EnhancedForStatementNoShortIf")

_EnhancedForStatementNoShortIf_cond = (Core.FieldName "cond")

_EnhancedForStatementNoShortIf_body = (Core.FieldName "body")

newtype BreakStatement 
  = BreakStatement {
    unBreakStatement :: (Maybe Identifier)
  }
  deriving (Eq, Ord, Read, Show)

_BreakStatement = (Core.Name "hydra/ext/java/syntax.BreakStatement")

newtype ContinueStatement 
  = ContinueStatement {
    unContinueStatement :: (Maybe Identifier)
  }
  deriving (Eq, Ord, Read, Show)

_ContinueStatement = (Core.Name "hydra/ext/java/syntax.ContinueStatement")

newtype ReturnStatement 
  = ReturnStatement {
    unReturnStatement :: (Maybe Expression)
  }
  deriving (Eq, Ord, Read, Show)

_ReturnStatement = (Core.Name "hydra/ext/java/syntax.ReturnStatement")

newtype ThrowStatement 
  = ThrowStatement {
    unThrowStatement :: Expression
  }
  deriving (Eq, Ord, Read, Show)

_ThrowStatement = (Core.Name "hydra/ext/java/syntax.ThrowStatement")

data SynchronizedStatement 
  = SynchronizedStatement {
    synchronizedStatementExpression :: Expression,
    synchronizedStatementBlock :: Block
  }
  deriving (Eq, Ord, Read, Show)

_SynchronizedStatement = (Core.Name "hydra/ext/java/syntax.SynchronizedStatement")

_SynchronizedStatement_expression = (Core.FieldName "expression")

_SynchronizedStatement_block = (Core.FieldName "block")

data TryStatement 
  = TryStatementSimple TryStatement_Simple
  | TryStatementWithFinally TryStatement_WithFinally
  | TryStatementWithResources TryWithResourcesStatement
  deriving (Eq, Ord, Read, Show)

_TryStatement = (Core.Name "hydra/ext/java/syntax.TryStatement")

_TryStatement_simple = (Core.FieldName "simple")

_TryStatement_withFinally = (Core.FieldName "withFinally")

_TryStatement_withResources = (Core.FieldName "withResources")

data TryStatement_Simple 
  = TryStatement_Simple {
    tryStatement_SimpleBlock :: Block,
    tryStatement_SimpleCatches :: Catches
  }
  deriving (Eq, Ord, Read, Show)

_TryStatement_Simple = (Core.Name "hydra/ext/java/syntax.TryStatement.Simple")

_TryStatement_Simple_block = (Core.FieldName "block")

_TryStatement_Simple_catches = (Core.FieldName "catches")

data TryStatement_WithFinally 
  = TryStatement_WithFinally {
    tryStatement_WithFinallyBlock :: Block,
    tryStatement_WithFinallyCatches :: (Maybe Catches),
    tryStatement_WithFinallyFinally :: Finally
  }
  deriving (Eq, Ord, Read, Show)

_TryStatement_WithFinally = (Core.Name "hydra/ext/java/syntax.TryStatement.WithFinally")

_TryStatement_WithFinally_block = (Core.FieldName "block")

_TryStatement_WithFinally_catches = (Core.FieldName "catches")

_TryStatement_WithFinally_finally = (Core.FieldName "finally")

newtype Catches 
  = Catches {
    unCatches :: [CatchClause]
  }
  deriving (Eq, Ord, Read, Show)

_Catches = (Core.Name "hydra/ext/java/syntax.Catches")

data CatchClause 
  = CatchClause {
    catchClauseParameter :: (Maybe CatchFormalParameter),
    catchClauseBlock :: Block
  }
  deriving (Eq, Ord, Read, Show)

_CatchClause = (Core.Name "hydra/ext/java/syntax.CatchClause")

_CatchClause_parameter = (Core.FieldName "parameter")

_CatchClause_block = (Core.FieldName "block")

data CatchFormalParameter 
  = CatchFormalParameter {
    catchFormalParameterModifiers :: [VariableModifier],
    catchFormalParameterType :: CatchType,
    catchFormalParameterId :: VariableDeclaratorId
  }
  deriving (Eq, Ord, Read, Show)

_CatchFormalParameter = (Core.Name "hydra/ext/java/syntax.CatchFormalParameter")

_CatchFormalParameter_modifiers = (Core.FieldName "modifiers")

_CatchFormalParameter_type = (Core.FieldName "type")

_CatchFormalParameter_id = (Core.FieldName "id")

data CatchType 
  = CatchType {
    catchTypeType :: UnannClassType,
    catchTypeTypes :: [ClassType]
  }
  deriving (Eq, Ord, Read, Show)

_CatchType = (Core.Name "hydra/ext/java/syntax.CatchType")

_CatchType_type = (Core.FieldName "type")

_CatchType_types = (Core.FieldName "types")

newtype Finally 
  = Finally {
    unFinally :: Block
  }
  deriving (Eq, Ord, Read, Show)

_Finally = (Core.Name "hydra/ext/java/syntax.Finally")

data TryWithResourcesStatement 
  = TryWithResourcesStatement {
    tryWithResourcesStatementResourceSpecification :: ResourceSpecification,
    tryWithResourcesStatementBlock :: Block,
    tryWithResourcesStatementCatches :: (Maybe Catches),
    tryWithResourcesStatementFinally :: (Maybe Finally)
  }
  deriving (Eq, Ord, Read, Show)

_TryWithResourcesStatement = (Core.Name "hydra/ext/java/syntax.TryWithResourcesStatement")

_TryWithResourcesStatement_resourceSpecification = (Core.FieldName "resourceSpecification")

_TryWithResourcesStatement_block = (Core.FieldName "block")

_TryWithResourcesStatement_catches = (Core.FieldName "catches")

_TryWithResourcesStatement_finally = (Core.FieldName "finally")

newtype ResourceSpecification 
  = ResourceSpecification {
    unResourceSpecification :: [Resource]
  }
  deriving (Eq, Ord, Read, Show)

_ResourceSpecification = (Core.Name "hydra/ext/java/syntax.ResourceSpecification")

data Resource 
  = ResourceLocal Resource_Local
  | ResourceVariable VariableAccess
  deriving (Eq, Ord, Read, Show)

_Resource = (Core.Name "hydra/ext/java/syntax.Resource")

_Resource_local = (Core.FieldName "local")

_Resource_variable = (Core.FieldName "variable")

data Resource_Local 
  = Resource_Local {
    resource_LocalModifiers :: [VariableModifier],
    resource_LocalType :: LocalVariableType,
    resource_LocalIdentifier :: Identifier,
    resource_LocalExpression :: Expression
  }
  deriving (Eq, Ord, Read, Show)

_Resource_Local = (Core.Name "hydra/ext/java/syntax.Resource.Local")

_Resource_Local_modifiers = (Core.FieldName "modifiers")

_Resource_Local_type = (Core.FieldName "type")

_Resource_Local_identifier = (Core.FieldName "identifier")

_Resource_Local_expression = (Core.FieldName "expression")

data VariableAccess 
  = VariableAccessExpressionName ExpressionName
  | VariableAccessFieldAccess FieldAccess
  deriving (Eq, Ord, Read, Show)

_VariableAccess = (Core.Name "hydra/ext/java/syntax.VariableAccess")

_VariableAccess_expressionName = (Core.FieldName "expressionName")

_VariableAccess_fieldAccess = (Core.FieldName "fieldAccess")

data Primary 
  = PrimaryNoNewArray PrimaryNoNewArray
  | PrimaryArrayCreation ArrayCreationExpression
  deriving (Eq, Ord, Read, Show)

_Primary = (Core.Name "hydra/ext/java/syntax.Primary")

_Primary_noNewArray = (Core.FieldName "noNewArray")

_Primary_arrayCreation = (Core.FieldName "arrayCreation")

data PrimaryNoNewArray 
  = PrimaryNoNewArrayLiteral Literal
  | PrimaryNoNewArrayClassLiteral ClassLiteral
  | PrimaryNoNewArrayThis 
  | PrimaryNoNewArrayDotThis TypeName
  | PrimaryNoNewArrayParens Expression
  | PrimaryNoNewArrayClassInstance ClassInstanceCreationExpression
  | PrimaryNoNewArrayFieldAccess FieldAccess
  | PrimaryNoNewArrayArrayAccess ArrayAccess
  | PrimaryNoNewArrayMethodInvocation MethodInvocation
  | PrimaryNoNewArrayMethodReference MethodReference
  deriving (Eq, Ord, Read, Show)

_PrimaryNoNewArray = (Core.Name "hydra/ext/java/syntax.PrimaryNoNewArray")

_PrimaryNoNewArray_literal = (Core.FieldName "literal")

_PrimaryNoNewArray_classLiteral = (Core.FieldName "classLiteral")

_PrimaryNoNewArray_this = (Core.FieldName "this")

_PrimaryNoNewArray_dotThis = (Core.FieldName "dotThis")

_PrimaryNoNewArray_parens = (Core.FieldName "parens")

_PrimaryNoNewArray_classInstance = (Core.FieldName "classInstance")

_PrimaryNoNewArray_fieldAccess = (Core.FieldName "fieldAccess")

_PrimaryNoNewArray_arrayAccess = (Core.FieldName "arrayAccess")

_PrimaryNoNewArray_methodInvocation = (Core.FieldName "methodInvocation")

_PrimaryNoNewArray_methodReference = (Core.FieldName "methodReference")

data ClassLiteral 
  = ClassLiteralType TypeNameArray
  | ClassLiteralNumericType NumericTypeArray
  | ClassLiteralBoolean BooleanArray
  | ClassLiteralVoid 
  deriving (Eq, Ord, Read, Show)

_ClassLiteral = (Core.Name "hydra/ext/java/syntax.ClassLiteral")

_ClassLiteral_type = (Core.FieldName "type")

_ClassLiteral_numericType = (Core.FieldName "numericType")

_ClassLiteral_boolean = (Core.FieldName "boolean")

_ClassLiteral_void = (Core.FieldName "void")

data TypeNameArray 
  = TypeNameArraySimple TypeName
  | TypeNameArrayArray TypeNameArray
  deriving (Eq, Ord, Read, Show)

_TypeNameArray = (Core.Name "hydra/ext/java/syntax.TypeNameArray")

_TypeNameArray_simple = (Core.FieldName "simple")

_TypeNameArray_array = (Core.FieldName "array")

data NumericTypeArray 
  = NumericTypeArraySimple NumericType
  | NumericTypeArrayArray NumericTypeArray
  deriving (Eq, Ord, Read, Show)

_NumericTypeArray = (Core.Name "hydra/ext/java/syntax.NumericTypeArray")

_NumericTypeArray_simple = (Core.FieldName "simple")

_NumericTypeArray_array = (Core.FieldName "array")

data BooleanArray 
  = BooleanArraySimple 
  | BooleanArrayArray BooleanArray
  deriving (Eq, Ord, Read, Show)

_BooleanArray = (Core.Name "hydra/ext/java/syntax.BooleanArray")

_BooleanArray_simple = (Core.FieldName "simple")

_BooleanArray_array = (Core.FieldName "array")

data ClassInstanceCreationExpression 
  = ClassInstanceCreationExpression {
    classInstanceCreationExpressionQualifier :: (Maybe ClassInstanceCreationExpression_Qualifier),
    classInstanceCreationExpressionExpression :: UnqualifiedClassInstanceCreationExpression
  }
  deriving (Eq, Ord, Read, Show)

_ClassInstanceCreationExpression = (Core.Name "hydra/ext/java/syntax.ClassInstanceCreationExpression")

_ClassInstanceCreationExpression_qualifier = (Core.FieldName "qualifier")

_ClassInstanceCreationExpression_expression = (Core.FieldName "expression")

data ClassInstanceCreationExpression_Qualifier 
  = ClassInstanceCreationExpression_QualifierExpression ExpressionName
  | ClassInstanceCreationExpression_QualifierPrimary Primary
  deriving (Eq, Ord, Read, Show)

_ClassInstanceCreationExpression_Qualifier = (Core.Name "hydra/ext/java/syntax.ClassInstanceCreationExpression.Qualifier")

_ClassInstanceCreationExpression_Qualifier_expression = (Core.FieldName "expression")

_ClassInstanceCreationExpression_Qualifier_primary = (Core.FieldName "primary")

data UnqualifiedClassInstanceCreationExpression 
  = UnqualifiedClassInstanceCreationExpression {
    unqualifiedClassInstanceCreationExpressionTypeArguments :: [TypeArgument],
    unqualifiedClassInstanceCreationExpressionClassOrInterface :: ClassOrInterfaceTypeToInstantiate,
    unqualifiedClassInstanceCreationExpressionArguments :: [Expression],
    unqualifiedClassInstanceCreationExpressionBody :: (Maybe ClassBody)
  }
  deriving (Eq, Ord, Read, Show)

_UnqualifiedClassInstanceCreationExpression = (Core.Name "hydra/ext/java/syntax.UnqualifiedClassInstanceCreationExpression")

_UnqualifiedClassInstanceCreationExpression_typeArguments = (Core.FieldName "typeArguments")

_UnqualifiedClassInstanceCreationExpression_classOrInterface = (Core.FieldName "classOrInterface")

_UnqualifiedClassInstanceCreationExpression_arguments = (Core.FieldName "arguments")

_UnqualifiedClassInstanceCreationExpression_body = (Core.FieldName "body")

data ClassOrInterfaceTypeToInstantiate 
  = ClassOrInterfaceTypeToInstantiate {
    classOrInterfaceTypeToInstantiateIdentifiers :: [AnnotatedIdentifier],
    classOrInterfaceTypeToInstantiateTypeArguments :: (Maybe TypeArgumentsOrDiamond)
  }
  deriving (Eq, Ord, Read, Show)

_ClassOrInterfaceTypeToInstantiate = (Core.Name "hydra/ext/java/syntax.ClassOrInterfaceTypeToInstantiate")

_ClassOrInterfaceTypeToInstantiate_identifiers = (Core.FieldName "identifiers")

_ClassOrInterfaceTypeToInstantiate_typeArguments = (Core.FieldName "typeArguments")

data AnnotatedIdentifier 
  = AnnotatedIdentifier {
    annotatedIdentifierAnnotations :: [Annotation],
    annotatedIdentifierIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_AnnotatedIdentifier = (Core.Name "hydra/ext/java/syntax.AnnotatedIdentifier")

_AnnotatedIdentifier_annotations = (Core.FieldName "annotations")

_AnnotatedIdentifier_identifier = (Core.FieldName "identifier")

data TypeArgumentsOrDiamond 
  = TypeArgumentsOrDiamondArguments [TypeArgument]
  | TypeArgumentsOrDiamondDiamond 
  deriving (Eq, Ord, Read, Show)

_TypeArgumentsOrDiamond = (Core.Name "hydra/ext/java/syntax.TypeArgumentsOrDiamond")

_TypeArgumentsOrDiamond_arguments = (Core.FieldName "arguments")

_TypeArgumentsOrDiamond_diamond = (Core.FieldName "diamond")

data FieldAccess 
  = FieldAccess {
    fieldAccessIdentifier :: Identifier,
    fieldAccessVariant :: FieldAccess_Variant
  }
  deriving (Eq, Ord, Read, Show)

_FieldAccess = (Core.Name "hydra/ext/java/syntax.FieldAccess")

_FieldAccess_identifier = (Core.FieldName "identifier")

_FieldAccess_variant = (Core.FieldName "variant")

data FieldAccess_Variant 
  = FieldAccess_VariantPrimary Primary
  | FieldAccess_VariantSuper 
  | FieldAccess_VariantTyped TypeName
  deriving (Eq, Ord, Read, Show)

_FieldAccess_Variant = (Core.Name "hydra/ext/java/syntax.FieldAccess.Variant")

_FieldAccess_Variant_primary = (Core.FieldName "primary")

_FieldAccess_Variant_super = (Core.FieldName "super")

_FieldAccess_Variant_typed = (Core.FieldName "typed")

data ArrayAccess 
  = ArrayAccess {
    arrayAccessExpression :: (Maybe Expression),
    arrayAccessVariant :: ArrayAccess_Variant
  }
  deriving (Eq, Ord, Read, Show)

_ArrayAccess = (Core.Name "hydra/ext/java/syntax.ArrayAccess")

_ArrayAccess_expression = (Core.FieldName "expression")

_ArrayAccess_variant = (Core.FieldName "variant")

data ArrayAccess_Variant 
  = ArrayAccess_VariantName ExpressionName
  | ArrayAccess_VariantPrimary PrimaryNoNewArray
  deriving (Eq, Ord, Read, Show)

_ArrayAccess_Variant = (Core.Name "hydra/ext/java/syntax.ArrayAccess.Variant")

_ArrayAccess_Variant_name = (Core.FieldName "name")

_ArrayAccess_Variant_primary = (Core.FieldName "primary")

data MethodInvocation 
  = MethodInvocation {
    methodInvocationId :: [MethodInvocation_Id],
    methodInvocationArguments :: [Expression],
    methodInvocationVariant :: MethodInvocation_Variant
  }
  deriving (Eq, Ord, Read, Show)

_MethodInvocation = (Core.Name "hydra/ext/java/syntax.MethodInvocation")

_MethodInvocation_id = (Core.FieldName "id")

_MethodInvocation_arguments = (Core.FieldName "arguments")

_MethodInvocation_variant = (Core.FieldName "variant")

data MethodInvocation_Variant 
  = MethodInvocation_VariantMethod MethodName
  | MethodInvocation_VariantType TypeName
  | MethodInvocation_VariantExpression ExpressionName
  | MethodInvocation_VariantPrimary Primary
  | MethodInvocation_VariantSuper 
  | MethodInvocation_VariantTypeSuper TypeName
  deriving (Eq, Ord, Read, Show)

_MethodInvocation_Variant = (Core.Name "hydra/ext/java/syntax.MethodInvocation.Variant")

_MethodInvocation_Variant_method = (Core.FieldName "method")

_MethodInvocation_Variant_type = (Core.FieldName "type")

_MethodInvocation_Variant_expression = (Core.FieldName "expression")

_MethodInvocation_Variant_primary = (Core.FieldName "primary")

_MethodInvocation_Variant_super = (Core.FieldName "super")

_MethodInvocation_Variant_typeSuper = (Core.FieldName "typeSuper")

data MethodInvocation_Id 
  = MethodInvocation_Id {
    methodInvocation_IdTypeArguments :: [TypeArgument],
    methodInvocation_IdIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_MethodInvocation_Id = (Core.Name "hydra/ext/java/syntax.MethodInvocation.Id")

_MethodInvocation_Id_typeArguments = (Core.FieldName "typeArguments")

_MethodInvocation_Id_identifier = (Core.FieldName "identifier")

data MethodReference 
  = MethodReferenceExpression MethodReference_Expression
  | MethodReferencePrimary MethodReference_Primary
  | MethodReferenceReferenceType MethodReference_ReferenceType
  | MethodReferenceSuper MethodReference_Super
  | MethodReferenceNew MethodReference_New
  | MethodReferenceArray MethodReference_Array
  deriving (Eq, Ord, Read, Show)

_MethodReference = (Core.Name "hydra/ext/java/syntax.MethodReference")

_MethodReference_expression = (Core.FieldName "expression")

_MethodReference_primary = (Core.FieldName "primary")

_MethodReference_referenceType = (Core.FieldName "referenceType")

_MethodReference_super = (Core.FieldName "super")

_MethodReference_new = (Core.FieldName "new")

_MethodReference_array = (Core.FieldName "array")

data MethodReference_Expression 
  = MethodReference_Expression {
    methodReference_ExpressionName :: ExpressionName,
    methodReference_ExpressionTypeArguments :: [TypeArgument],
    methodReference_ExpressionIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_MethodReference_Expression = (Core.Name "hydra/ext/java/syntax.MethodReference.Expression")

_MethodReference_Expression_name = (Core.FieldName "name")

_MethodReference_Expression_typeArguments = (Core.FieldName "typeArguments")

_MethodReference_Expression_identifier = (Core.FieldName "identifier")

data MethodReference_Primary 
  = MethodReference_Primary {
    methodReference_PrimaryPrimary :: Primary,
    methodReference_PrimaryTypeArguments :: [TypeArgument],
    methodReference_PrimaryIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_MethodReference_Primary = (Core.Name "hydra/ext/java/syntax.MethodReference.Primary")

_MethodReference_Primary_primary = (Core.FieldName "primary")

_MethodReference_Primary_typeArguments = (Core.FieldName "typeArguments")

_MethodReference_Primary_identifier = (Core.FieldName "identifier")

data MethodReference_ReferenceType 
  = MethodReference_ReferenceType {
    methodReference_ReferenceTypeReferenceType :: ReferenceType,
    methodReference_ReferenceTypeTypeArguments :: [TypeArgument],
    methodReference_ReferenceTypeIdentifier :: Identifier
  }
  deriving (Eq, Ord, Read, Show)

_MethodReference_ReferenceType = (Core.Name "hydra/ext/java/syntax.MethodReference.ReferenceType")

_MethodReference_ReferenceType_referenceType = (Core.FieldName "referenceType")

_MethodReference_ReferenceType_typeArguments = (Core.FieldName "typeArguments")

_MethodReference_ReferenceType_identifier = (Core.FieldName "identifier")

data MethodReference_Super 
  = MethodReference_Super {
    methodReference_SuperTypeArguments :: [TypeArgument],
    methodReference_SuperIdentifier :: Identifier,
    methodReference_SuperSuper :: Bool
  }
  deriving (Eq, Ord, Read, Show)

_MethodReference_Super = (Core.Name "hydra/ext/java/syntax.MethodReference.Super")

_MethodReference_Super_typeArguments = (Core.FieldName "typeArguments")

_MethodReference_Super_identifier = (Core.FieldName "identifier")

_MethodReference_Super_super = (Core.FieldName "super")

data MethodReference_New 
  = MethodReference_New {
    methodReference_NewClassType :: ClassType,
    methodReference_NewTypeArguments :: [TypeArgument]
  }
  deriving (Eq, Ord, Read, Show)

_MethodReference_New = (Core.Name "hydra/ext/java/syntax.MethodReference.New")

_MethodReference_New_classType = (Core.FieldName "classType")

_MethodReference_New_typeArguments = (Core.FieldName "typeArguments")

newtype MethodReference_Array 
  = MethodReference_Array {
    unMethodReference_Array :: ArrayType
  }
  deriving (Eq, Ord, Read, Show)

_MethodReference_Array = (Core.Name "hydra/ext/java/syntax.MethodReference.Array")

data ArrayCreationExpression 
  = ArrayCreationExpressionPrimitive ArrayCreationExpression_Primitive
  | ArrayCreationExpressionClassOrInterface ArrayCreationExpression_ClassOrInterface
  | ArrayCreationExpressionPrimitiveArray ArrayCreationExpression_PrimitiveArray
  | ArrayCreationExpressionClassOrInterfaceArray ArrayCreationExpression_ClassOrInterfaceArray
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression = (Core.Name "hydra/ext/java/syntax.ArrayCreationExpression")

_ArrayCreationExpression_primitive = (Core.FieldName "primitive")

_ArrayCreationExpression_classOrInterface = (Core.FieldName "classOrInterface")

_ArrayCreationExpression_primitiveArray = (Core.FieldName "primitiveArray")

_ArrayCreationExpression_classOrInterfaceArray = (Core.FieldName "classOrInterfaceArray")

data ArrayCreationExpression_Primitive 
  = ArrayCreationExpression_Primitive {
    arrayCreationExpression_PrimitiveType :: PrimitiveTypeWithAnnotations,
    arrayCreationExpression_PrimitiveDimExprs :: [DimExpr],
    arrayCreationExpression_PrimitiveDims :: (Maybe Dims)
  }
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_Primitive = (Core.Name "hydra/ext/java/syntax.ArrayCreationExpression.Primitive")

_ArrayCreationExpression_Primitive_type = (Core.FieldName "type")

_ArrayCreationExpression_Primitive_dimExprs = (Core.FieldName "dimExprs")

_ArrayCreationExpression_Primitive_dims = (Core.FieldName "dims")

data ArrayCreationExpression_ClassOrInterface 
  = ArrayCreationExpression_ClassOrInterface {
    arrayCreationExpression_ClassOrInterfaceType :: ClassOrInterfaceType,
    arrayCreationExpression_ClassOrInterfaceDimExprs :: [DimExpr],
    arrayCreationExpression_ClassOrInterfaceDims :: (Maybe Dims)
  }
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_ClassOrInterface = (Core.Name "hydra/ext/java/syntax.ArrayCreationExpression.ClassOrInterface")

_ArrayCreationExpression_ClassOrInterface_type = (Core.FieldName "type")

_ArrayCreationExpression_ClassOrInterface_dimExprs = (Core.FieldName "dimExprs")

_ArrayCreationExpression_ClassOrInterface_dims = (Core.FieldName "dims")

data ArrayCreationExpression_PrimitiveArray 
  = ArrayCreationExpression_PrimitiveArray {
    arrayCreationExpression_PrimitiveArrayType :: PrimitiveTypeWithAnnotations,
    arrayCreationExpression_PrimitiveArrayDims :: [Dims],
    arrayCreationExpression_PrimitiveArrayArray :: ArrayInitializer
  }
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_PrimitiveArray = (Core.Name "hydra/ext/java/syntax.ArrayCreationExpression.PrimitiveArray")

_ArrayCreationExpression_PrimitiveArray_type = (Core.FieldName "type")

_ArrayCreationExpression_PrimitiveArray_dims = (Core.FieldName "dims")

_ArrayCreationExpression_PrimitiveArray_array = (Core.FieldName "array")

data ArrayCreationExpression_ClassOrInterfaceArray 
  = ArrayCreationExpression_ClassOrInterfaceArray {
    arrayCreationExpression_ClassOrInterfaceArrayType :: ClassOrInterfaceType,
    arrayCreationExpression_ClassOrInterfaceArrayDims :: [Dims],
    arrayCreationExpression_ClassOrInterfaceArrayArray :: ArrayInitializer
  }
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_ClassOrInterfaceArray = (Core.Name "hydra/ext/java/syntax.ArrayCreationExpression.ClassOrInterfaceArray")

_ArrayCreationExpression_ClassOrInterfaceArray_type = (Core.FieldName "type")

_ArrayCreationExpression_ClassOrInterfaceArray_dims = (Core.FieldName "dims")

_ArrayCreationExpression_ClassOrInterfaceArray_array = (Core.FieldName "array")

data DimExpr 
  = DimExpr {
    dimExprAnnotations :: [Annotation],
    dimExprExpression :: (Maybe Expression)
  }
  deriving (Eq, Ord, Read, Show)

_DimExpr = (Core.Name "hydra/ext/java/syntax.DimExpr")

_DimExpr_annotations = (Core.FieldName "annotations")

_DimExpr_expression = (Core.FieldName "expression")

data Expression 
  = ExpressionLambda LambdaExpression
  | ExpressionAssignment AssignmentExpression
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/ext/java/syntax.Expression")

_Expression_lambda = (Core.FieldName "lambda")

_Expression_assignment = (Core.FieldName "assignment")

data LambdaExpression 
  = LambdaExpression {
    lambdaExpressionParameters :: LambdaParameters,
    lambdaExpressionBody :: LambdaBody
  }
  deriving (Eq, Ord, Read, Show)

_LambdaExpression = (Core.Name "hydra/ext/java/syntax.LambdaExpression")

_LambdaExpression_parameters = (Core.FieldName "parameters")

_LambdaExpression_body = (Core.FieldName "body")

data LambdaParameters 
  = LambdaParametersTuple [LambdaParameters]
  | LambdaParametersSingle Identifier
  deriving (Eq, Ord, Read, Show)

_LambdaParameters = (Core.Name "hydra/ext/java/syntax.LambdaParameters")

_LambdaParameters_tuple = (Core.FieldName "tuple")

_LambdaParameters_single = (Core.FieldName "single")

data LambdaParameter 
  = LambdaParameterNormal LambdaParameter_Normal
  | LambdaParameterVariableArity VariableArityParameter
  deriving (Eq, Ord, Read, Show)

_LambdaParameter = (Core.Name "hydra/ext/java/syntax.LambdaParameter")

_LambdaParameter_normal = (Core.FieldName "normal")

_LambdaParameter_variableArity = (Core.FieldName "variableArity")

data LambdaParameter_Normal 
  = LambdaParameter_Normal {
    lambdaParameter_NormalModifiers :: [VariableModifier],
    lambdaParameter_NormalType :: LambdaParameterType,
    lambdaParameter_NormalId :: VariableDeclaratorId
  }
  deriving (Eq, Ord, Read, Show)

_LambdaParameter_Normal = (Core.Name "hydra/ext/java/syntax.LambdaParameter.Normal")

_LambdaParameter_Normal_modifiers = (Core.FieldName "modifiers")

_LambdaParameter_Normal_type = (Core.FieldName "type")

_LambdaParameter_Normal_id = (Core.FieldName "id")

data LambdaParameterType 
  = LambdaParameterTypeType UnannType
  | LambdaParameterTypeVar 
  deriving (Eq, Ord, Read, Show)

_LambdaParameterType = (Core.Name "hydra/ext/java/syntax.LambdaParameterType")

_LambdaParameterType_type = (Core.FieldName "type")

_LambdaParameterType_var = (Core.FieldName "var")

data LambdaBody 
  = LambdaBodyExpression Expression
  | LambdaBodyBlock Block
  deriving (Eq, Ord, Read, Show)

_LambdaBody = (Core.Name "hydra/ext/java/syntax.LambdaBody")

_LambdaBody_expression = (Core.FieldName "expression")

_LambdaBody_block = (Core.FieldName "block")

data AssignmentExpression 
  = AssignmentExpressionConditional ConditionalExpression
  | AssignmentExpressionAssignment Assignment
  deriving (Eq, Ord, Read, Show)

_AssignmentExpression = (Core.Name "hydra/ext/java/syntax.AssignmentExpression")

_AssignmentExpression_conditional = (Core.FieldName "conditional")

_AssignmentExpression_assignment = (Core.FieldName "assignment")

data Assignment 
  = Assignment {
    assignmentLhs :: LeftHandSide,
    assignmentOp :: AssignmentOperator,
    assignmentExpression :: Expression
  }
  deriving (Eq, Ord, Read, Show)

_Assignment = (Core.Name "hydra/ext/java/syntax.Assignment")

_Assignment_lhs = (Core.FieldName "lhs")

_Assignment_op = (Core.FieldName "op")

_Assignment_expression = (Core.FieldName "expression")

data LeftHandSide 
  = LeftHandSideExpressionName ExpressionName
  | LeftHandSideFieldAccess FieldAccess
  | LeftHandSideArrayAccess ArrayAccess
  deriving (Eq, Ord, Read, Show)

_LeftHandSide = (Core.Name "hydra/ext/java/syntax.LeftHandSide")

_LeftHandSide_expressionName = (Core.FieldName "expressionName")

_LeftHandSide_fieldAccess = (Core.FieldName "fieldAccess")

_LeftHandSide_arrayAccess = (Core.FieldName "arrayAccess")

data AssignmentOperator 
  = AssignmentOperatorSimple 
  | AssignmentOperatorTimes 
  | AssignmentOperatorDiv 
  | AssignmentOperatorMod 
  | AssignmentOperatorPlus 
  | AssignmentOperatorMinus 
  | AssignmentOperatorShiftLeft 
  | AssignmentOperatorShiftRight 
  | AssignmentOperatorShiftRightZeroFill 
  | AssignmentOperatorAnd 
  | AssignmentOperatorXor 
  | AssignmentOperatorOr 
  deriving (Eq, Ord, Read, Show)

_AssignmentOperator = (Core.Name "hydra/ext/java/syntax.AssignmentOperator")

_AssignmentOperator_simple = (Core.FieldName "simple")

_AssignmentOperator_times = (Core.FieldName "times")

_AssignmentOperator_div = (Core.FieldName "div")

_AssignmentOperator_mod = (Core.FieldName "mod")

_AssignmentOperator_plus = (Core.FieldName "plus")

_AssignmentOperator_minus = (Core.FieldName "minus")

_AssignmentOperator_shiftLeft = (Core.FieldName "shiftLeft")

_AssignmentOperator_shiftRight = (Core.FieldName "shiftRight")

_AssignmentOperator_shiftRightZeroFill = (Core.FieldName "shiftRightZeroFill")

_AssignmentOperator_and = (Core.FieldName "and")

_AssignmentOperator_xor = (Core.FieldName "xor")

_AssignmentOperator_or = (Core.FieldName "or")

data ConditionalExpression 
  = ConditionalExpressionSimple ConditionalOrExpression
  | ConditionalExpressionTernaryCond ConditionalExpression_TernaryCond
  | ConditionalExpressionTernaryLambda ConditionalExpression_TernaryLambda
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression = (Core.Name "hydra/ext/java/syntax.ConditionalExpression")

_ConditionalExpression_simple = (Core.FieldName "simple")

_ConditionalExpression_ternaryCond = (Core.FieldName "ternaryCond")

_ConditionalExpression_ternaryLambda = (Core.FieldName "ternaryLambda")

data ConditionalExpression_TernaryCond 
  = ConditionalExpression_TernaryCond {
    conditionalExpression_TernaryCondCond :: ConditionalOrExpression,
    conditionalExpression_TernaryCondIfTrue :: Expression,
    conditionalExpression_TernaryCondIfFalse :: ConditionalExpression
  }
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression_TernaryCond = (Core.Name "hydra/ext/java/syntax.ConditionalExpression.TernaryCond")

_ConditionalExpression_TernaryCond_cond = (Core.FieldName "cond")

_ConditionalExpression_TernaryCond_ifTrue = (Core.FieldName "ifTrue")

_ConditionalExpression_TernaryCond_ifFalse = (Core.FieldName "ifFalse")

data ConditionalExpression_TernaryLambda 
  = ConditionalExpression_TernaryLambda {
    conditionalExpression_TernaryLambdaCond :: ConditionalOrExpression,
    conditionalExpression_TernaryLambdaIfTrue :: Expression,
    conditionalExpression_TernaryLambdaIfFalse :: LambdaExpression
  }
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression_TernaryLambda = (Core.Name "hydra/ext/java/syntax.ConditionalExpression.TernaryLambda")

_ConditionalExpression_TernaryLambda_cond = (Core.FieldName "cond")

_ConditionalExpression_TernaryLambda_ifTrue = (Core.FieldName "ifTrue")

_ConditionalExpression_TernaryLambda_ifFalse = (Core.FieldName "ifFalse")

-- Note: list cannot be empty
newtype ConditionalOrExpression 
  = ConditionalOrExpression {
    unConditionalOrExpression :: [ConditionalAndExpression]
  }
  deriving (Eq, Ord, Read, Show)

_ConditionalOrExpression = (Core.Name "hydra/ext/java/syntax.ConditionalOrExpression")

-- Note: list cannot be empty
newtype ConditionalAndExpression 
  = ConditionalAndExpression {
    unConditionalAndExpression :: [InclusiveOrExpression]
  }
  deriving (Eq, Ord, Read, Show)

_ConditionalAndExpression = (Core.Name "hydra/ext/java/syntax.ConditionalAndExpression")

-- Note: list cannot be empty
newtype InclusiveOrExpression 
  = InclusiveOrExpression {
    unInclusiveOrExpression :: [ExclusiveOrExpression]
  }
  deriving (Eq, Ord, Read, Show)

_InclusiveOrExpression = (Core.Name "hydra/ext/java/syntax.InclusiveOrExpression")

-- Note: list cannot be empty
newtype ExclusiveOrExpression 
  = ExclusiveOrExpression {
    unExclusiveOrExpression :: [AndExpression]
  }
  deriving (Eq, Ord, Read, Show)

_ExclusiveOrExpression = (Core.Name "hydra/ext/java/syntax.ExclusiveOrExpression")

-- Note: list cannot be empty
newtype AndExpression 
  = AndExpression {
    unAndExpression :: [EqualityExpression]
  }
  deriving (Eq, Ord, Read, Show)

_AndExpression = (Core.Name "hydra/ext/java/syntax.AndExpression")

data EqualityExpression 
  = EqualityExpressionUnary RelationalExpression
  | EqualityExpressionEqual EqualityExpression_Binary
  | EqualityExpressionNotEqual EqualityExpression_Binary
  deriving (Eq, Ord, Read, Show)

_EqualityExpression = (Core.Name "hydra/ext/java/syntax.EqualityExpression")

_EqualityExpression_unary = (Core.FieldName "unary")

_EqualityExpression_equal = (Core.FieldName "equal")

_EqualityExpression_notEqual = (Core.FieldName "notEqual")

data EqualityExpression_Binary 
  = EqualityExpression_Binary {
    equalityExpression_BinaryLhs :: EqualityExpression,
    equalityExpression_BinaryRhs :: RelationalExpression
  }
  deriving (Eq, Ord, Read, Show)

_EqualityExpression_Binary = (Core.Name "hydra/ext/java/syntax.EqualityExpression.Binary")

_EqualityExpression_Binary_lhs = (Core.FieldName "lhs")

_EqualityExpression_Binary_rhs = (Core.FieldName "rhs")

data RelationalExpression 
  = RelationalExpressionSimple ShiftExpression
  | RelationalExpressionLessThan RelationalExpression_LessThan
  | RelationalExpressionGreaterThan RelationalExpression_GreaterThan
  | RelationalExpressionLessThanEqual RelationalExpression_LessThanEqual
  | RelationalExpressionGreaterThanEqual RelationalExpression_GreaterThanEqual
  | RelationalExpressionInstanceof RelationalExpression_InstanceOf
  deriving (Eq, Ord, Read, Show)

_RelationalExpression = (Core.Name "hydra/ext/java/syntax.RelationalExpression")

_RelationalExpression_simple = (Core.FieldName "simple")

_RelationalExpression_lessThan = (Core.FieldName "lessThan")

_RelationalExpression_greaterThan = (Core.FieldName "greaterThan")

_RelationalExpression_lessThanEqual = (Core.FieldName "lessThanEqual")

_RelationalExpression_greaterThanEqual = (Core.FieldName "greaterThanEqual")

_RelationalExpression_instanceof = (Core.FieldName "instanceof")

data RelationalExpression_LessThan 
  = RelationalExpression_LessThan {
    relationalExpression_LessThanLhs :: RelationalExpression,
    relationalExpression_LessThanRhs :: ShiftExpression
  }
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_LessThan = (Core.Name "hydra/ext/java/syntax.RelationalExpression.LessThan")

_RelationalExpression_LessThan_lhs = (Core.FieldName "lhs")

_RelationalExpression_LessThan_rhs = (Core.FieldName "rhs")

data RelationalExpression_GreaterThan 
  = RelationalExpression_GreaterThan {
    relationalExpression_GreaterThanLhs :: RelationalExpression,
    relationalExpression_GreaterThanRhs :: ShiftExpression
  }
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_GreaterThan = (Core.Name "hydra/ext/java/syntax.RelationalExpression.GreaterThan")

_RelationalExpression_GreaterThan_lhs = (Core.FieldName "lhs")

_RelationalExpression_GreaterThan_rhs = (Core.FieldName "rhs")

data RelationalExpression_LessThanEqual 
  = RelationalExpression_LessThanEqual {
    relationalExpression_LessThanEqualLhs :: RelationalExpression,
    relationalExpression_LessThanEqualRhs :: ShiftExpression
  }
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_LessThanEqual = (Core.Name "hydra/ext/java/syntax.RelationalExpression.LessThanEqual")

_RelationalExpression_LessThanEqual_lhs = (Core.FieldName "lhs")

_RelationalExpression_LessThanEqual_rhs = (Core.FieldName "rhs")

data RelationalExpression_GreaterThanEqual 
  = RelationalExpression_GreaterThanEqual {
    relationalExpression_GreaterThanEqualLhs :: RelationalExpression,
    relationalExpression_GreaterThanEqualRhs :: ShiftExpression
  }
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_GreaterThanEqual = (Core.Name "hydra/ext/java/syntax.RelationalExpression.GreaterThanEqual")

_RelationalExpression_GreaterThanEqual_lhs = (Core.FieldName "lhs")

_RelationalExpression_GreaterThanEqual_rhs = (Core.FieldName "rhs")

data RelationalExpression_InstanceOf 
  = RelationalExpression_InstanceOf {
    relationalExpression_InstanceOfLhs :: RelationalExpression,
    relationalExpression_InstanceOfRhs :: ReferenceType
  }
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_InstanceOf = (Core.Name "hydra/ext/java/syntax.RelationalExpression.InstanceOf")

_RelationalExpression_InstanceOf_lhs = (Core.FieldName "lhs")

_RelationalExpression_InstanceOf_rhs = (Core.FieldName "rhs")

data ShiftExpression 
  = ShiftExpressionUnary AdditiveExpression
  | ShiftExpressionShiftLeft ShiftExpression_Binary
  | ShiftExpressionShiftRight ShiftExpression_Binary
  | ShiftExpressionShiftRightZeroFill ShiftExpression_Binary
  deriving (Eq, Ord, Read, Show)

_ShiftExpression = (Core.Name "hydra/ext/java/syntax.ShiftExpression")

_ShiftExpression_unary = (Core.FieldName "unary")

_ShiftExpression_shiftLeft = (Core.FieldName "shiftLeft")

_ShiftExpression_shiftRight = (Core.FieldName "shiftRight")

_ShiftExpression_shiftRightZeroFill = (Core.FieldName "shiftRightZeroFill")

data ShiftExpression_Binary 
  = ShiftExpression_Binary {
    shiftExpression_BinaryLhs :: ShiftExpression,
    shiftExpression_BinaryRhs :: AdditiveExpression
  }
  deriving (Eq, Ord, Read, Show)

_ShiftExpression_Binary = (Core.Name "hydra/ext/java/syntax.ShiftExpression.Binary")

_ShiftExpression_Binary_lhs = (Core.FieldName "lhs")

_ShiftExpression_Binary_rhs = (Core.FieldName "rhs")

data AdditiveExpression 
  = AdditiveExpressionUnary MultiplicativeExpression
  | AdditiveExpressionPlus AdditiveExpression_Binary
  | AdditiveExpressionMinus AdditiveExpression_Binary
  deriving (Eq, Ord, Read, Show)

_AdditiveExpression = (Core.Name "hydra/ext/java/syntax.AdditiveExpression")

_AdditiveExpression_unary = (Core.FieldName "unary")

_AdditiveExpression_plus = (Core.FieldName "plus")

_AdditiveExpression_minus = (Core.FieldName "minus")

data AdditiveExpression_Binary 
  = AdditiveExpression_Binary {
    additiveExpression_BinaryLhs :: AdditiveExpression,
    additiveExpression_BinaryRhs :: MultiplicativeExpression
  }
  deriving (Eq, Ord, Read, Show)

_AdditiveExpression_Binary = (Core.Name "hydra/ext/java/syntax.AdditiveExpression.Binary")

_AdditiveExpression_Binary_lhs = (Core.FieldName "lhs")

_AdditiveExpression_Binary_rhs = (Core.FieldName "rhs")

data MultiplicativeExpression 
  = MultiplicativeExpressionUnary UnaryExpression
  | MultiplicativeExpressionTimes MultiplicativeExpression_Binary
  | MultiplicativeExpressionDivide MultiplicativeExpression_Binary
  | MultiplicativeExpressionMod MultiplicativeExpression_Binary
  deriving (Eq, Ord, Read, Show)

_MultiplicativeExpression = (Core.Name "hydra/ext/java/syntax.MultiplicativeExpression")

_MultiplicativeExpression_unary = (Core.FieldName "unary")

_MultiplicativeExpression_times = (Core.FieldName "times")

_MultiplicativeExpression_divide = (Core.FieldName "divide")

_MultiplicativeExpression_mod = (Core.FieldName "mod")

data MultiplicativeExpression_Binary 
  = MultiplicativeExpression_Binary {
    multiplicativeExpression_BinaryLhs :: MultiplicativeExpression,
    multiplicativeExpression_BinaryRhs :: UnaryExpression
  }
  deriving (Eq, Ord, Read, Show)

_MultiplicativeExpression_Binary = (Core.Name "hydra/ext/java/syntax.MultiplicativeExpression.Binary")

_MultiplicativeExpression_Binary_lhs = (Core.FieldName "lhs")

_MultiplicativeExpression_Binary_rhs = (Core.FieldName "rhs")

data UnaryExpression 
  = UnaryExpressionPreIncrement PreIncrementExpression
  | UnaryExpressionPreDecrement PreDecrementExpression
  | UnaryExpressionPlus UnaryExpression
  | UnaryExpressionMinus UnaryExpression
  | UnaryExpressionOther UnaryExpressionNotPlusMinus
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra/ext/java/syntax.UnaryExpression")

_UnaryExpression_preIncrement = (Core.FieldName "preIncrement")

_UnaryExpression_preDecrement = (Core.FieldName "preDecrement")

_UnaryExpression_plus = (Core.FieldName "plus")

_UnaryExpression_minus = (Core.FieldName "minus")

_UnaryExpression_other = (Core.FieldName "other")

newtype PreIncrementExpression 
  = PreIncrementExpression {
    unPreIncrementExpression :: UnaryExpression
  }
  deriving (Eq, Ord, Read, Show)

_PreIncrementExpression = (Core.Name "hydra/ext/java/syntax.PreIncrementExpression")

newtype PreDecrementExpression 
  = PreDecrementExpression {
    unPreDecrementExpression :: UnaryExpression
  }
  deriving (Eq, Ord, Read, Show)

_PreDecrementExpression = (Core.Name "hydra/ext/java/syntax.PreDecrementExpression")

data UnaryExpressionNotPlusMinus 
  = UnaryExpressionNotPlusMinusPostfix PostfixExpression
  | UnaryExpressionNotPlusMinusTilde UnaryExpression
  | UnaryExpressionNotPlusMinusNot UnaryExpression
  | UnaryExpressionNotPlusMinusCast CastExpression
  deriving (Eq, Ord, Read, Show)

_UnaryExpressionNotPlusMinus = (Core.Name "hydra/ext/java/syntax.UnaryExpressionNotPlusMinus")

_UnaryExpressionNotPlusMinus_postfix = (Core.FieldName "postfix")

_UnaryExpressionNotPlusMinus_tilde = (Core.FieldName "tilde")

_UnaryExpressionNotPlusMinus_not = (Core.FieldName "not")

_UnaryExpressionNotPlusMinus_cast = (Core.FieldName "cast")

data PostfixExpression 
  = PostfixExpressionPrimary Primary
  | PostfixExpressionName ExpressionName
  | PostfixExpressionPostIncrement PostIncrementExpression
  | PostfixExpressionPostDecrement PostDecrementExpression
  deriving (Eq, Ord, Read, Show)

_PostfixExpression = (Core.Name "hydra/ext/java/syntax.PostfixExpression")

_PostfixExpression_primary = (Core.FieldName "primary")

_PostfixExpression_name = (Core.FieldName "name")

_PostfixExpression_postIncrement = (Core.FieldName "postIncrement")

_PostfixExpression_postDecrement = (Core.FieldName "postDecrement")

newtype PostIncrementExpression 
  = PostIncrementExpression {
    unPostIncrementExpression :: PostfixExpression
  }
  deriving (Eq, Ord, Read, Show)

_PostIncrementExpression = (Core.Name "hydra/ext/java/syntax.PostIncrementExpression")

newtype PostDecrementExpression 
  = PostDecrementExpression {
    unPostDecrementExpression :: PostfixExpression
  }
  deriving (Eq, Ord, Read, Show)

_PostDecrementExpression = (Core.Name "hydra/ext/java/syntax.PostDecrementExpression")

data CastExpression 
  = CastExpressionPrimitive CastExpression_Primitive
  | CastExpressionNotPlusMinus CastExpression_NotPlusMinus
  | CastExpressionLambda CastExpression_Lambda
  deriving (Eq, Ord, Read, Show)

_CastExpression = (Core.Name "hydra/ext/java/syntax.CastExpression")

_CastExpression_primitive = (Core.FieldName "primitive")

_CastExpression_notPlusMinus = (Core.FieldName "notPlusMinus")

_CastExpression_lambda = (Core.FieldName "lambda")

data CastExpression_Primitive 
  = CastExpression_Primitive {
    castExpression_PrimitiveType :: (Maybe PrimitiveTypeWithAnnotations),
    castExpression_PrimitiveExpression :: UnaryExpression
  }
  deriving (Eq, Ord, Read, Show)

_CastExpression_Primitive = (Core.Name "hydra/ext/java/syntax.CastExpression.Primitive")

_CastExpression_Primitive_type = (Core.FieldName "type")

_CastExpression_Primitive_expression = (Core.FieldName "expression")

data CastExpression_NotPlusMinus 
  = CastExpression_NotPlusMinus {
    castExpression_NotPlusMinusRefAndBounds :: (Maybe CastExpression_RefAndBounds),
    castExpression_NotPlusMinusExpression :: UnaryExpression
  }
  deriving (Eq, Ord, Read, Show)

_CastExpression_NotPlusMinus = (Core.Name "hydra/ext/java/syntax.CastExpression.NotPlusMinus")

_CastExpression_NotPlusMinus_refAndBounds = (Core.FieldName "refAndBounds")

_CastExpression_NotPlusMinus_expression = (Core.FieldName "expression")

data CastExpression_Lambda 
  = CastExpression_Lambda {
    castExpression_LambdaRefAndBounds :: (Maybe CastExpression_RefAndBounds),
    castExpression_LambdaExpression :: LambdaExpression
  }
  deriving (Eq, Ord, Read, Show)

_CastExpression_Lambda = (Core.Name "hydra/ext/java/syntax.CastExpression.Lambda")

_CastExpression_Lambda_refAndBounds = (Core.FieldName "refAndBounds")

_CastExpression_Lambda_expression = (Core.FieldName "expression")

data CastExpression_RefAndBounds 
  = CastExpression_RefAndBounds {
    castExpression_RefAndBoundsType :: ReferenceType,
    castExpression_RefAndBoundsBounds :: [AdditionalBound]
  }
  deriving (Eq, Ord, Read, Show)

_CastExpression_RefAndBounds = (Core.Name "hydra/ext/java/syntax.CastExpression.RefAndBounds")

_CastExpression_RefAndBounds_type = (Core.FieldName "type")

_CastExpression_RefAndBounds_bounds = (Core.FieldName "bounds")

newtype ConstantExpression 
  = ConstantExpression {
    unConstantExpression :: Expression
  }
  deriving (Eq, Ord, Read, Show)

_ConstantExpression = (Core.Name "hydra/ext/java/syntax.ConstantExpression")