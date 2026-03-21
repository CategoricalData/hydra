# Python Coder Architecture Analysis

> **Status**: Reference document. Describes architecture of the Python coder for future maintainers.
> **Reviewed**: 2026-02-05 - Architecture description remains accurate. Python is fully complete with 100% test parity.

## Executive Summary

The Python coder in `hydra-ext` has evolved organically to handle the complete Hydra kernel generation. While functional and comprehensive, there are opportunities to extract common patterns that could benefit other language coders. This analysis identifies both strengths and areas for refactoring.

---

## Architecture Overview

### File Structure
```
Hydra/Ext/Staging/Python/
├── Coder.hs       (1073 lines) - Main encoding logic
├── Names.hs       (101 lines)  - Name encoding & environment
├── Utils.hs       (235 lines)  - Python AST helpers
└── Serde.hs       (not examined) - Pretty printing
```

### Core Components

1. **PythonModuleMetadata** (lines 28-49): Tracks import dependencies
2. **PyGraph** (lines 51-53): Wraps Graph with metadata
3. **PythonEnvironment** (Names.hs:19-22): Context for encoding
4. **Flow PyGraph**: Stateful computation managing graph + metadata

---

## Design Observations

### Strengths

#### 1. **Sophisticated Metadata Tracking**
The metadata system automatically determines which Python imports are needed:

```haskell
data PythonModuleMetadata = PythonModuleMetadata {
  pythonModuleMetadataUsesCallable :: Bool,
  pythonModuleMetadataUsesCast :: Bool,
  pythonModuleMetadataUsesDataclass :: Bool,
  -- ... 15+ flags
}
```

**Reusable Pattern**: Metadata accumulation during traversal
- Track language-specific feature usage
- Generate imports/headers automatically
- Avoid manual import management

#### 2. **Term Analysis Functions**
```haskell
gatherBindingsAndParams :: PythonEnvironment -> Term
  -> Flow s ([Name], [Name], [Binding], Term, [Type], Type, PythonEnvironment)
```

**Reusable Pattern**: Recursive term decomposition
- Peel off lambdas, lets, type applications
- Accumulate parameters/bindings in context
- Return simplified body + accumulated context

#### 3. **Smart Assignment Detection**
```haskell
isSimpleAssignment :: Term -> Bool
isFunctionCall :: Binding -> Bool
```

**Reusable Pattern**: Context-sensitive encoding decisions
- Simple values → assignments (`x = 42`)
- Complex terms → function definitions (`def x(): ...`)
- Reduces code duplication

#### 4. **Namespace Management**
The `Namespaces` type and `findNamespaces` function automatically discover cross-module dependencies.

**Reusable Pattern**: Dependency analysis
- Scan definitions for external references
- Map to target language import syntax
- Handle focus namespace (current module)

### Areas of Concern

#### 1. **Function Size & Complexity**

**Problem**: Several functions exceed 100 lines:
- `encodeModule`: 76 lines (lines 339-371) - manageable
- `encodeTermMultiline`: 66 lines (lines 597-666)
- `encodeApplication`: 59 lines (lines 76-135)
- `extendMetaForType`: 60+ lines (lines 863-923)

**Impact**: Difficult to understand/modify individual behaviors

**Refactoring Opportunity**:
```haskell
-- Instead of one large encodeTermMultiline:
encodeTermMultiline :: PythonEnvironment -> Term -> Flow PyGraph [Py.Statement]

-- Split into:
encodeTermAsStatements :: PythonEnvironment -> Term -> Flow PyGraph [Py.Statement]
encodeCaseStatement :: PythonEnvironment -> Term -> EliminationUnion -> Flow PyGraph [Py.Statement]
encodeLetBindings :: PythonEnvironment -> [Binding] -> Term -> Flow PyGraph [Py.Statement]
```

#### 2. **Scattered Type Encoding Logic**

Type encoding appears in multiple places:
- `encodeType` (lines 667-702): Main dispatcher
- `encodeLiteralType` (lines 322-337)
- `encodeFunctionType` (lines 286-298)
- `encodeApplicationType` (lines 136-145)
- `encodeForallType` (lines 198-206)

**Refactoring Opportunity**: Centralize in a type encoding module

#### 3. **Hard-Coded Python Specifics**

Many Python-specific workarounds are embedded in general logic:

```haskell
-- Line 62-74: Deduplicate case variables (Python-specific issue)
deduplicateCaseVariables :: [Field] -> [Field]

-- Lines 217-242: Walrus operator for lambda with bindings
-- Lines 624-665: Match statement special case
```

**Abstraction Opportunity**: Separate language-specific transforms from core logic

#### 4. **Inline Special Cases**

Throughout the coder:
```haskell
-- Line 96-102: Special needsCast check for maps
needsCast term typ = case typ of
  TypeMap _ -> case deannotateAndDetypeTerm term of
    TermMap _ -> False
    t -> t == TermFunction (FunctionPrimitive _maps_fromList)

-- Lines 654-664: Special isEnum handling in case statements
```

**Issue**: Mixing concerns makes it hard to find/modify specific behaviors

---

## Reusable Patterns for Other Coders

### Pattern 1: Metadata Accumulation

**What**: Track feature usage while encoding

**Python Example** (lines 835-861, 863-923):
```haskell
extendMetaForTerm :: Bool -> PythonModuleMetadata -> Term -> PythonModuleMetadata
extendMetaForType :: Bool -> Bool -> Type -> PythonModuleMetadata -> PythonModuleMetadata

-- Usage in encoding:
updateMeta $ \m -> m { pythonModuleMetadataUsesCast = True }
```

**Generalized**:
```haskell
-- In hydra-ext/src/main/haskell/Hydra/Ext/Staging/Common/
data FeatureTracker lang = FeatureTracker {
  trackerRequiredImports :: Set (Import lang),
  trackerLanguageFeatures :: Set (Feature lang),
  trackerTypeVariables :: Set Name
}

trackTerm :: Term -> FeatureTracker lang -> FeatureTracker lang
trackType :: Type -> FeatureTracker lang -> FeatureTracker lang
```

**Benefits**:
- Automatic import generation
- Feature compatibility checking
- Consistent across coders

### Pattern 2: Term Decomposition

**What**: Recursively peel structure from terms

**Python Example** (lines 944-966):
```haskell
gatherBindingsAndParams :: PythonEnvironment -> Term
  -> Flow s ([Name], [Name], [Binding], Term, [Type], Type, PythonEnvironment)
gatherBindingsAndParams env term = gather True env [] [] [] [] [] term
  where
    gather argMode env tparams args bindings doms tapps term = case deannotateTerm term of
      TermFunction (FunctionLambda lam@(Lambda var (Just dom) body)) ->
        gather argMode env2 tparams (var:args) bindings (dom:doms) tapps body
      TermLet lt@(Let bindings2 body) ->
        gather False env2 tparams args (bindings ++ bindings2) doms tapps body
      -- ...
```

**Generalized**:
```haskell
-- In Hydra.Adapt.Terms or similar
data TermStructure = TermStructure {
  structureTypeParams :: [Name],
  structureParams :: [Name],
  structureBindings :: [Binding],
  structureBody :: Term,
  structureDomains :: [Type],
  structureCodomain :: Type
}

decomposeFunction :: TypeContext -> Term -> Flow Graph TermStructure
```

**Benefits**:
- Consistent handling across languages
- Single source of truth for term structure
- Easier to test

### Pattern 3: Dual Encoding Modes

**What**: Support both inline expressions and statement blocks

**Python Example**:
```haskell
encodeTermInline :: PythonEnvironment -> Bool -> Term -> Flow PyGraph Py.Expression
encodeTermMultiline :: PythonEnvironment -> Term -> Flow PyGraph [Py.Statement]
```

**Generalized Interface**:
```haskell
class Coder lang where
  type Expr lang
  type Stmt lang

  encodeAsExpr :: Environment lang -> Term -> Flow Graph (Expr lang)
  encodeAsStmt :: Environment lang -> Term -> Flow Graph [Stmt lang]

  -- Smart decision based on term structure
  encodeAuto :: Environment lang -> Term -> Flow Graph (Either (Expr lang) [Stmt lang])
```

**Benefits**:
- Handle expression-based (Scala) vs statement-based (Java) languages
- Clearer API for downstream code

### Pattern 4: Name Encoding Strategy

**What**: Systematic handling of name conventions and sanitization

**Python Example** (Names.hs:38-62):
```haskell
encodeName :: Bool -> CaseConvention -> PythonEnvironment -> Name -> Py.Name
encodeFieldName :: PythonEnvironment -> Name -> Py.Name
encodeEnumValue :: PythonEnvironment -> Name -> Py.Name
encodeConstantForTypeName :: PythonEnvironment -> Name -> Py.Name
```

**Generalized**:
```haskell
-- In Hydra.Ext.Staging.Common.Names
data NameContext = NameContext {
  nameContextFocus :: Namespace,
  nameContextReservedWords :: Set String,
  nameContextBoundTypeVars :: Map Name String
}

data NameRole = TypeName | TermName | FieldName | EnumValue | Constant

encodeName :: CaseConvention -> NameRole -> NameContext -> Name -> String
```

**Benefits**:
- Portable across languages
- Centralized reserved word handling
- Consistent qualified name logic

### Pattern 5: Environment Extension

**What**: Thread typing context through encoding

**Python Example** (lines 823-833):
```haskell
extendEnvironmentForLambda :: PythonEnvironment -> Lambda -> PythonEnvironment
extendEnvironmentForLet :: PythonEnvironment -> Let -> PythonEnvironment
extendEnvironmentForTypeLambda :: PythonEnvironment -> TypeLambda -> PythonEnvironment
```

**Generalized**:
```haskell
class HasTypeContext env where
  getTypeContext :: env -> TypeContext
  extendTypeContext :: TypeContext -> env -> env

withLambda :: HasTypeContext env => Lambda -> Flow env a -> Flow env a
withLambda lam flow = do
  env <- getState
  let tc' = extendTypeContextForLambda (getTypeContext env) lam
  withState (extendTypeContext tc' env) flow
```

**Benefits**:
- Consistent scoping across coders
- Easier to reason about variable binding
- Reusable with Reader monad pattern

---

## Refactoring Recommendations

### Priority 1: Extract Common Metadata Pattern

**Create**: `Hydra.Ext.Staging.Common.Metadata`

```haskell
module Hydra.Ext.Staging.Common.Metadata where

-- Generic feature tracker
data FeatureSet lang = FeatureSet {
  featureSetImports :: Set (Import lang),
  featureSetTypeVars :: Set Name
}

class HasFeatures lang where
  featuresForTerm :: Term -> FeatureSet lang
  featuresForType :: Type -> FeatureSet lang
  combineFeatures :: FeatureSet lang -> FeatureSet lang -> FeatureSet lang

-- Fold over definitions to collect features
gatherFeatures :: HasFeatures lang => [Definition] -> FeatureSet lang
```

**Apply to**: Python, Java, Scala coders

### Priority 2: Centralize Term Decomposition

**Create**: `Hydra.Adapt.TermStructure`

```haskell
-- Move gatherBindingsAndParams logic here
-- Make it language-agnostic
-- Return structured data instead of tuples

data FunctionStructure = FunctionStructure {
  functionTypeParams :: [Name],
  functionParams :: [(Name, Type)],
  functionBindings :: [Binding],
  functionBody :: Term,
  functionType :: Type
}

analyzeFunctionTerm :: TypeContext -> Term -> Flow Graph FunctionStructure
```

**Benefits**:
- Single source of truth
- Easier to test in isolation
- Reusable across all coders

### Priority 3: Separate Language-Specific Transforms

**Create**: `Hydra.Ext.Staging.Python.Transforms`

```haskell
-- Move Python-specific workarounds:
-- - deduplicateCaseVariables
-- - walrus operator logic
-- - match statement optimization

transformForPython :: Term -> Term
transformCaseStatement :: CaseStatement -> CaseStatement
```

**Apply to**: Keep Coder.hs focused on encoding, not transformation

### Priority 4: Modularize Type Encoding

**Split**: Create `Hydra.Ext.Staging.Python.TypeEncoder`

```haskell
module Hydra.Ext.Staging.Python.TypeEncoder where

-- Move all type encoding:
encodeType :: PythonEnvironment -> Type -> Flow PyGraph Py.Expression
encodeLiteralType :: LiteralType -> Flow PyGraph Py.Expression
encodeFunctionType :: PythonEnvironment -> FunctionType -> Flow PyGraph Py.Expression
-- ...

-- Keep pure encoding separate from metadata updates
```

**Benefits**:
- Clearer separation of concerns
- Easier to modify type encoding rules
- Potential for unit testing

---

## Common Utility Functions to Extract

### 1. AST Construction Helpers

**Current State**: Python coder has extensive helpers in `Utils.hs`

**Observation**: Many are Python-specific, but pattern is reusable

**Extract**:
```haskell
-- In Hydra.Ext.Staging.Common.AstUtils
class HasFunctionCall lang where
  functionCall :: Name lang -> [Expr lang] -> Expr lang

class HasAssignment lang where
  assignment :: Name lang -> Expr lang -> Stmt lang

class HasBlock lang where
  block :: Maybe Comment -> [[Stmt lang]] -> Block lang
```

### 2. Import Management

**Current**: Hardcoded in `encodeModule` (lines 400-443)

**Extract**:
```haskell
-- In Hydra.Ext.Staging.Common.Imports
data Import lang = Import {
  importModule :: ModuleName,
  importSymbols :: [Symbol]
}

generateImports :: FeatureSet lang -> [Import lang]
```

### 3. Name Sanitization

**Current**: `sanitizePythonName` in Names.hs

**Extract**:
```haskell
-- In Hydra.Ext.Staging.Common.Names
sanitizeName :: Set String -> String -> String
sanitizeWithUnderscores :: Set String -> String -> String
sanitizeWithSuffix :: Set String -> String -> String
```

---

## Architectural Comparison: Java vs Python Coders

This section analyzes the fundamental architectural differences between the Java and Python coders, with an eye toward maximizing code sharing.

### File Size Comparison

| Component | Python | Java |
|-----------|--------|------|
| Coder.hs | 1073 lines | 1049 lines |
| Names.hs | 101 lines | 40 lines |
| Utils.hs | 235 lines | 557 lines |
| Language.hs | ~234 lines | 234 lines |

**Key Observation**: Similar total size, but different distribution. Java has much more extensive AST utilities.

### State Management: Fundamental Difference

#### Python: Metadata Accumulation Pattern
```haskell
-- Python/Coder.hs:28-49
data PythonModuleMetadata = PythonModuleMetadata {
  pythonModuleMetadataUsesCallable :: Bool,
  pythonModuleMetadataUsesCast :: Bool,
  pythonModuleMetadataUsesDataclass :: Bool,
  pythonModuleMetadataUsesDecimal :: Bool,
  pythonModuleMetadataUsesDict :: Bool,
  pythonModuleMetadataUsesEnum :: Bool,
  pythonModuleMetadataUsesLambda :: Bool,
  pythonModuleMetadataUsesOrOperator :: Bool,
  pythonModuleMetadataUsesPair :: Bool,
  pythonModuleMetadataUsesProductClass :: Bool,
  pythonModuleMetadataUsesSet :: Bool,
  pythonModuleMetadataUsesTypeAlias :: Bool,
  pythonModuleMetadataUsesTypeVar :: Bool,
  pythonModuleMetadataUsesUnion :: Bool,
  pythonModuleMetadataUsesWalrus :: Bool
}

type PyGraph = Graph PythonModuleMetadata
```

**Philosophy**: Track every import-worthy feature as you encode. Automatically generate imports at the end.

#### Java: Aliases for Namespace Resolution
```haskell
-- Java/Utils.hs:38-42
data Aliases = Aliases {
  aliasesCurrentNamespace :: Namespace,
  aliasesPackages :: PackageMap,
  aliasesRecursiveVars :: S.Set Name
}
```

**Philosophy**: Track where things live in package structure. No automatic import generation.

**Incompatibility**: These serve completely different purposes:
- Python metadata → import generation
- Java aliases → name qualification

**Implication for sharing**: Can't easily unify these. Different languages have different needs.

### Module Encoding: Two Different Paradigms

#### Python: Custom Graph State
```haskell
-- Python/Coder.hs:339-371
encodeModule :: PythonEnvironment -> Module -> [Definition] -> Flow PyGraph Py.Module
encodeModule env mod defs = do
  -- Custom logic with PyGraph state
  stmts <- concat <$> mapM (encodeDefinition env) defsInDependencyOrder
  meta <- getMeta
  let imports = metadataToImports meta
  return $ Py.Module name imports stmts
```

**Features**:
- Custom metadata in Flow PyGraph
- Automatic import generation
- Direct control over entire process

#### Java: Two Approaches Coexist

**Approach 1: Direct (Current)**
```haskell
-- Java/Coder.hs:61-68
moduleToJava :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToJava mod defs = do
  units <- defsToJavaCompilationUnits mod defs
  return $ M.fromList $ forPair <$> M.toList units
  where
    forPair (name, unit) = (namespace ++ "/" ++ name ++ ".java",
                            Printer.printCompilationUnit unit)
```

**Approach 2: transformModule (Deprecated)**
```haskell
-- Java/Coder.hs:1011-1013
moduleToJavaDeprecated :: Module -> Flow Graph (M.Map FilePath String)
moduleToJavaDeprecated mod =
  transformModule javaLanguage encode constructModule mod
```

**Key Insight**: Java is in transition. The `transformModule` abstraction exists but is deprecated in favor of direct encoding.

**Implication for sharing**: The `transformModule` pattern could be the common ground, but Java moved away from it.

### Name Encoding Philosophy

#### Python: Feature-Rich Environment
```haskell
-- Python/Names.hs:19-22
data PythonEnvironment = PythonEnvironment {
  pythonEnvironmentNamespaces :: Namespaces Py.DottedName,
  pythonEnvironmentBoundTypeVariables :: ([Name], M.Map Name Py.Name),
  pythonEnvironmentTypeContext :: TypeContext
}
```

- Tracks namespaces for imports
- Manages type variable bindings explicitly
- Maintains full type context

#### Java: Simpler Context
```haskell
-- Java coder typically uses just TypeContext + Aliases
-- No elaborate environment type
```

- Relies on Aliases passed separately
- Type context managed in Flow Graph
- Less centralized state

**Implication for sharing**: Python's richer environment is needed for its sophisticated features. Java's simpler approach might not need it.

### Term Decomposition: Similar but Different

#### Python: gatherBindingsAndParams
```haskell
-- Python/Coder.hs:944-966
gatherBindingsAndParams :: PythonEnvironment -> Term
  -> Flow s ([Name], [Name], [Binding], Term, [Type], Type, PythonEnvironment)
```

**Returns**: Type params, value params, bindings, body, domains, codomain, updated environment

**Features**:
- Handles type lambdas
- Accumulates bindings from lets
- Updates environment during traversal
- Returns 7-tuple (complex!)

#### Java: Similar Patterns, Less Formalized
```haskell
-- Java/Coder.hs has similar logic scattered throughout
-- No single unified function like gatherBindingsAndParams
-- Each encoding function handles its own decomposition
```

**Implication for sharing**: This is a PRIME candidate for extraction! Both need it, Python has it formalized.

### Type Encoding: Different Strategies

#### Python: Metadata-Driven
```haskell
encodeType :: PythonEnvironment -> Type -> Flow PyGraph Py.Expression
```

- Updates metadata as it encodes types
- Tracks which type constructs are used
- Returns both the encoded type AND updated metadata state

#### Java: Pure Translation
```haskell
-- Java type encoding (in various places)
-- Generally Flow Graph, not Flow (Graph Metadata)
-- Doesn't track feature usage
```

**Implication for sharing**: Python's approach is more sophisticated. Java might benefit from adopting it for automatic imports.

---

## Impact on Refactoring Recommendations

### Original Recommendations Reconsidered

#### Priority 1: Metadata Pattern - **ADJUSTED**

**Original Recommendation**: Extract metadata pattern for all coders

**After Java Analysis**:
- Java doesn't currently use metadata accumulation
- Java's simpler model works for Java (no automatic imports needed)
- BUT: Java *could* benefit from this for automatic import generation

**Revised Recommendation**:
1. Extract metadata pattern as **optional** enhancement
2. Keep Python's implementation as reference
3. Offer to Java/other coders as opt-in feature
4. Don't force uniform approach

```haskell
-- Generalize as:
class HasImportTracking lang where
  type Metadata lang
  emptyMetadata :: Metadata lang
  combineMetadata :: Metadata lang -> Metadata lang -> Metadata lang
  metadataToImports :: Metadata lang -> [Import lang]

-- Python implements this:
instance HasImportTracking PythonLanguage where
  type Metadata PythonLanguage = PythonModuleMetadata
  -- ...

-- Java can choose to implement or not:
instance HasImportTracking JavaLanguage where
  type Metadata JavaLanguage = ()  -- No-op if not needed
  -- ...
```

#### Priority 2: Term Decomposition - **STRENGTHENED**

**Original Recommendation**: Centralize term decomposition

**After Java Analysis**:
- Java lacks this formalization
- Both coders need it
- This is THE prime sharing opportunity
- Java would immediately benefit

**Revised Recommendation**: **HIGHEST PRIORITY**
1. Extract `gatherBindingsAndParams` to `Hydra.Adapt.Terms`
2. Make it return a structured type, not a 7-tuple
3. Use in both Python and Java
4. This enables consistent handling across all coders

```haskell
-- In Hydra.Adapt.Terms
data FunctionStructure = FunctionStructure {
  functionTypeParams :: [Name],
  functionValueParams :: [Name],
  functionBindings :: [Binding],
  functionBody :: Term,
  functionDomains :: [Type],
  functionCodomain :: Type
}

analyzeFunctionTerm :: TypeContext
                    -> Term
                    -> Flow Graph (FunctionStructure, TypeContext)
-- This can work for ANY language coder!
```

**Benefit for Java**: Would immediately simplify several complex functions in Java/Coder.hs

**Benefit for Python**: Clearer API, better tested, more maintainable

#### Priority 3: Dual Encoding Modes - **EXPANDED**

**Original Recommendation**: Support inline/multiline encoding

**After Java Analysis**:
- Java has similar needs (expressions vs statements)
- Pattern is language-agnostic
- Both would benefit from common interface

**Revised Recommendation**: **HIGH PRIORITY**

Create common abstraction:
```haskell
-- In Hydra.Ext.Staging.Common.Encoding
class DualModeCoder lang where
  type Expr lang
  type Stmt lang
  type Env lang

  encodeAsExpr :: Env lang -> Term -> Flow Graph (Expr lang)
  encodeAsStmts :: Env lang -> Term -> Flow Graph [Stmt lang]

  -- Default implementation: try expr first, fall back to stmts
  encodeAuto :: Env lang -> Term -> Flow Graph (Either (Expr lang) [Stmt lang])
  encodeAuto env term =
    case canBeExpression term of
      True -> Left <$> encodeAsExpr env term
      False -> Right <$> encodeAsStmts env term
```

Both Python and Java implement this interface differently, but share the pattern.

#### Priority 4: Name Encoding - **CONFIRMED**

**Original Recommendation**: Extract name encoding utilities

**After Java Analysis**:
- Both coders sanitize names against reserved words
- Both handle case conventions
- Both qualify names from other namespaces
- Implementation details differ, but pattern is identical

**Revised Recommendation**: **MEDIUM-HIGH PRIORITY**

Shared utilities confirmed as valuable:
```haskell
-- In Hydra.Ext.Staging.Common.Names
sanitizeName :: Set String -> String -> String
applyCase :: CaseConvention -> String -> String
qualifyName :: Namespace -> Namespace -> Name -> String
```

Both coders use these, just with different reserved word sets and conventions.

#### Priority 5: Environment Extension - **RECONSIDERED**

**Original Recommendation**: Generalize environment extension pattern

**After Java Analysis**:
- Python has rich PythonEnvironment
- Java uses simpler TypeContext + Aliases
- Forcing uniformity might not help

**Revised Recommendation**: **LOW PRIORITY**

Don't force common environment type. Instead, extract the **pattern**:
```haskell
-- In Hydra.Ext.Staging.Common.Environment
class HasTypeContext env where
  getTypeContext :: env -> TypeContext
  setTypeContext :: TypeContext -> env -> env

-- Pattern for local scoping:
withExtendedContext :: HasTypeContext env
                    => (TypeContext -> TypeContext)
                    -> Flow env a
                    -> Flow env a
withExtendedContext f action = do
  env <- getState
  let tc = getTypeContext env
  withState (setTypeContext (f tc) env) action
```

Python and Java both implement the class, but keep their own env types.

---

## New Shared Abstraction Layer

### Proposed Architecture for Maximum Sharing

```
Hydra/Ext/Staging/
├── Common/
│   ├── Coder.hs              -- Type class for all coders
│   ├── Terms.hs               -- Term decomposition (analyzeFunctionTerm, etc.)
│   ├── Names.hs               -- Name sanitization, case conversion
│   ├── Environment.hs         -- HasTypeContext type class
│   ├── Encoding.hs            -- DualModeCoder type class
│   └── ImportTracking.hs      -- Optional metadata pattern (type class)
├── Python/
│   ├── Coder.hs               -- Implements Common.Coder type class
│   ├── Environment.hs         -- PythonEnvironment (implements HasTypeContext)
│   ├── Metadata.hs            -- PythonModuleMetadata (implements HasImportTracking)
│   ├── TermEncoder.hs         -- Implements DualModeCoder
│   ├── TypeEncoder.hs         -- Type encoding
│   ├── Transforms.hs          -- Python-specific term rewrites
│   └── Utils.hs               -- Python AST helpers
└── Java/
    ├── Coder.hs               -- Implements Common.Coder type class
    ├── Environment.hs         -- JavaEnvironment (implements HasTypeContext)
    ├── TermEncoder.hs         -- Implements DualModeCoder
    ├── TypeEncoder.hs         -- Type encoding
    └── Utils.hs               -- Java AST helpers (extensive!)
```

### What Gets Shared

| Component | Sharing Strategy | Priority |
|-----------|------------------|----------|
| Term decomposition | Direct code sharing | **HIGHEST** |
| Name sanitization | Direct code sharing | **HIGH** |
| Dual-mode encoding | Type class interface | **HIGH** |
| Import tracking | Optional type class | **MEDIUM** |
| Environment | Type class only | **LOW** |
| Type encoding | No sharing (too different) | N/A |
| AST utilities | No sharing (language-specific) | N/A |

### The Unification Strategy

**Phase 1: Extract Universal Patterns**
1. Extract `analyzeFunctionTerm` from Python → `Common.Terms`
2. Extract name utilities (sanitize, case conversion) → `Common.Names`
3. Test with Python coder first

**Phase 2: Adopt in Java**
4. Refactor Java coder to use `analyzeFunctionTerm`
5. Use shared name utilities in Java
6. Measure code reduction

**Phase 3: Define Interfaces**
7. Create `DualModeCoder` type class based on learnings
8. Create `HasTypeContext` type class
9. Have both implement these

**Phase 4: Optional Enhancements**
10. Offer `HasImportTracking` to Java (might help with auto-imports)
11. Consider for other coders (Scala, C++, etc.)

### The transformModule Question

**Current State**:
- Scala uses `transformModule` (active)
- Java has deprecated `transformModule` in favor of direct approach
- Python never used `transformModule`

**Question**: Should we standardize on `transformModule` or the direct approach?

**Analysis**:
- `transformModule` is more declarative and uniform
- Direct approach gives more control and flexibility
- Python's sophistication (metadata tracking) needs direct approach
- Java moved away from `transformModule` for good reasons

**Recommendation**: **Don't force transformModule**
- It's a valid pattern for simpler coders (Scala)
- More complex coders (Python, Java) benefit from direct control
- Both approaches should be supported

### Language Comparison Summary

| Aspect | Python | Java | Sharing Potential |
|--------|--------|------|-------------------|
| Module encoding | Custom PyGraph | Direct w/ Aliases | LOW - different needs |
| Metadata tracking | 15 boolean flags | None | MEDIUM - Java could adopt |
| Term decomposition | gatherBindingsAndParams | Ad-hoc | **HIGH - extract to common** |
| Name encoding | Sophisticated | Simpler | **HIGH - utilities shareable** |
| Environment | Rich PythonEnvironment | TypeContext + Aliases | LOW - type class only |
| Type encoding | Metadata-aware | Direct | LOW - too different |
| AST utilities | 235 lines | 557 lines | NONE - language-specific |
| Dual modes | Inline/multiline | Expression/statement | **HIGH - pattern shareable** |

---

## Proposed New Shared Architecture

### Directory Structure
```
Hydra/Ext/Staging/
├── Common/
│   ├── Metadata.hs      -- Feature tracking
│   ├── TermStructure.hs -- Term decomposition
│   ├── Names.hs         -- Name encoding
│   ├── Imports.hs       -- Import management
│   └── Environment.hs   -- Environment extension
└── Python/
    ├── Coder.hs         -- Main logic (slimmer)
    ├── Transforms.hs    -- Python-specific transforms
    ├── TypeEncoder.hs   -- Type encoding
    ├── TermEncoder.hs   -- Term encoding
    └── Utils.hs         -- Python AST helpers
```

### Shared Type Classes

```haskell
-- In Hydra.Ext.Staging.Common.Coder
class LanguageCoder lang where
  type Environment lang
  type Metadata lang
  type Expression lang
  type Statement lang
  type Module lang

  -- Core encoding functions
  encodeType :: Environment lang -> Type -> Flow Graph (Expression lang)
  encodeTerm :: Environment lang -> Term -> Flow Graph (Expression lang)
  encodeDefinition :: Environment lang -> Definition -> Flow Graph [Statement lang]

  -- Metadata management
  gatherMetadata :: [Definition] -> Metadata lang
  metadataToImports :: Metadata lang -> [Import lang]

  -- Module construction
  constructModule :: Module -> [Statement lang] -> Metadata lang -> Module lang
```

---

## Specific Refactoring Steps for Python Coder

### Step 1: Extract Metadata (Low Risk)
1. Create `Common/Metadata.hs`
2. Move `PythonModuleMetadata` definition
3. Move `extendMetaFor*` functions
4. Update imports in `Coder.hs`

**Estimated Impact**: 200 lines moved, no behavior change

### Step 2: Extract Term Decomposition (Medium Risk)
1. Create `Common/TermStructure.hs`
2. Move `gatherBindingsAndParams` with generalization
3. Create unit tests
4. Update callers

**Estimated Impact**: 100 lines moved, better tested

### Step 3: Split Type Encoding (Medium Risk)
1. Create `Python/TypeEncoder.hs`
2. Move all `encode*Type` functions
3. Keep metadata updates in coder
4. Add module export

**Estimated Impact**: 150 lines moved, clearer structure

### Step 4: Extract Python Transforms (Low Risk)
1. Create `Python/Transforms.hs`
2. Move `deduplicateCaseVariables`
3. Move other Python-specific term rewrites
4. Apply in preprocessing step

**Estimated Impact**: 50 lines moved, better separation

### Step 5: Generalize Environment (High Risk)
1. Create `Common/Environment.hs`
2. Extract `extendEnvironmentFor*` pattern
3. Implement with type class
4. Migrate Python coder

**Estimated Impact**: Complex, but high payoff

---

## Testing Strategy

### Current State
- No visible unit tests in coder files
- Relies on integration testing (full kernel generation)

### Proposed
1. **Unit tests for pure functions**:
   - `sanitizePythonName`
   - `isSimpleAssignment`
   - `deduplicateCaseVariables`

2. **Property tests for encoding**:
   - Roundtrip: decode(encode(term)) = term
   - Type preservation
   - Name uniqueness

3. **Integration tests**:
   - Small Hydra modules → Python
   - Verify generated code compiles
   - Check import correctness

---

## Conclusion

### Summary of Findings

**Strengths**:
- Comprehensive feature coverage
- Sophisticated metadata tracking for automatic imports
- Smart encoding decisions (inline vs multiline)
- Handles complex Python constructs (match statements, walrus operator)
- Well-formalized term decomposition (gatherBindingsAndParams)

**Weaknesses**:
- Large, complex functions (60-100+ lines)
- Scattered logic for related concerns (type encoding in multiple places)
- Python-specific hacks mixed with general logic
- Limited code reuse with Java coder despite similar needs
- Returns 7-tuples instead of structured types

**Comparison with Java Coder**:
- Both ~1000 lines but different architectures
- Python uses metadata accumulation; Java uses simpler Aliases
- Python has formalized term decomposition; Java has ad-hoc patterns
- Both need name sanitization, dual-mode encoding
- Java has 2.5x more AST utilities (557 vs 235 lines)

### Revised Recommendations Priority (After Java Analysis)

**HIGHEST PRIORITY** - Immediate Benefits for Both Coders:
1. **Extract term decomposition** (`analyzeFunctionTerm`)
   - Python has it formalized; Java needs it
   - Eliminates 7-tuple returns
   - Single source of truth for both coders
   - Would simplify multiple Java functions immediately

**HIGH PRIORITY** - Clear Sharing Opportunities:
2. **Extract name encoding utilities**
   - Both sanitize against reserved words
   - Both handle case conventions
   - Both qualify cross-namespace names
   - Direct code sharing possible

3. **Define dual-mode encoding interface**
   - Python: inline/multiline
   - Java: expression/statement
   - Same pattern, different implementations
   - Type class captures common structure

**MEDIUM PRIORITY** - Optional/Enhancement:
4. **Offer metadata pattern as type class**
   - Keep Python's implementation
   - Make it opt-in for Java
   - Could enable auto-imports in Java
   - Don't force uniformity

5. **Split Python type encoding**
   - Better organization for Python
   - No immediate Java benefit
   - Separate concern from code sharing

**LOW PRIORITY** - Nice-to-Have:
6. **Environment type class**
   - Python and Java have different needs
   - Extract pattern, not implementation
   - Low-value abstraction

7. **Separate Python transforms**
   - Python-specific cleanup
   - No sharing potential
   - Good practice, not urgent

### Expected Benefits

**For Python Coder**:
- Clearer structure (structured types instead of tuples)
- Easier to understand (smaller, focused functions)
- Better tested (shared code gets more testing)
- Less maintenance burden (bugs fixed in shared code)

**For Java Coder**:
- Immediate simplification from `analyzeFunctionTerm`
- Better name handling from shared utilities
- Potential for auto-imports via metadata pattern
- More consistent API

**For New Coders** (C++, Rust, etc.):
- Ready-made patterns to follow
- `analyzeFunctionTerm` works for any language
- Name utilities easily adapted
- Clear separation of concerns

**For Maintenance**:
- Shared code means shared bug fixes
- Changes benefit all coders simultaneously
- Single source of truth reduces divergence

### Code Sharing Reality Check

**What CAN be shared** (60-70% confidence):
- Term decomposition logic (high value)
- Name sanitization utilities (high value)
- Type class interfaces (medium value)
- Case conversion logic (medium value)

**What PROBABLY CAN'T be shared**:
- Module encoding strategies (too different)
- Type encoding logic (language-specific)
- AST construction utilities (inherently language-specific)
- Environment types (different needs)

**What SHOULDN'T be shared**:
- Language-specific transforms
- Reserved word lists
- Import generation logic (format varies)

### Phased Implementation Plan

**Phase 1: Quick Wins (Low Risk, High Value)**
1. Create `Hydra.Ext.Staging.Common.Names` module
2. Extract `sanitizeName`, `applyCase`, `qualifyName`
3. Use in both Python and Java coders
4. **Estimated effort**: 2-4 hours
5. **Estimated benefit**: ~50 lines of code sharing

**Phase 2: Foundation (Medium Risk, Highest Value)**
1. Create `Hydra.Ext.Staging.Common.Terms` module
2. Extract `gatherBindingsAndParams` → `analyzeFunctionTerm`
3. Return `FunctionStructure` instead of 7-tuple
4. Add unit tests
5. Refactor Python to use it
6. Refactor Java to use it
7. **Estimated effort**: 1-2 days
8. **Estimated benefit**: Clearer API, Java simplification, foundation for all coders

**Phase 3: Interfaces (Low Risk, Medium Value)**
1. Define `DualModeCoder` type class
2. Define `HasTypeContext` type class
3. Implement for Python and Java
4. **Estimated effort**: 4-8 hours
5. **Estimated benefit**: Uniform API, easier to reason about

**Phase 4: Optional Enhancements (Low Risk, Uncertain Value)**
1. Define `HasImportTracking` type class
2. Consider for Java (measure value)
3. Apply to other coders if valuable
4. **Estimated effort**: 4-8 hours
5. **Estimated benefit**: TBD based on adoption

**Phase 5: Python Cleanup (Low Risk, Python-Only Benefit)**
1. Split out `TypeEncoder` module
2. Extract `Transforms` module
3. Reduce function sizes
4. **Estimated effort**: 4-8 hours
5. **Estimated benefit**: Better maintainability for Python

### Success Metrics

**Quantitative**:
- Lines of shared code between Python and Java: Target 100-200 lines
- Reduction in code duplication: Target 10-15%
- Number of coders using shared utilities: Target 3+ (Python, Java, future coders)

**Qualitative**:
- Easier to implement new coders (use shared patterns)
- Clearer separation of concerns
- More consistent APIs across coders
- Better test coverage of core patterns

### Next Steps

1. **Get buy-in**: Discuss this analysis with maintainers
2. **Start small**: Implement Phase 1 (name utilities) as proof of concept
3. **Measure impact**: Track LOC reduction and developer experience
4. **Iterate**: Proceed to Phase 2 only if Phase 1 proves valuable
5. **Document**: Update developer documentation with new patterns
6. **Expand**: Consider Scala, C++, C# coders for additional sharing

### Final Recommendation

**DO**:
- Extract `analyzeFunctionTerm` (highest value, both coders benefit)
- Share name utilities (quick win, low risk)
- Define type class interfaces (good design)
- Keep language-specific details separate

**DON'T**:
- Force uniform architecture where languages differ
- Share AST utilities (inherently language-specific)
- Prematurely abstract (wait for 3rd use case)
- Sacrifice clarity for code sharing

The goal is **pragmatic sharing**: extract what truly benefits multiple coders, while respecting that different languages have different needs.
