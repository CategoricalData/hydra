-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for working with Haskell syntax trees

module Hydra.Ext.Haskell.Utils where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Haskell.Ast as Ast
import qualified Hydra.Ext.Haskell.Language as Language
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create an application pattern from a name and argument patterns
applicationPattern :: Ast.Name -> [Ast.Pattern] -> Ast.Pattern
applicationPattern name args =
    Ast.PatternApplication (Ast.ApplicationPattern {
      Ast.applicationPatternName = name,
      Ast.applicationPatternArgs = args})

-- | Generate a Haskell name reference for a Hydra element
elementReference :: Module.Namespaces Ast.ModuleName -> Core.Name -> Ast.Name
elementReference namespaces name =
     
      let namespacePair = Module.namespacesFocus namespaces 
          gname = Pairs.first namespacePair
          gmod = Ast.unModuleName (Pairs.second namespacePair)
          namespacesMap = Module.namespacesMapping namespaces
          qname = Names.qualifyName name
          local = Module.qualifiedNameLocal qname
          escLocal = sanitizeHaskellName local
          mns = Module.qualifiedNameNamespace qname
      in (Maybes.cases (Module.qualifiedNameNamespace qname) (simpleName local) (\ns -> Maybes.cases (Maps.lookup ns namespacesMap) (simpleName local) (\mn ->  
        let aliasStr = Ast.unModuleName mn
        in (Logic.ifElse (Equality.equal ns gname) (simpleName escLocal) (rawName (Strings.cat [
          aliasStr,
          ".",
          (sanitizeHaskellName local)]))))))

-- | Create a Haskell function application expression
hsapp :: Ast.Expression -> Ast.Expression -> Ast.Expression
hsapp l r =
    Ast.ExpressionApplication (Ast.ApplicationExpression {
      Ast.applicationExpressionFunction = l,
      Ast.applicationExpressionArgument = r})

-- | Create a Haskell lambda expression
hslambda :: Ast.Name -> Ast.Expression -> Ast.Expression
hslambda name rhs =
    Ast.ExpressionLambda (Ast.LambdaExpression {
      Ast.lambdaExpressionBindings = [
        Ast.PatternName name],
      Ast.lambdaExpressionInner = rhs})

-- | Create a Haskell literal expression
hslit :: Ast.Literal -> Ast.Expression
hslit lit = Ast.ExpressionLiteral lit

-- | Create a Haskell variable expression from a string
hsvar :: String -> Ast.Expression
hsvar s = Ast.ExpressionVariable (rawName s)

-- | Compute the Haskell module namespaces for a Hydra module
namespacesForModule :: Module.Module -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (Module.Namespaces Ast.ModuleName)
namespacesForModule mod cx g =
    Eithers.bind (Schemas.moduleDependencyNamespaces cx g True True True True mod) (\nss ->  
      let ns = Module.moduleNamespace mod 
          toModuleName =
                  \namespace ->  
                    let namespaceStr = Module.unNamespace namespace 
                        parts = Strings.splitOn "." namespaceStr
                        lastPart = Lists.last parts
                        capitalized = Formatting.capitalize lastPart
                    in (Ast.ModuleName capitalized)
          toPair = \name -> (name, (toModuleName name))
          addPair =
                  \state -> \namePair ->  
                    let currentMap = Pairs.first state 
                        currentSet = Pairs.second state
                        name = Pairs.first namePair
                        alias = Pairs.second namePair
                        aliasStr = Ast.unModuleName alias
                    in (Logic.ifElse (Sets.member alias currentSet) (addPair state (name, (Ast.ModuleName (Strings.cat2 aliasStr "_")))) (Maps.insert name alias currentMap, (Sets.insert alias currentSet)))
          focusPair = toPair ns
          nssAsList = Sets.toList nss
          nssPairs = Lists.map toPair nssAsList
          emptyState = (Maps.empty, Sets.empty)
          finalState = Lists.foldl addPair emptyState nssPairs
          resultMap = Pairs.first finalState
      in (Right (Module.Namespaces {
        Module.namespacesFocus = focusPair,
        Module.namespacesMapping = resultMap})))

-- | Generate an accessor name for a newtype wrapper (e.g., 'unFoo' for Foo)
newtypeAccessorName :: Core.Name -> String
newtypeAccessorName name = Strings.cat2 "un" (Names.localNameOf name)

-- | Create a raw Haskell name from a string without sanitization
rawName :: String -> Ast.Name
rawName n =
    Ast.NameNormal (Ast.QualifiedName {
      Ast.qualifiedNameQualifiers = [],
      Ast.qualifiedNameUnqualified = (Ast.NamePart n)})

-- | Generate a Haskell name for a record field accessor
recordFieldReference :: Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Name -> Ast.Name
recordFieldReference namespaces sname fname =
     
      let fnameStr = Core.unName fname 
          qname = Names.qualifyName sname
          ns = Module.qualifiedNameNamespace qname
          typeNameStr = typeNameForRecord sname
          decapitalized = Formatting.decapitalize typeNameStr
          capitalized = Formatting.capitalize fnameStr
          nm = Strings.cat2 decapitalized capitalized
          qualName =
                  Module.QualifiedName {
                    Module.qualifiedNameNamespace = ns,
                    Module.qualifiedNameLocal = nm}
          unqualName = Names.unqualifyName qualName
      in (elementReference namespaces unqualName)

-- | Sanitize a string to be a valid Haskell identifier, escaping reserved words
sanitizeHaskellName :: String -> String
sanitizeHaskellName = Formatting.sanitizeWithUnderscores Language.reservedWords

-- | Create a sanitized Haskell name from a string
simpleName :: String -> Ast.Name
simpleName arg_ = rawName (sanitizeHaskellName arg_)

-- | Create a simple value binding (e.g., 'foo = expr' or 'foo = expr where ...')
simpleValueBinding :: Ast.Name -> Ast.Expression -> Maybe Ast.LocalBindings -> Ast.ValueBinding
simpleValueBinding hname rhs bindings =
     
      let pat =
              Ast.PatternApplication (Ast.ApplicationPattern {
                Ast.applicationPatternName = hname,
                Ast.applicationPatternArgs = []}) 
          rightHandSide = Ast.RightHandSide rhs
      in (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
        Ast.simpleValueBindingPattern = pat,
        Ast.simpleValueBindingRhs = rightHandSide,
        Ast.simpleValueBindingLocalBindings = bindings}))

-- | Convert a list of types into a nested type application
toTypeApplication :: [Ast.Type] -> Ast.Type
toTypeApplication types =
     
      let app =
              \l -> Logic.ifElse (Equality.gt (Lists.length l) 1) (Ast.TypeApplication (Ast.ApplicationType {
                Ast.applicationTypeContext = (app (Lists.tail l)),
                Ast.applicationTypeArgument = (Lists.head l)})) (Lists.head l)
      in (app (Lists.reverse types))

-- | Extract the local type name from a fully qualified record type name
typeNameForRecord :: Core.Name -> String
typeNameForRecord sname =
     
      let snameStr = Core.unName sname 
          parts = Strings.splitOn "." snameStr
      in (Lists.last parts)

-- | Generate a Haskell name for a union variant constructor, with disambiguation
unionFieldReference :: S.Set Core.Name -> Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Name -> Ast.Name
unionFieldReference boundNames namespaces sname fname =
     
      let fnameStr = Core.unName fname 
          qname = Names.qualifyName sname
          ns = Module.qualifiedNameNamespace qname
          typeNameStr = typeNameForRecord sname
          capitalizedTypeName = Formatting.capitalize typeNameStr
          capitalizedFieldName = Formatting.capitalize fnameStr
          deconflict =
                  \name ->  
                    let tname =
                            Names.unqualifyName (Module.QualifiedName {
                              Module.qualifiedNameNamespace = ns,
                              Module.qualifiedNameLocal = name})
                    in (Logic.ifElse (Sets.member tname boundNames) (deconflict (Strings.cat2 name "_")) name)
          nm = deconflict (Strings.cat2 capitalizedTypeName capitalizedFieldName)
          qualName =
                  Module.QualifiedName {
                    Module.qualifiedNameNamespace = ns,
                    Module.qualifiedNameLocal = nm}
          unqualName = Names.unqualifyName qualName
      in (elementReference namespaces unqualName)

-- | Unpack nested forall types into a list of type variables and the inner type
unpackForallType :: Core.Type -> ([Core.Name], Core.Type)
unpackForallType t =
    case (Rewriting.deannotateType t) of
      Core.TypeForall v0 ->  
        let v = Core.forallTypeParameter v0 
            tbody = Core.forallTypeBody v0
            recursiveResult = unpackForallType tbody
            vars = Pairs.first recursiveResult
            finalType = Pairs.second recursiveResult
        in (Lists.cons v vars, finalType)
      _ -> ([], t)
