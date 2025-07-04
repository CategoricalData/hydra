-- | Utilities for working with Haskell syntax trees

module Hydra.Ext.Haskell.Utils where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Haskell.Ast as Ast
import qualified Hydra.Ext.Haskell.Language as Language
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Qnames as Qnames
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

applicationPattern :: (Ast.Name -> [Ast.Pattern] -> Ast.Pattern)
applicationPattern name args = (Ast.PatternApplication (Ast.ApplicationPattern {
  Ast.applicationPatternName = name,
  Ast.applicationPatternArgs = args}))

elementReference :: (Module.Namespaces Ast.ModuleName -> Core.Name -> Ast.Name)
elementReference namespaces name =  
  let namespacePair = (Module.namespacesFocus namespaces) 
      gname = (fst namespacePair)
      gmod = (Ast.unModuleName (snd namespacePair))
      namespacesMap = (Module.namespacesMapping namespaces)
      qname = (Qnames.qualifyName name)
      local = (Module.qualifiedNameLocal qname)
      escLocal = (sanitizeHaskellName local)
      mns = (Module.qualifiedNameNamespace qname)
  in (Optionals.cases (Module.qualifiedNameNamespace qname) (simpleName local) (\ns -> Optionals.cases (Maps.lookup ns namespacesMap) (simpleName local) (\mn ->  
    let aliasStr = (Ast.unModuleName mn)
    in (Logic.ifElse (Equality.equal ns gname) (simpleName escLocal) (rawName (Strings.cat [
      aliasStr,
      ".",
      (sanitizeHaskellName local)]))))))

hsapp :: (Ast.Expression -> Ast.Expression -> Ast.Expression)
hsapp l r = (Ast.ExpressionApplication (Ast.ApplicationExpression {
  Ast.applicationExpressionFunction = l,
  Ast.applicationExpressionArgument = r}))

hslambda :: (Ast.Name -> Ast.Expression -> Ast.Expression)
hslambda name rhs = (Ast.ExpressionLambda (Ast.LambdaExpression {
  Ast.lambdaExpressionBindings = [
    Ast.PatternName name],
  Ast.lambdaExpressionInner = rhs}))

hslit :: (Ast.Literal -> Ast.Expression)
hslit lit = (Ast.ExpressionLiteral lit)

hsvar :: (String -> Ast.Expression)
hsvar s = (Ast.ExpressionVariable (rawName s))

namespacesForModule :: (Module.Module -> Compute.Flow Graph.Graph (Module.Namespaces Ast.ModuleName))
namespacesForModule mod = (Flows.bind (Schemas.moduleDependencyNamespaces True True True True mod) (\nss ->  
  let ns = (Module.moduleNamespace mod) 
      focusPair = (toPair ns)
      nssAsList = (Sets.toList nss)
      nssPairs = (Lists.map toPair nssAsList)
      emptyState = (Maps.empty, Sets.empty)
      finalState = (Lists.foldl addPair emptyState nssPairs)
      resultMap = (fst finalState)
      toModuleName = (\namespace ->  
              let namespaceStr = (Module.unNamespace namespace) 
                  parts = (Strings.splitOn "." namespaceStr)
                  lastPart = (Lists.last parts)
                  capitalized = (Formatting.capitalize lastPart)
              in (Ast.ModuleName capitalized))
      toPair = (\name -> (name, (toModuleName name)))
      addPair = (\state -> \namePair ->  
              let currentMap = (fst state) 
                  currentSet = (snd state)
                  name = (fst namePair)
                  alias = (snd namePair)
                  aliasStr = (Ast.unModuleName alias)
              in (Logic.ifElse (Sets.member alias currentSet) (addPair state (name, (Ast.ModuleName (Strings.cat2 aliasStr "_")))) (Maps.insert name alias currentMap, (Sets.insert alias currentSet))))
  in (Flows.pure (Module.Namespaces {
    Module.namespacesFocus = focusPair,
    Module.namespacesMapping = resultMap}))))

newtypeAccessorName :: (Core.Name -> String)
newtypeAccessorName name = (Strings.cat2 "un" (Qnames.localNameOf name))

rawName :: (String -> Ast.Name)
rawName n = (Ast.NameNormal (Ast.QualifiedName {
  Ast.qualifiedNameQualifiers = [],
  Ast.qualifiedNameUnqualified = (Ast.NamePart n)}))

recordFieldReference :: (Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Name -> Ast.Name)
recordFieldReference namespaces sname fname =  
  let fnameStr = (Core.unName fname) 
      qname = (Qnames.qualifyName sname)
      ns = (Module.qualifiedNameNamespace qname)
      typeNameStr = (typeNameForRecord sname)
      decapitalized = (Formatting.decapitalize typeNameStr)
      capitalized = (Formatting.capitalize fnameStr)
      nm = (Strings.cat2 decapitalized capitalized)
      qualName = Module.QualifiedName {
              Module.qualifiedNameNamespace = ns,
              Module.qualifiedNameLocal = nm}
      unqualName = (Qnames.unqualifyName qualName)
  in (elementReference namespaces unqualName)

sanitizeHaskellName :: (String -> String)
sanitizeHaskellName = (Formatting.sanitizeWithUnderscores Language.reservedWords)

simpleName :: (String -> Ast.Name)
simpleName arg_ = (rawName (sanitizeHaskellName arg_))

simpleValueBinding :: (Ast.Name -> Ast.Expression -> Maybe Ast.LocalBindings -> Ast.ValueBinding)
simpleValueBinding hname rhs bindings =  
  let pat = (Ast.PatternApplication (Ast.ApplicationPattern {
          Ast.applicationPatternName = hname,
          Ast.applicationPatternArgs = []})) 
      rightHandSide = (Ast.RightHandSide rhs)
  in (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
    Ast.simpleValueBindingPattern = pat,
    Ast.simpleValueBindingRhs = rightHandSide,
    Ast.simpleValueBindingLocalBindings = bindings}))

toTypeApplication :: ([Ast.Type] -> Ast.Type)
toTypeApplication types =  
  let app = (\l -> Logic.ifElse (Equality.gtInt32 (Lists.length l) 1) (Ast.TypeApplication (Ast.ApplicationType {
          Ast.applicationTypeContext = (app (Lists.tail l)),
          Ast.applicationTypeArgument = (Lists.head l)})) (Lists.head l))
  in (app (Lists.reverse types))

typeNameForRecord :: (Core.Name -> String)
typeNameForRecord sname =  
  let snameStr = (Core.unName sname) 
      parts = (Strings.splitOn "." snameStr)
  in (Lists.last parts)

unionFieldReference :: (Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Name -> Ast.Name)
unionFieldReference namespaces sname fname =  
  let fnameStr = (Core.unName fname) 
      qname = (Qnames.qualifyName sname)
      ns = (Module.qualifiedNameNamespace qname)
      typeNameStr = (typeNameForRecord sname)
      capitalizedTypeName = (Formatting.capitalize typeNameStr)
      capitalizedFieldName = (Formatting.capitalize fnameStr)
      nm = (Strings.cat2 capitalizedTypeName capitalizedFieldName)
      qualName = Module.QualifiedName {
              Module.qualifiedNameNamespace = ns,
              Module.qualifiedNameLocal = nm}
      unqualName = (Qnames.unqualifyName qualName)
  in (elementReference namespaces unqualName)

unpackForallType :: (t0 -> Core.Type -> ([Core.Name], Core.Type))
unpackForallType cx t = ((\x -> case x of
  Core.TypeForall v1 ->  
    let v = (Core.forallTypeParameter v1) 
        tbody = (Core.forallTypeBody v1)
        recursiveResult = (unpackForallType cx tbody)
        vars = (fst recursiveResult)
        finalType = (snd recursiveResult)
    in (Lists.cons v vars, finalType)
  _ -> ([], t)) (Strip.stripType t))
