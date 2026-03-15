-- Note: this is an automatically generated file. Do not edit.

-- | Common utilities for language coders, providing shared patterns for term decomposition and analysis.

module Hydra.CoderUtils where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Normalize a comment string for consistent output across coders
normalizeComment :: (String -> String)
normalizeComment s =  
  let stripped = (Formatting.stripLeadingAndTrailingWhitespace s)
  in (Logic.ifElse (Strings.null stripped) "" ( 
    let lastIdx = (Math.sub (Strings.length stripped) 1)
    in  
      let lastChar = (Strings.charAt lastIdx stripped)
      in (Logic.ifElse (Equality.equal lastChar 46) stripped (Strings.cat2 stripped "."))))

-- | Gather applications from a term, returning (args, baseTerm)
gatherApplications :: (Core.Term -> ([Core.Term], Core.Term))
gatherApplications term =  
  let go = (\args -> \t -> (\x -> case x of
          Core.TermApplication v0 ->  
            let lhs = (Core.applicationFunction v0)
            in  
              let rhs = (Core.applicationArgument v0)
              in (go (Lists.cons rhs args) lhs)
          _ -> (args, t)) (Rewriting.deannotateTerm t))
  in (go [] term)

-- | Gather term arguments, stripping type-level constructs
gatherArgs :: (Core.Term -> [Core.Term] -> (Core.Term, [Core.Term]))
gatherArgs term args = ((\x -> case x of
  Core.TermApplication v0 ->  
    let lhs = (Core.applicationFunction v0)
    in  
      let rhs = (Core.applicationArgument v0)
      in (gatherArgs lhs (Lists.cons rhs args))
  Core.TermTypeLambda v0 ->  
    let body = (Core.typeLambdaBody v0)
    in (gatherArgs body args)
  Core.TermTypeApplication v0 ->  
    let body = (Core.typeApplicationTermBody v0)
    in (gatherArgs body args)
  _ -> (term, args)) (Rewriting.deannotateTerm term))

-- | Gather term and type arguments from a term
gatherArgsWithTypeApps :: (Core.Term -> [Core.Term] -> [Core.Type] -> (Core.Term, ([Core.Term], [Core.Type])))
gatherArgsWithTypeApps term args tyArgs = ((\x -> case x of
  Core.TermApplication v0 ->  
    let lhs = (Core.applicationFunction v0)
    in  
      let rhs = (Core.applicationArgument v0)
      in (gatherArgsWithTypeApps lhs (Lists.cons rhs args) tyArgs)
  Core.TermTypeLambda v0 ->  
    let body = (Core.typeLambdaBody v0)
    in (gatherArgsWithTypeApps body args tyArgs)
  Core.TermTypeApplication v0 ->  
    let body = (Core.typeApplicationTermBody v0)
    in  
      let typ = (Core.typeApplicationTermType v0)
      in (gatherArgsWithTypeApps body args (Lists.cons typ tyArgs))
  _ -> (term, (args, tyArgs))) (Rewriting.deannotateTerm term))

-- | Check if a term can be encoded as a simple assignment
isSimpleAssignment :: (Core.Term -> Bool)
isSimpleAssignment term = ((\x -> case x of
  Core.TermAnnotated v0 -> (isSimpleAssignment (Core.annotatedTermBody v0))
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionLambda _ -> False
    _ -> True) v0)
  Core.TermLet _ -> False
  Core.TermTypeLambda _ -> False
  Core.TermTypeApplication v0 -> (isSimpleAssignment (Core.typeApplicationTermBody v0))
  _ ->  
    let baseTerm = (Pairs.first (gatherArgs term []))
    in ((\x -> case x of
      Core.TermFunction v0 -> ((\x -> case x of
        Core.FunctionElimination v1 -> ((\x -> case x of
          Core.EliminationUnion _ -> False
          _ -> True) v1)
        _ -> True) v0)
      _ -> True) baseTerm)) term)

-- | Check if a term needs to be treated as a function rather than a simple value
isComplexTerm :: (Graph.Graph -> Core.Term -> Bool)
isComplexTerm tc t = ((\x -> case x of
  Core.TermLet _ -> True
  Core.TermTypeApplication _ -> True
  Core.TermTypeLambda _ -> True
  Core.TermVariable v0 -> (isComplexVariable tc v0)
  _ -> (Lists.foldl (\b -> \sub -> Logic.or b (isComplexTerm tc sub)) False (Rewriting.subterms t))) t)

-- | Check if a variable is bound to a complex term
isComplexVariable :: (Graph.Graph -> Core.Name -> Bool)
isComplexVariable tc name =  
  let metaLookup = (Maps.lookup name (Graph.graphMetadata tc))
  in (Logic.ifElse (Maybes.isJust metaLookup) True (Logic.ifElse (Sets.member name (Graph.graphLambdaVariables tc)) True ( 
    let typeLookup = (Maps.lookup name (Graph.graphBoundTypes tc))
    in (Maybes.maybe True (\ts -> Equality.gt (Arity.typeSchemeArity ts) 0) typeLookup))))

-- | Check if a binding needs to be treated as a function
isComplexBinding :: (Graph.Graph -> Core.Binding -> Bool)
isComplexBinding tc b =  
  let term = (Core.bindingTerm b)
  in  
    let mts = (Core.bindingType b)
    in (Maybes.cases mts (isComplexTerm tc term) (\ts ->  
      let isPolymorphic = (Logic.not (Lists.null (Core.typeSchemeVariables ts)))
      in  
        let isNonNullary = (Equality.gt (Arity.typeArity (Core.typeSchemeType ts)) 0)
        in  
          let isComplex = (isComplexTerm tc term)
          in (Logic.or (Logic.or isPolymorphic isNonNullary) isComplex)))

-- | Check if a term is trivially cheap (no thunking needed)
isTrivialTerm :: (Core.Term -> Bool)
isTrivialTerm t = ((\x -> case x of
  Core.TermLiteral _ -> True
  Core.TermVariable _ -> True
  Core.TermUnit -> True
  Core.TermApplication v0 ->  
    let fun = (Core.applicationFunction v0)
    in  
      let arg = (Core.applicationArgument v0)
      in ((\x -> case x of
        Core.TermFunction v1 -> ((\x -> case x of
          Core.FunctionElimination v2 -> ((\x -> case x of
            Core.EliminationRecord _ -> (isTrivialTerm arg)
            Core.EliminationWrap _ -> (isTrivialTerm arg)
            _ -> False) v2)
          _ -> False) v1)
        _ -> False) fun)
  Core.TermMaybe v0 -> (Maybes.maybe True (\inner -> isTrivialTerm inner) v0)
  Core.TermRecord v0 -> (Lists.foldl (\acc -> \fld -> Logic.and acc (isTrivialTerm (Core.fieldTerm fld))) True (Core.recordFields v0))
  Core.TermWrap v0 -> (isTrivialTerm (Core.wrappedTermBody v0))
  Core.TermTypeApplication v0 -> (isTrivialTerm (Core.typeApplicationTermBody v0))
  Core.TermTypeLambda v0 -> (isTrivialTerm (Core.typeLambdaBody v0))
  _ -> False) (Rewriting.deannotateTerm t))

-- | Check if a term body is self-tail-recursive with respect to a function name
isSelfTailRecursive :: (Core.Name -> Core.Term -> Bool)
isSelfTailRecursive funcName body =  
  let callsSelf = (Logic.not (Rewriting.isFreeVariableInTerm funcName body))
  in (Logic.ifElse callsSelf (isTailRecursiveInTailPosition funcName body) False)

-- | Check that all self-references are in tail position
isTailRecursiveInTailPosition :: (Core.Name -> Core.Term -> Bool)
isTailRecursiveInTailPosition funcName term =  
  let stripped = (Rewriting.deannotateAndDetypeTerm term)
  in ((\x -> case x of
    Core.TermApplication _ ->  
      let gathered = (gatherApplications stripped)
      in  
        let gatherArgs = (Pairs.first gathered)
        in  
          let gatherFun = (Pairs.second gathered)
          in  
            let strippedFun = (Rewriting.deannotateAndDetypeTerm gatherFun)
            in ((\x -> case x of
              Core.TermVariable v1 -> (Logic.ifElse (Equality.equal v1 funcName) ( 
                let argsNoFunc = (Lists.foldl (\ok -> \arg -> Logic.and ok (Rewriting.isFreeVariableInTerm funcName arg)) True gatherArgs)
                in  
                  let argsNoLambda = (Lists.foldl (\ok -> \arg -> Logic.and ok (Logic.not (Rewriting.foldOverTerm Coders.TraversalOrderPre (\found -> \t -> Logic.or found ((\x -> case x of
                          Core.TermFunction v2 -> ((\x -> case x of
                            Core.FunctionLambda v3 ->  
                              let ignore = (Core.lambdaBody v3)
                              in True
                            _ -> False) v2)
                          _ -> False) t)) False arg))) True gatherArgs)
                  in (Logic.and argsNoFunc argsNoLambda)) (Rewriting.isFreeVariableInTerm funcName term))
              Core.TermFunction v1 -> ((\x -> case x of
                Core.FunctionElimination v2 -> ((\x -> case x of
                  Core.EliminationUnion v3 ->  
                    let cases_ = (Core.caseStatementCases v3)
                    in  
                      let dflt = (Core.caseStatementDefault v3)
                      in  
                        let branchesOk = (Lists.foldl (\ok -> \field -> Logic.and ok (isTailRecursiveInTailPosition funcName (Core.fieldTerm field))) True cases_)
                        in  
                          let dfltOk = (Maybes.maybe True (\d -> isTailRecursiveInTailPosition funcName d) dflt)
                          in  
                            let argsOk = (Lists.foldl (\ok -> \arg -> Logic.and ok (Rewriting.isFreeVariableInTerm funcName arg)) True gatherArgs)
                            in (Logic.and (Logic.and branchesOk dfltOk) argsOk)
                  _ -> (Rewriting.isFreeVariableInTerm funcName term)) v2)
                _ -> (Rewriting.isFreeVariableInTerm funcName term)) v1)
              _ -> (Rewriting.isFreeVariableInTerm funcName term)) strippedFun)
    Core.TermFunction v0 -> ((\x -> case x of
      Core.FunctionLambda v1 -> (isTailRecursiveInTailPosition funcName (Core.lambdaBody v1))
      _ -> (Rewriting.isFreeVariableInTerm funcName term)) v0)
    Core.TermLet v0 ->  
      let bindingsOk = (Lists.foldl (\ok -> \b -> Logic.and ok (Rewriting.isFreeVariableInTerm funcName (Core.bindingTerm b))) True (Core.letBindings v0))
      in (Logic.and bindingsOk (isTailRecursiveInTailPosition funcName (Core.letBody v0)))
    _ -> (Rewriting.isFreeVariableInTerm funcName term)) stripped)

-- | Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator
nameToFilePath :: (Util.CaseConvention -> Util.CaseConvention -> Module.FileExtension -> Core.Name -> String)
nameToFilePath nsConv localConv ext name =  
  let qualName = (Names.qualifyName name)
  in  
    let ns = (Module.qualifiedNameNamespace qualName)
    in  
      let local = (Module.qualifiedNameLocal qualName)
      in  
        let nsToFilePath = (\ns -> Strings.intercalate "/" (Lists.map (\part -> Formatting.convertCase Util.CaseConventionCamel nsConv part) (Strings.splitOn "." (Module.unNamespace ns))))
        in  
          let prefix = (Maybes.maybe "" (\n -> Strings.cat2 (nsToFilePath n) "/") ns)
          in  
            let suffix = (Formatting.convertCase Util.CaseConventionPascal localConv local)
            in (Strings.cat [
              prefix,
              suffix,
              ".",
              (Module.unFileExtension ext)])

-- | Convert a union row type to a record row type
unionTypeToRecordType :: ([Core.FieldType] -> [Core.FieldType])
unionTypeToRecordType rt =  
  let makeOptional = (\f ->  
          let fn = (Core.fieldTypeName f)
          in  
            let ft = (Core.fieldTypeType f)
            in Core.FieldType {
              Core.fieldTypeName = fn,
              Core.fieldTypeType = (Rewriting.mapBeneathTypeAnnotations (\x -> Core.TypeMaybe x) ft)})
  in (Lists.map makeOptional rt)

-- | Extract comments/description from a Binding
commentsFromElement :: (Context.Context -> Graph.Graph -> Core.Binding -> Either (Context.InContext Error.Error) (Maybe String))
commentsFromElement cx g b = (Annotations.getTermDescription cx g (Core.bindingTerm b))

-- | Extract comments/description from a FieldType
commentsFromFieldType :: (Context.Context -> Graph.Graph -> Core.FieldType -> Either (Context.InContext Error.Error) (Maybe String))
commentsFromFieldType cx g ft = (Annotations.getTypeDescription cx g (Core.fieldTypeType ft))

-- | Check the type of a term
typeOfTerm :: (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Type)
typeOfTerm cx g term = (Eithers.map Pairs.first (Checking.typeOf cx g [] term))

-- | Produces metadata for a binding if it is complex
bindingMetadata :: (Graph.Graph -> Core.Binding -> Maybe Core.Term)
bindingMetadata tc b = (Logic.ifElse (isComplexBinding tc b) (Just (Core.TermLiteral (Core.LiteralBoolean True))) Nothing)

-- | Analyze a function term, collecting lambdas, type lambdas, lets, and type applications
analyzeFunctionTerm :: (Context.Context -> (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t0) -> t0 -> Core.Term -> Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTerm cx getTC setTC env term = (analyzeFunctionTermWith cx bindingMetadata getTC setTC env term)

-- | Analyze a function term with configurable binding metadata
analyzeFunctionTermWith :: (Context.Context -> (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t0) -> t0 -> Core.Term -> Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTermWith cx forBinding getTC setTC env term = (analyzeFunctionTermWith_gather cx forBinding getTC setTC True env [] [] [] [] [] term)

analyzeFunctionTermWith_finish :: (Context.Context -> (t0 -> Graph.Graph) -> t0 -> [Core.Name] -> [Core.Name] -> [Core.Binding] -> [Core.Type] -> [Core.Type] -> Core.Term -> Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTermWith_finish cx getTC fEnv tparams args bindings doms tapps body =  
  let bodyWithTapps = (Lists.foldl (\trm -> \typ -> Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = trm,
          Core.typeApplicationTermType = typ})) body tapps)
  in  
    let mcod = (Eithers.either (\_ -> Nothing) (\c -> Just c) (typeOfTerm cx (getTC fEnv) bodyWithTapps))
    in (Right (Typing.FunctionStructure {
      Typing.functionStructureTypeParams = (Lists.reverse tparams),
      Typing.functionStructureParams = (Lists.reverse args),
      Typing.functionStructureBindings = bindings,
      Typing.functionStructureBody = bodyWithTapps,
      Typing.functionStructureDomains = (Lists.reverse doms),
      Typing.functionStructureCodomain = mcod,
      Typing.functionStructureEnvironment = fEnv}))

analyzeFunctionTermWith_gather :: (Context.Context -> (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t0) -> Bool -> t0 -> [Core.Name] -> [Core.Name] -> [Core.Binding] -> [Core.Type] -> [Core.Type] -> Core.Term -> Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode gEnv tparams args bindings doms tapps t = ((\x -> case x of
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionLambda v1 -> (Logic.ifElse argMode ( 
      let v = (Core.lambdaParameter v1)
      in  
        let dom = (Maybes.maybe (Core.TypeVariable (Core.Name "_")) (\x_ -> x_) (Core.lambdaDomain v1))
        in  
          let body = (Core.lambdaBody v1)
          in  
            let newEnv = (setTC (Schemas.extendGraphForLambda (getTC gEnv) v1) gEnv)
            in (analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode newEnv tparams (Lists.cons v args) bindings (Lists.cons dom doms) tapps body)) (analyzeFunctionTermWith_finish cx getTC gEnv tparams args bindings doms tapps t))
    _ -> (analyzeFunctionTermWith_finish cx getTC gEnv tparams args bindings doms tapps t)) v0)
  Core.TermLet v0 ->  
    let newBindings = (Core.letBindings v0)
    in  
      let body = (Core.letBody v0)
      in  
        let newEnv = (setTC (Schemas.extendGraphForLet forBinding (getTC gEnv) v0) gEnv)
        in (analyzeFunctionTermWith_gather cx forBinding getTC setTC False newEnv tparams args (Lists.concat2 bindings newBindings) doms tapps body)
  Core.TermTypeApplication v0 ->  
    let taBody = (Core.typeApplicationTermBody v0)
    in  
      let typ = (Core.typeApplicationTermType v0)
      in (analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode gEnv tparams args bindings doms (Lists.cons typ tapps) taBody)
  Core.TermTypeLambda v0 ->  
    let tvar = (Core.typeLambdaParameter v0)
    in  
      let tlBody = (Core.typeLambdaBody v0)
      in  
        let newEnv = (setTC (Schemas.extendGraphForTypeLambda (getTC gEnv) v0) gEnv)
        in (analyzeFunctionTermWith_gather cx forBinding getTC setTC argMode newEnv (Lists.cons tvar tparams) args bindings doms tapps tlBody)
  _ -> (analyzeFunctionTermWith_finish cx getTC gEnv tparams args bindings doms tapps t)) (Rewriting.deannotateTerm t))
