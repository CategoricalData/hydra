-- Note: this is an automatically generated file. Do not edit.

-- | Functions for encoding Hydra modules as Haskell modules

module Hydra.Ext.Haskell.Coder where

import qualified Hydra.Adapt.Modules as Modules
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Ext.Haskell.Ast as Ast
import qualified Hydra.Ext.Haskell.Language as Language
import qualified Hydra.Ext.Haskell.Serde as Serde
import qualified Hydra.Ext.Haskell.Utils as Utils
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

includeTypeDefinitions :: Bool
includeTypeDefinitions = False

useCoreImport :: Bool
useCoreImport = True

keyHaskellVar :: Core.Name
keyHaskellVar = (Core.Name "haskellVar")

adaptTypeToHaskellAndEncode :: (Module.Namespaces Ast.ModuleName -> Core.Type -> Compute.Flow Graph.Graph Ast.Type)
adaptTypeToHaskellAndEncode namespaces = (Modules.adaptTypeToLanguageAndEncode Language.haskellLanguage (encodeType namespaces))

constantForFieldName :: (Core.Name -> Core.Name -> String)
constantForFieldName tname fname = (Strings.cat [
  "_",
  Names.localNameOf tname,
  "_",
  (Core.unName fname)])

constantForTypeName :: (Core.Name -> String)
constantForTypeName tname = (Strings.cat2 "_" (Names.localNameOf tname))

constructModule :: (Module.Namespaces Ast.ModuleName -> Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph Ast.Module)
constructModule namespaces mod defs =  
  let h = (\namespace -> Module.unNamespace namespace) 
      createDeclarations = (\g -> \def -> (\x -> case x of
              Module.DefinitionType v1 ->  
                let name = (Module.typeDefinitionName v1) 
                    typ = (Module.typeDefinitionType v1)
                in (toTypeDeclarationsFrom namespaces name typ)
              Module.DefinitionTerm v1 -> (Flows.bind (toDataDeclaration namespaces v1) (\d -> Flows.pure [
                d]))) def)
      importName = (\name -> Ast.ModuleName (Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." name))))
      imports = (Lists.concat2 domainImports standardImports)
      domainImports =  
              let toImport = (\pair ->  
                      let namespace = (Pairs.first pair) 
                          alias = (Pairs.second pair)
                          name = (h namespace)
                      in Ast.Import {
                        Ast.importQualified = True,
                        Ast.importModule = (importName name),
                        Ast.importAs = (Just alias),
                        Ast.importSpec = Nothing})
              in (Lists.map toImport (Maps.toList (Module.namespacesMapping namespaces)))
      standardImports =  
              let toImport = (\triple ->  
                      let name = (Pairs.first (Pairs.first triple)) 
                          malias = (Pairs.second (Pairs.first triple))
                          hidden = (Pairs.second triple)
                          spec = (Logic.ifElse (Lists.null hidden) Nothing (Just (Ast.SpecImportHiding (Lists.map (\n -> Ast.ImportExportSpec {
                                  Ast.importExportSpecModifier = Nothing,
                                  Ast.importExportSpecName = (Utils.simpleName n),
                                  Ast.importExportSpecSubspec = Nothing}) hidden))))
                      in Ast.Import {
                        Ast.importQualified = (Maybes.isJust malias),
                        Ast.importModule = (Ast.ModuleName name),
                        Ast.importAs = (Maybes.map (\x -> Ast.ModuleName x) malias),
                        Ast.importSpec = spec})
              in (Lists.map toImport [
                (("Prelude", Nothing), [
                  "Enum",
                  "Ordering",
                  "fail",
                  "map",
                  "pure",
                  "sum"]),
                (("Data.Int", (Just "I")), []),
                (("Data.List", (Just "L")), []),
                (("Data.Map", (Just "M")), []),
                (("Data.Set", (Just "S")), [])])
  in (Flows.bind Monads.getState (\g -> Flows.bind (Flows.mapList (createDeclarations g) defs) (\declLists ->  
    let decls = (Lists.concat declLists) 
        mc = (Module.moduleDescription mod)
    in (Flows.pure (Ast.Module {
      Ast.moduleHead = (Just (Ast.ModuleHead {
        Ast.moduleHeadComments = mc,
        Ast.moduleHeadName = (importName (h (Module.moduleNamespace mod))),
        Ast.moduleHeadExports = []})),
      Ast.moduleImports = imports,
      Ast.moduleDeclarations = decls})))))

encodeFunction :: (Module.Namespaces Ast.ModuleName -> Core.Function -> Compute.Flow Graph.Graph Ast.Expression)
encodeFunction namespaces fun = ((\x -> case x of
  Core.FunctionElimination v1 -> ((\x -> case x of
    Core.EliminationWrap v2 -> (Flows.pure (Ast.ExpressionVariable (Utils.elementReference namespaces (Names.qname (Maybes.fromJust (Names.namespaceOf v2)) (Utils.newtypeAccessorName v2)))))
    Core.EliminationRecord v2 ->  
      let dn = (Core.projectionTypeName v2) 
          fname = (Core.projectionField v2)
      in (Flows.pure (Ast.ExpressionVariable (Utils.recordFieldReference namespaces dn fname)))
    Core.EliminationUnion v2 ->  
      let dn = (Core.caseStatementTypeName v2) 
          def = (Core.caseStatementDefault v2)
          fields = (Core.caseStatementCases v2)
          caseExpr = (Flows.bind (Lexical.withSchemaContext (Schemas.requireUnionType dn)) (\rt ->  
                  let fieldMap = (Maps.fromList (Lists.map toFieldMapEntry (Core.rowTypeFields rt))) 
                      toFieldMapEntry = (\f -> (Core.fieldTypeName f, f))
                  in (Flows.bind (Flows.mapList (toAlt fieldMap) fields) (\ecases -> Flows.bind (Maybes.cases def (Flows.pure []) (\d -> Flows.bind (Flows.map (\x -> Ast.CaseRhs x) (encodeTerm namespaces d)) (\cs ->  
                    let lhs = (Ast.PatternName (Utils.rawName Constants.ignoredVariable)) 
                        alt = Ast.Alternative {
                                Ast.alternativePattern = lhs,
                                Ast.alternativeRhs = cs,
                                Ast.alternativeBinds = Nothing}
                    in (Flows.pure [
                      alt])))) (\dcases -> Flows.pure (Ast.ExpressionCase (Ast.CaseExpression {
                    Ast.caseExpressionCase = (Utils.hsvar "x"),
                    Ast.caseExpressionAlternatives = (Lists.concat2 ecases dcases)})))))))
          toAlt = (\fieldMap -> \field ->  
                  let fn = (Core.fieldName field) 
                      fun_ = (Core.fieldTerm field)
                  in (Annotations.withDepth keyHaskellVar (\depth ->  
                    let v0 = (Strings.cat2 "v" (Literals.showInt32 depth)) 
                        raw = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = fun_,
                                Core.applicationArgument = (Core.TermVariable (Core.Name v0))}))
                        rhsTerm = (Rewriting.simplifyTerm raw)
                        v1 = (Logic.ifElse (Rewriting.isFreeVariableInTerm (Core.Name v0) rhsTerm) Constants.ignoredVariable v0)
                        hname = (Utils.unionFieldReference namespaces dn fn)
                    in (Flows.bind (Maybes.cases (Maps.lookup fn fieldMap) (Flows.fail (Strings.cat [
                      "field ",
                      Literals.showString (Core.unName fn),
                      " not found in ",
                      (Literals.showString (Core.unName dn))])) (\fieldType ->  
                      let ft = (Core.fieldTypeType fieldType) 
                          noArgs = (Flows.pure [])
                          singleArg = (Flows.pure [
                                  Ast.PatternName (Utils.rawName v1)])
                      in ((\x -> case x of
                        Core.TypeUnit -> noArgs
                        _ -> singleArg) (Rewriting.deannotateType ft)))) (\args ->  
                      let lhs = (Utils.applicationPattern hname args)
                      in (Flows.bind (Flows.map (\x -> Ast.CaseRhs x) (encodeTerm namespaces rhsTerm)) (\rhs -> Flows.pure (Ast.Alternative {
                        Ast.alternativePattern = lhs,
                        Ast.alternativeRhs = rhs,
                        Ast.alternativeBinds = Nothing}))))))))
      in (Flows.map (Utils.hslambda (Utils.rawName "x")) caseExpr)) v1)
  Core.FunctionLambda v1 ->  
    let v = (Core.lambdaParameter v1) 
        body = (Core.lambdaBody v1)
    in (Flows.bind (encodeTerm namespaces body) (\hbody -> Flows.pure (Utils.hslambda (Utils.elementReference namespaces v) hbody)))
  Core.FunctionPrimitive v1 -> (Flows.pure (Ast.ExpressionVariable (Utils.elementReference namespaces v1)))) fun)

encodeLiteral :: (Core.Literal -> Compute.Flow t0 Ast.Expression)
encodeLiteral l = ((\x -> case x of
  Core.LiteralBoolean v1 -> (Flows.pure (Utils.hsvar (Logic.ifElse v1 "True" "False")))
  Core.LiteralFloat v1 -> ((\x -> case x of
    Core.FloatValueFloat32 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralFloat v2)))
    Core.FloatValueFloat64 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralDouble v2)))
    Core.FloatValueBigfloat v2 -> (Flows.pure (Utils.hslit (Ast.LiteralDouble (Literals.bigfloatToFloat64 v2))))) v1)
  Core.LiteralInteger v1 -> ((\x -> case x of
    Core.IntegerValueBigint v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger v2)))
    Core.IntegerValueInt8 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger (Literals.int8ToBigint v2))))
    Core.IntegerValueInt16 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger (Literals.int16ToBigint v2))))
    Core.IntegerValueInt32 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInt v2)))
    Core.IntegerValueInt64 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger (Literals.int64ToBigint v2))))
    Core.IntegerValueUint8 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger (Literals.uint8ToBigint v2))))
    Core.IntegerValueUint16 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger (Literals.uint16ToBigint v2))))
    Core.IntegerValueUint32 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger (Literals.uint32ToBigint v2))))
    Core.IntegerValueUint64 v2 -> (Flows.pure (Utils.hslit (Ast.LiteralInteger (Literals.uint64ToBigint v2))))) v1)
  Core.LiteralString v1 -> (Flows.pure (Utils.hslit (Ast.LiteralString v1)))
  _ -> (Flows.fail (Strings.cat2 "literal value " (Core__.literal l)))) l)

encodeTerm :: (Module.Namespaces Ast.ModuleName -> Core.Term -> Compute.Flow Graph.Graph Ast.Expression)
encodeTerm namespaces term =  
  let encode = (encodeTerm namespaces)
  in  
    let nonemptyMap = (\m ->  
            let lhs = (Utils.hsvar "M.fromList") 
                encodePair = (\pair ->  
                        let k = (Pairs.first pair) 
                            v = (Pairs.second pair)
                            hk = (encode k)
                            hv = (encode v)
                        in (Flows.map (\x -> Ast.ExpressionTuple x) (Flows.sequence [
                          hk,
                          hv])))
            in (Flows.bind (Flows.map (\x -> Ast.ExpressionList x) (Flows.mapList encodePair (Maps.toList m))) (\rhs -> Flows.pure (Utils.hsapp lhs rhs))))
    in  
      let nonemptySet = (\s ->  
              let lhs = (Utils.hsvar "S.fromList")
              in (Flows.bind (encodeTerm namespaces (Core.TermList (Sets.toList s))) (\rhs -> Flows.pure (Utils.hsapp lhs rhs))))
      in ((\x -> case x of
        Core.TermApplication v1 ->  
          let fun = (Core.applicationFunction v1) 
              arg = (Core.applicationArgument v1)
          in (Flows.bind (encode fun) (\hfun -> Flows.bind (encode arg) (\harg -> Flows.pure (Utils.hsapp hfun harg))))
        Core.TermEither v1 -> (Eithers.either (\l -> Flows.bind (encode l) (\hl -> Flows.pure (Utils.hsapp (Utils.hsvar "Left") hl))) (\r -> Flows.bind (encode r) (\hr -> Flows.pure (Utils.hsapp (Utils.hsvar "Right") hr))) v1)
        Core.TermFunction v1 -> (encodeFunction namespaces v1)
        Core.TermLet v1 ->  
          let bindings = (Core.letBindings v1) 
              env = (Core.letBody v1)
              encodeBinding = (\binding ->  
                      let name = (Core.bindingName binding) 
                          term_ = (Core.bindingTerm binding)
                          hname = (Utils.simpleName (Core.unName name))
                      in (Flows.bind (encode term_) (\hexpr -> Flows.pure (Ast.LocalBindingValue (Utils.simpleValueBinding hname hexpr Nothing)))))
          in (Flows.bind (Flows.mapList encodeBinding bindings) (\hbindings -> Flows.bind (encode env) (\hinner -> Flows.pure (Ast.ExpressionLet (Ast.LetExpression {
            Ast.letExpressionBindings = hbindings,
            Ast.letExpressionInner = hinner})))))
        Core.TermList v1 -> (Flows.bind (Flows.mapList encode v1) (\helems -> Flows.pure (Ast.ExpressionList helems)))
        Core.TermLiteral v1 -> (encodeLiteral v1)
        Core.TermMap v1 -> (Logic.ifElse (Maps.null v1) (Flows.pure (Utils.hsvar "M.empty")) (nonemptyMap v1))
        Core.TermMaybe v1 -> (Maybes.cases v1 (Flows.pure (Utils.hsvar "Nothing")) (\t -> Flows.bind (encode t) (\ht -> Flows.pure (Utils.hsapp (Utils.hsvar "Just") ht))))
        Core.TermPair v1 -> (Flows.bind (encode (Pairs.first v1)) (\f -> Flows.bind (encode (Pairs.second v1)) (\s -> Flows.pure (Ast.ExpressionTuple [
          f,
          s]))))
        Core.TermRecord v1 ->  
          let sname = (Core.recordTypeName v1) 
              fields = (Core.recordFields v1)
              toFieldUpdate = (\field ->  
                      let fn = (Core.fieldName field) 
                          ft = (Core.fieldTerm field)
                          fieldRef = (Utils.recordFieldReference namespaces sname fn)
                      in (Flows.bind (encode ft) (\hft -> Flows.pure (Ast.FieldUpdate {
                        Ast.fieldUpdateName = fieldRef,
                        Ast.fieldUpdateValue = hft}))))
              typeName = (Utils.elementReference namespaces sname)
          in (Flows.bind (Flows.mapList toFieldUpdate fields) (\updates -> Flows.pure (Ast.ExpressionConstructRecord (Ast.ConstructRecordExpression {
            Ast.constructRecordExpressionName = typeName,
            Ast.constructRecordExpressionFields = updates}))))
        Core.TermSet v1 -> (Logic.ifElse (Sets.null v1) (Flows.pure (Utils.hsvar "S.empty")) (nonemptySet v1))
        Core.TermTypeLambda v1 ->  
          let term1 = (Core.typeLambdaBody v1)
          in (encode term1)
        Core.TermTypeApplication v1 ->  
          let term1 = (Core.typeApplicationTermBody v1)
          in (encode term1)
        Core.TermUnion v1 ->  
          let sname = (Core.injectionTypeName v1) 
              field = (Core.injectionField v1)
              fn = (Core.fieldName field)
              ft = (Core.fieldTerm field)
              lhs = (Ast.ExpressionVariable (Utils.unionFieldReference namespaces sname fn))
              dflt = (Flows.map (Utils.hsapp lhs) (encode ft))
          in (Flows.bind (Schemas.requireUnionField sname fn) (\ftyp -> (\x -> case x of
            Core.TypeUnit -> (Flows.pure lhs)
            _ -> dflt) (Rewriting.deannotateType ftyp)))
        Core.TermUnit -> (Flows.pure (Ast.ExpressionTuple []))
        Core.TermVariable v1 -> (Flows.pure (Ast.ExpressionVariable (Utils.elementReference namespaces v1)))
        Core.TermWrap v1 ->  
          let tname = (Core.wrappedTermTypeName v1) 
              term_ = (Core.wrappedTermBody v1)
              lhs = (Ast.ExpressionVariable (Utils.elementReference namespaces tname))
          in (Flows.bind (encode term_) (\rhs -> Flows.pure (Utils.hsapp lhs rhs)))
        _ -> (Flows.fail (Strings.cat2 "unexpected term: " (Core__.term term)))) (Rewriting.deannotateTerm term))

encodeType :: (Module.Namespaces Ast.ModuleName -> Core.Type -> Compute.Flow t0 Ast.Type)
encodeType namespaces typ =  
  let encode = (encodeType namespaces) 
      ref = (\name -> Flows.pure (Ast.TypeVariable (Utils.elementReference namespaces name)))
      unitTuple = (Ast.TypeTuple [])
  in (Monads.withTrace "encode type" ((\x -> case x of
    Core.TypeApplication v1 ->  
      let lhs = (Core.applicationTypeFunction v1) 
          rhs = (Core.applicationTypeArgument v1)
      in (Flows.bind (encode lhs) (\hlhs -> Flows.bind (encode rhs) (\hrhs -> Flows.pure (Utils.toTypeApplication [
        hlhs,
        hrhs]))))
    Core.TypeEither v1 ->  
      let left = (Core.eitherTypeLeft v1) 
          right = (Core.eitherTypeRight v1)
      in (Flows.map Utils.toTypeApplication (Flows.sequence [
        Flows.pure (Ast.TypeVariable (Utils.rawName "Either")),
        encode left,
        (encode right)]))
    Core.TypeFunction v1 ->  
      let dom = (Core.functionTypeDomain v1) 
          cod = (Core.functionTypeCodomain v1)
      in (Flows.bind (encode dom) (\hdom -> Flows.bind (encode cod) (\hcod -> Flows.pure (Ast.TypeFunction (Ast.FunctionType {
        Ast.functionTypeDomain = hdom,
        Ast.functionTypeCodomain = hcod})))))
    Core.TypeForall v1 ->  
      let v = (Core.forallTypeParameter v1) 
          body = (Core.forallTypeBody v1)
      in (encode body)
    Core.TypeList v1 -> (Flows.bind (encode v1) (\hlt -> Flows.pure (Ast.TypeList hlt)))
    Core.TypeLiteral v1 -> ((\x -> case x of
      Core.LiteralTypeBoolean -> (Flows.pure (Ast.TypeVariable (Utils.rawName "Bool")))
      Core.LiteralTypeFloat v2 -> ((\x -> case x of
        Core.FloatTypeFloat32 -> (Flows.pure (Ast.TypeVariable (Utils.rawName "Float")))
        Core.FloatTypeFloat64 -> (Flows.pure (Ast.TypeVariable (Utils.rawName "Double")))
        Core.FloatTypeBigfloat -> (Flows.pure (Ast.TypeVariable (Utils.rawName "Double")))) v2)
      Core.LiteralTypeInteger v2 -> ((\x -> case x of
        Core.IntegerTypeBigint -> (Flows.pure (Ast.TypeVariable (Utils.rawName "Integer")))
        Core.IntegerTypeInt8 -> (Flows.pure (Ast.TypeVariable (Utils.rawName "I.Int8")))
        Core.IntegerTypeInt16 -> (Flows.pure (Ast.TypeVariable (Utils.rawName "I.Int16")))
        Core.IntegerTypeInt32 -> (Flows.pure (Ast.TypeVariable (Utils.rawName "Int")))
        Core.IntegerTypeInt64 -> (Flows.pure (Ast.TypeVariable (Utils.rawName "I.Int64")))
        _ -> (Flows.fail (Strings.cat2 "unexpected integer type: " (Core__.integerType v2)))) v2)
      Core.LiteralTypeString -> (Flows.pure (Ast.TypeVariable (Utils.rawName "String")))
      _ -> (Flows.fail (Strings.cat2 "unexpected literal type: " (Core__.literalType v1)))) v1)
    Core.TypeMap v1 ->  
      let kt = (Core.mapTypeKeys v1) 
          vt = (Core.mapTypeValues v1)
      in (Flows.map Utils.toTypeApplication (Flows.sequence [
        Flows.pure (Ast.TypeVariable (Utils.rawName "M.Map")),
        encode kt,
        (encode vt)]))
    Core.TypeMaybe v1 -> (Flows.map Utils.toTypeApplication (Flows.sequence [
      Flows.pure (Ast.TypeVariable (Utils.rawName "Maybe")),
      (encode v1)]))
    Core.TypePair v1 -> (Flows.bind (encode (Core.pairTypeFirst v1)) (\f -> Flows.bind (encode (Core.pairTypeSecond v1)) (\s -> Flows.pure (Ast.TypeTuple [
      f,
      s]))))
    Core.TypeRecord v1 -> (ref (Core.rowTypeTypeName v1))
    Core.TypeSet v1 -> (Flows.map Utils.toTypeApplication (Flows.sequence [
      Flows.pure (Ast.TypeVariable (Utils.rawName "S.Set")),
      (encode v1)]))
    Core.TypeUnion v1 ->  
      let typeName = (Core.rowTypeTypeName v1)
      in (ref typeName)
    Core.TypeUnit -> (Flows.pure unitTuple)
    Core.TypeVariable v1 -> (ref v1)
    Core.TypeWrap v1 ->  
      let name = (Core.wrappedTypeTypeName v1)
      in (ref name)
    _ -> (Flows.fail (Strings.cat2 "unexpected type: " (Core__.type_ typ)))) (Rewriting.deannotateType typ)))

encodeTypeWithClassAssertions :: (Module.Namespaces Ast.ModuleName -> M.Map Core.Name (S.Set Classes.TypeClass) -> Core.Type -> Compute.Flow Graph.Graph Ast.Type)
encodeTypeWithClassAssertions namespaces explicitClasses typ =  
  let classes = (Maps.union explicitClasses (getImplicitTypeClasses typ)) 
      implicitClasses = (getImplicitTypeClasses typ)
      encodeAssertion = (\pair ->  
              let name = (Pairs.first pair) 
                  cls = (Pairs.second pair)
                  hname = (Utils.rawName ((\x -> case x of
                          Classes.TypeClassEquality -> "Eq"
                          Classes.TypeClassOrdering -> "Ord") cls))
                  htype = (Ast.TypeVariable (Utils.rawName (Core.unName name)))
              in (Ast.AssertionClass (Ast.ClassAssertion {
                Ast.classAssertionName = hname,
                Ast.classAssertionTypes = [
                  htype]})))
      assertPairs = (Lists.concat (Lists.map toPairs (Maps.toList classes)))
      toPairs = (\mapEntry ->  
              let name = (Pairs.first mapEntry) 
                  clsSet = (Pairs.second mapEntry)
                  toPair = (\c -> (name, c))
              in (Lists.map toPair (Sets.toList clsSet)))
  in (Monads.withTrace "encode with assertions" (Flows.bind (adaptTypeToHaskellAndEncode namespaces typ) (\htyp -> Logic.ifElse (Lists.null assertPairs) (Flows.pure htyp) ( 
    let encoded = (Lists.map encodeAssertion assertPairs) 
        hassert = (Logic.ifElse (Equality.gt (Lists.length encoded) 1) (Lists.head encoded) (Ast.AssertionTuple encoded))
    in (Flows.pure (Ast.TypeCtx (Ast.ContextType {
      Ast.contextTypeCtx = hassert,
      Ast.contextTypeType = htyp})))))))

findOrdVariables :: (Core.Type -> S.Set Core.Name)
findOrdVariables typ =  
  let fold = (\names -> \typ_ -> (\x -> case x of
          Core.TypeMap v1 ->  
            let kt = (Core.mapTypeKeys v1)
            in (tryType names kt)
          Core.TypeSet v1 -> (tryType names v1)
          _ -> names) typ_) 
      isTypeVariable = (\v ->  
              let nameStr = (Core.unName v) 
                  hasNoNamespace = (Maybes.isNothing (Names.namespaceOf v))
                  startsWithT = (Equality.equal (Strings.charAt 0 nameStr) 116)
              in (Logic.and hasNoNamespace startsWithT))
      tryType = (\names -> \t -> (\x -> case x of
              Core.TypeVariable v1 -> (Logic.ifElse (isTypeVariable v1) (Sets.insert v1 names) names)
              _ -> names) (Rewriting.deannotateType t))
  in (Rewriting.foldOverType Coders.TraversalOrderPre fold Sets.empty typ)

getImplicitTypeClasses :: (Core.Type -> M.Map Core.Name (S.Set Classes.TypeClass))
getImplicitTypeClasses typ =  
  let toPair = (\name -> (name, (Sets.fromList [
          Classes.TypeClassOrdering])))
  in (Maps.fromList (Lists.map toPair (Sets.toList (findOrdVariables typ))))

moduleToHaskellModule :: (Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph Ast.Module)
moduleToHaskellModule mod defs = (Flows.bind (Utils.namespacesForModule mod) (\namespaces -> constructModule namespaces mod defs))

moduleToHaskell :: (Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph (M.Map String String))
moduleToHaskell mod defs = (Flows.bind (moduleToHaskellModule mod defs) (\hsmod ->  
  let s = (Serialization.printExpr (Serialization.parenthesize (Serde.moduleToExpr hsmod))) 
      filepath = (Names.namespaceToFilePath Util.CaseConventionPascal (Module.FileExtension "hs") (Module.moduleNamespace mod))
  in (Flows.pure (Maps.singleton filepath s))))

nameDecls :: (t0 -> Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Type -> [Ast.DeclarationWithComments])
nameDecls g namespaces name typ =  
  let nm = (Core.unName name) 
      toDecl = (\n -> \pair ->  
              let k = (Pairs.first pair) 
                  v = (Pairs.second pair)
                  decl = (Ast.DeclarationValueBinding (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
                          Ast.simpleValueBindingPattern = (Utils.applicationPattern (Utils.simpleName k) []),
                          Ast.simpleValueBindingRhs = (Ast.RightHandSide (Ast.ExpressionApplication (Ast.ApplicationExpression {
                            Ast.applicationExpressionFunction = (Ast.ExpressionVariable (Utils.elementReference namespaces n)),
                            Ast.applicationExpressionArgument = (Ast.ExpressionLiteral (Ast.LiteralString v))}))),
                          Ast.simpleValueBindingLocalBindings = Nothing})))
              in Ast.DeclarationWithComments {
                Ast.declarationWithCommentsBody = decl,
                Ast.declarationWithCommentsComments = Nothing})
      nameDecl = (constantForTypeName name, nm)
      fieldDecls = (Lists.map toConstant (Lexical.fieldsOf typ))
      toConstant = (\fieldType ->  
              let fname = (Core.fieldTypeName fieldType)
              in (constantForFieldName name fname, (Core.unName fname)))
  in (Logic.ifElse useCoreImport (Lists.cons (toDecl (Core.Name "hydra.core.Name") nameDecl) (Lists.map (toDecl (Core.Name "hydra.core.Name")) fieldDecls)) [])

toDataDeclaration :: (Module.Namespaces Ast.ModuleName -> Module.TermDefinition -> Compute.Flow Graph.Graph Ast.DeclarationWithComments)
toDataDeclaration namespaces def =  
  let name = (Module.termDefinitionName def) 
      term = (Module.termDefinitionTerm def)
      typ = (Module.termDefinitionType def)
      hname = (Utils.simpleName (Names.localNameOf name))
      rewriteValueBinding = (\vb -> (\x -> case x of
              Ast.ValueBindingSimple v1 ->  
                let pattern_ = (Ast.simpleValueBindingPattern v1) 
                    rhs = (Ast.simpleValueBindingRhs v1)
                    bindings = (Ast.simpleValueBindingLocalBindings v1)
                in ((\x -> case x of
                  Ast.PatternApplication v2 ->  
                    let name_ = (Ast.applicationPatternName v2) 
                        args = (Ast.applicationPatternArgs v2)
                        rhsExpr = (Ast.unRightHandSide rhs)
                    in ((\x -> case x of
                      Ast.ExpressionLambda v3 ->  
                        let vars = (Ast.lambdaExpressionBindings v3) 
                            body = (Ast.lambdaExpressionInner v3)
                            newPattern = (Utils.applicationPattern name_ (Lists.concat2 args vars))
                            newRhs = (Ast.RightHandSide body)
                        in (rewriteValueBinding (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
                          Ast.simpleValueBindingPattern = newPattern,
                          Ast.simpleValueBindingRhs = newRhs,
                          Ast.simpleValueBindingLocalBindings = bindings})))
                      _ -> vb) rhsExpr)
                  _ -> vb) pattern_)) vb)
      toDecl = (\comments -> \hname_ -> \term_ -> \bindings -> (\x -> case x of
              Core.TermLet v1 ->  
                let lbindings = (Core.letBindings v1) 
                    env = (Core.letBody v1)
                    toBinding = (\hname_ -> \hterm_ -> Ast.LocalBindingValue (Utils.simpleValueBinding hname_ hterm_ Nothing))
                    hnames = (Lists.map (\binding -> Utils.simpleName (Core.unName (Core.bindingName binding))) lbindings)
                    terms = (Lists.map Core.bindingTerm lbindings)
                in (Flows.bind (Flows.mapList (encodeTerm namespaces) terms) (\hterms ->  
                  let hbindings = (Lists.zipWith toBinding hnames hterms)
                  in (toDecl comments hname_ env (Just (Ast.LocalBindings hbindings)))))
              _ -> (Flows.bind (encodeTerm namespaces term_) (\hterm ->  
                let vb = (Utils.simpleValueBinding hname_ hterm bindings)
                in (Flows.bind (Annotations.getTypeClasses (Rewriting.removeTypesFromTerm term)) (\explicitClasses -> Flows.bind (encodeTypeWithClassAssertions namespaces explicitClasses typ) (\htype ->  
                  let decl = (Ast.DeclarationTypedBinding (Ast.TypedBinding {
                          Ast.typedBindingTypeSignature = Ast.TypeSignature {
                            Ast.typeSignatureName = hname_,
                            Ast.typeSignatureType = htype},
                          Ast.typedBindingValueBinding = (rewriteValueBinding vb)}))
                  in (Flows.pure (Ast.DeclarationWithComments {
                    Ast.declarationWithCommentsBody = decl,
                    Ast.declarationWithCommentsComments = comments})))))))) (Rewriting.deannotateTerm term_))
  in (Flows.bind (Annotations.getTermDescription term) (\comments -> toDecl comments hname term Nothing))

toTypeDeclarationsFrom :: (Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Type -> Compute.Flow Graph.Graph [Ast.DeclarationWithComments])
toTypeDeclarationsFrom namespaces elementName typ =  
  let lname = (Names.localNameOf elementName) 
      hname = (Utils.simpleName lname)
      declHead = (\name -> \vars_ -> Logic.ifElse (Lists.null vars_) (Ast.DeclarationHeadSimple name) ( 
              let h = (Lists.head vars_) 
                  rest = (Lists.tail vars_)
                  hvar = (Ast.Variable (Utils.simpleName (Core.unName h)))
              in (Ast.DeclarationHeadApplication (Ast.ApplicationDeclarationHead {
                Ast.applicationDeclarationHeadFunction = (declHead name rest),
                Ast.applicationDeclarationHeadOperand = hvar}))))
      newtypeCons = (\tname -> \typ_ ->  
              let hname = (Utils.simpleName (Utils.newtypeAccessorName tname))
              in (Flows.bind (adaptTypeToHaskellAndEncode namespaces typ_) (\htype ->  
                let hfield = Ast.FieldWithComments {
                        Ast.fieldWithCommentsField = Ast.Field {
                          Ast.fieldName = hname,
                          Ast.fieldType = htype},
                        Ast.fieldWithCommentsComments = Nothing} 
                    constructorName = (Utils.simpleName (Names.localNameOf tname))
                in (Flows.pure (Ast.ConstructorWithComments {
                  Ast.constructorWithCommentsBody = (Ast.ConstructorRecord (Ast.RecordConstructor {
                    Ast.recordConstructorName = constructorName,
                    Ast.recordConstructorFields = [
                      hfield]})),
                  Ast.constructorWithCommentsComments = Nothing})))))
      recordCons = (\lname_ -> \fields ->  
              let toField = (\fieldType ->  
                      let fname = (Core.fieldTypeName fieldType) 
                          ftype = (Core.fieldTypeType fieldType)
                          hname_ = (Utils.simpleName (Strings.cat2 (Formatting.decapitalize lname_) (Formatting.capitalize (Core.unName fname))))
                      in (Flows.bind (adaptTypeToHaskellAndEncode namespaces ftype) (\htype -> Flows.bind (Annotations.getTypeDescription ftype) (\comments -> Flows.pure (Ast.FieldWithComments {
                        Ast.fieldWithCommentsField = Ast.Field {
                          Ast.fieldName = hname_,
                          Ast.fieldType = htype},
                        Ast.fieldWithCommentsComments = comments})))))
              in (Flows.bind (Flows.mapList toField fields) (\hFields -> Flows.pure (Ast.ConstructorWithComments {
                Ast.constructorWithCommentsBody = (Ast.ConstructorRecord (Ast.RecordConstructor {
                  Ast.recordConstructorName = (Utils.simpleName lname_),
                  Ast.recordConstructorFields = hFields})),
                Ast.constructorWithCommentsComments = Nothing}))))
      unionCons = (\g_ -> \lname_ -> \fieldType ->  
              let fname = (Core.fieldTypeName fieldType) 
                  ftype = (Core.fieldTypeType fieldType)
                  deconflict = (\name ->  
                          let tname = (Names.unqualifyName (Module.QualifiedName {
                                  Module.qualifiedNameNamespace = (Just (Pairs.first (Module.namespacesFocus namespaces))),
                                  Module.qualifiedNameLocal = name}))
                          in (Logic.ifElse (Maybes.isJust (Maps.lookup tname (Graph.graphElements g_))) (deconflict (Strings.cat2 name "_")) name))
              in (Flows.bind (Annotations.getTypeDescription ftype) (\comments ->  
                let nm = (deconflict (Strings.cat2 (Formatting.capitalize lname_) (Formatting.capitalize (Core.unName fname))))
                in (Flows.bind (Logic.ifElse (Equality.equal (Rewriting.deannotateType ftype) Core.TypeUnit) (Flows.pure []) (Flows.bind (adaptTypeToHaskellAndEncode namespaces ftype) (\htype -> Flows.pure [
                  htype]))) (\typeList -> Flows.pure (Ast.ConstructorWithComments {
                  Ast.constructorWithCommentsBody = (Ast.ConstructorOrdinary (Ast.OrdinaryConstructor {
                    Ast.ordinaryConstructorName = (Utils.simpleName nm),
                    Ast.ordinaryConstructorFields = typeList})),
                  Ast.constructorWithCommentsComments = comments}))))))
  in (Monads.withTrace (Strings.cat2 "type definition " (Core.unName elementName)) (Flows.bind Monads.getState (\g -> Flows.bind (Schemas.isSerializableByName elementName) (\isSer ->  
    let deriv = (Ast.Deriving (Logic.ifElse isSer (Lists.map Utils.rawName [
            "Eq",
            "Ord",
            "Read",
            "Show"]) [])) 
        unpackResult = (Utils.unpackForallType g typ)
        vars = (Pairs.first unpackResult)
        t_ = (Pairs.second unpackResult)
        hd = (declHead hname (Lists.reverse vars))
    in (Flows.bind ((\x -> case x of
      Core.TypeRecord v1 -> (Flows.bind (recordCons lname (Core.rowTypeFields v1)) (\cons -> Flows.pure (Ast.DeclarationData (Ast.DataDeclaration {
        Ast.dataDeclarationKeyword = Ast.DataOrNewtypeData,
        Ast.dataDeclarationContext = [],
        Ast.dataDeclarationHead = hd,
        Ast.dataDeclarationConstructors = [
          cons],
        Ast.dataDeclarationDeriving = [
          deriv]}))))
      Core.TypeUnion v1 -> (Flows.bind (Flows.mapList (unionCons g lname) (Core.rowTypeFields v1)) (\cons -> Flows.pure (Ast.DeclarationData (Ast.DataDeclaration {
        Ast.dataDeclarationKeyword = Ast.DataOrNewtypeData,
        Ast.dataDeclarationContext = [],
        Ast.dataDeclarationHead = hd,
        Ast.dataDeclarationConstructors = cons,
        Ast.dataDeclarationDeriving = [
          deriv]}))))
      Core.TypeWrap v1 ->  
        let wt = (Core.wrappedTypeBody v1)
        in (Flows.bind (newtypeCons elementName wt) (\cons -> Flows.pure (Ast.DeclarationData (Ast.DataDeclaration {
          Ast.dataDeclarationKeyword = Ast.DataOrNewtypeNewtype,
          Ast.dataDeclarationContext = [],
          Ast.dataDeclarationHead = hd,
          Ast.dataDeclarationConstructors = [
            cons],
          Ast.dataDeclarationDeriving = [
            deriv]}))))
      _ -> (Flows.bind (adaptTypeToHaskellAndEncode namespaces typ) (\htype -> Flows.pure (Ast.DeclarationType (Ast.TypeDeclaration {
        Ast.typeDeclarationName = hd,
        Ast.typeDeclarationType = htype}))))) (Rewriting.deannotateType t_)) (\decl -> Flows.bind (Annotations.getTypeDescription typ) (\comments -> Flows.bind (Logic.ifElse includeTypeDefinitions (Flows.bind (typeDecl namespaces elementName typ) (\decl_ -> Flows.pure [
      decl_])) (Flows.pure [])) (\tdecls ->  
      let mainDecl = Ast.DeclarationWithComments {
              Ast.declarationWithCommentsBody = decl,
              Ast.declarationWithCommentsComments = comments} 
          nameDecls_ = (nameDecls g namespaces elementName typ)
      in (Flows.pure (Lists.concat [
        [
          mainDecl],
        nameDecls_,
        tdecls]))))))))))

typeDecl :: (Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Type -> Compute.Flow Graph.Graph Ast.DeclarationWithComments)
typeDecl namespaces name typ =  
  let typeName = (\ns -> \name_ -> Names.qname ns (typeNameLocal name_)) 
      typeNameLocal = (\name_ -> Strings.cat [
              "_",
              Names.localNameOf name_,
              "_type_"])
      rawTerm = (Core_.type_ typ)
      rewrite = (\recurse -> \term ->  
              let variantResult = ((\x -> case x of
                      Core.TermUnion v1 -> (Logic.ifElse (Equality.equal (Core.injectionTypeName v1) (Core.Name "hydra.core.Type")) (Just (Core.injectionField v1)) Nothing)
                      _ -> Nothing) (Rewriting.deannotateTerm term)) 
                  decodeString = (\term -> (\x -> case x of
                          Core.TermLiteral v1 -> ((\x -> case x of
                            Core.LiteralString v2 -> (Just v2)
                            _ -> Nothing) v1)
                          _ -> Nothing) (Rewriting.deannotateTerm term))
                  decodeName = (\term -> (\x -> case x of
                          Core.TermWrap v1 -> (Logic.ifElse (Equality.equal (Core.wrappedTermTypeName v1) (Core.Name "hydra.core.Name")) (Maybes.map (\x -> Core.Name x) (decodeString (Core.wrappedTermBody v1))) Nothing)
                          _ -> Nothing) (Rewriting.deannotateTerm term))
                  forType = (\field ->  
                          let fname = (Core.fieldName field) 
                              fterm = (Core.fieldTerm field)
                          in (Logic.ifElse (Equality.equal fname (Core.Name "record")) Nothing (Logic.ifElse (Equality.equal fname (Core.Name "variable")) (Maybes.bind (decodeName fterm) forVariableType) Nothing)))
                  forVariableType = (\vname ->  
                          let qname = (Names.qualifyName vname) 
                              mns = (Module.qualifiedNameNamespace qname)
                              local = (Module.qualifiedNameLocal qname)
                          in (Maybes.map (\ns -> Core.TermVariable (Names.qname ns (Strings.cat [
                            "_",
                            local,
                            "_type_"]))) mns))
              in (Maybes.fromMaybe (recurse term) (Maybes.bind variantResult forType)))
      finalTerm = (Rewriting.rewriteTerm rewrite rawTerm)
  in (Flows.bind (Modules.constructCoder Language.haskellLanguage (encodeTerm namespaces) (Core.TypeVariable (Core.Name "hydra.core.Type"))) (\coder -> Flows.bind (Compute.coderEncode coder finalTerm) (\expr ->  
    let rhs = (Ast.RightHandSide expr) 
        hname = (Utils.simpleName (typeNameLocal name))
        pat = (Utils.applicationPattern hname [])
        decl = (Ast.DeclarationValueBinding (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
                Ast.simpleValueBindingPattern = pat,
                Ast.simpleValueBindingRhs = rhs,
                Ast.simpleValueBindingLocalBindings = Nothing})))
    in (Flows.pure (Ast.DeclarationWithComments {
      Ast.declarationWithCommentsBody = decl,
      Ast.declarationWithCommentsComments = Nothing})))))
