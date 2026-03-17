-- Note: this is an automatically generated file. Do not edit.

-- | Scala code generator: converts Hydra modules to Scala source code

module Hydra.Ext.Scala.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Scala.Meta as Meta
import qualified Hydra.Ext.Scala.Serde as Serde
import qualified Hydra.Ext.Scala.Utils as Utils
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
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
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a Hydra module to Scala source code
moduleToScala :: Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (M.Map String String)
moduleToScala mod defs cx g =
    Eithers.bind (constructModule cx g mod defs) (\pkg ->
      let s = Serialization.printExpr (Serialization.parenthesize (Serde.writePkg pkg))
      in (Right (Maps.singleton (Names.namespaceToFilePath Util.CaseConventionCamel (Module.FileExtension "scala") (Module.moduleNamespace mod)) s)))

-- | Construct a Scala package from a Hydra module and its definitions
constructModule :: Context.Context -> Graph.Graph -> Module.Module -> [Module.Definition] -> Either (Context.InContext Error.Error) Meta.Pkg
constructModule cx g mod defs =

      let partitioned = Schemas.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          termDefs = Pairs.second partitioned
          nsName = Module.unNamespace (Module.moduleNamespace mod)
          pname = Meta.Data_Name {
                Meta.data_NameValue = (Meta.PredefString (Strings.intercalate "." (Strings.splitOn "." nsName)))}
          pref = Meta.Data_RefName pname
      in (Eithers.bind (Eithers.mapList (\td -> encodeTypeDefinition cx g td) typeDefs) (\typeDeclStats -> Eithers.bind (Eithers.mapList (\td -> encodeTermDefinition cx g td) termDefs) (\termDeclStats -> Eithers.bind (findImports cx g mod) (\imports -> Right (Meta.Pkg {
        Meta.pkgName = pname,
        Meta.pkgRef = pref,
        Meta.pkgStats = (Lists.concat [
          imports,
          typeDeclStats,
          termDeclStats])})))))

-- | Find import statements for the module
findImports :: Context.Context -> Graph.Graph -> Module.Module -> Either (Context.InContext Error.Error) [Meta.Stat]
findImports cx g mod =
    Eithers.bind (Schemas.moduleDependencyNamespaces cx g False False True False mod) (\elImps -> Eithers.bind (Schemas.moduleDependencyNamespaces cx g False True False False mod) (\primImps -> Right (Lists.concat [
      Lists.map toElImport (Sets.toList elImps),
      (Lists.map toPrimImport (Sets.toList primImps))])))

-- | Create an element import statement
toElImport :: Module.Namespace -> Meta.Stat
toElImport ns =
    Meta.StatImportExport (Meta.ImportExportStatImport (Meta.Import {
      Meta.importImporters = [
        Meta.Importer {
          Meta.importerRef = (Meta.Data_RefName (Meta.Data_Name {
            Meta.data_NameValue = (Meta.PredefString (Strings.intercalate "." (Strings.splitOn "." (Module.unNamespace ns))))})),
          Meta.importerImportees = [
            Meta.ImporteeWildcard]}]}))

-- | Create a primitive import statement
toPrimImport :: Module.Namespace -> Meta.Stat
toPrimImport ns =
    Meta.StatImportExport (Meta.ImportExportStatImport (Meta.Import {
      Meta.importImporters = [
        Meta.Importer {
          Meta.importerRef = (Meta.Data_RefName (Meta.Data_Name {
            Meta.data_NameValue = (Meta.PredefString (Strings.intercalate "." (Strings.splitOn "." (Module.unNamespace ns))))})),
          Meta.importerImportees = []}]}))

-- | Encode a type definition as a Scala statement
encodeTypeDefinition :: Context.Context -> t0 -> Module.TypeDefinition -> Either (Context.InContext Error.Error) Meta.Stat
encodeTypeDefinition cx g td =

      let name = Module.typeDefinitionName td
          typ = Module.typeDefinitionType td
          lname = Names.localNameOf name
          tname = Meta.Type_Name {
                Meta.type_NameValue = lname}
          dname = Meta.Data_Name {
                Meta.data_NameValue = (Meta.PredefString lname)}
          freeVars =
                  Lists.filter (\v -> Logic.not (Lists.elem 46 (Strings.toList (Core.unName v)))) (Sets.toList (Rewriting.freeVariablesInType typ))
          tparams =
                  Lists.map (\_v ->
                    let vn = Core.unName _v
                    in Meta.Type_Param {
                      Meta.type_ParamMods = [],
                      Meta.type_ParamName = (Meta.NameValue vn),
                      Meta.type_ParamTparams = [],
                      Meta.type_ParamTbounds = [],
                      Meta.type_ParamVbounds = [],
                      Meta.type_ParamCbounds = []}) freeVars
      in case (Rewriting.deannotateType typ) of
        Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (\f -> fieldToParam cx g f) v0) (\params -> Right (Meta.StatDefn (Meta.DefnClass (Meta.Defn_Class {
          Meta.defn_ClassMods = [
            Meta.ModCase],
          Meta.defn_ClassName = tname,
          Meta.defn_ClassTparams = tparams,
          Meta.defn_ClassCtor = Meta.Ctor_Primary {
            Meta.ctor_PrimaryMods = [],
            Meta.ctor_PrimaryName = (Meta.NameValue ""),
            Meta.ctor_PrimaryParamss = [
              params]},
          Meta.defn_ClassTemplate = Meta.Template {
            Meta.templateEarly = [],
            Meta.templateInits = [],
            Meta.templateSelf = (Meta.Self ()),
            Meta.templateStats = []}}))))
        Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (\f -> fieldToEnumCase cx g lname tparams f) v0) (\cases -> Right (Meta.StatDefn (Meta.DefnEnum (Meta.Defn_Enum {
          Meta.defn_EnumMods = [],
          Meta.defn_EnumName = tname,
          Meta.defn_EnumTparams = tparams,
          Meta.defn_EnumCtor = Meta.Ctor_Primary {
            Meta.ctor_PrimaryMods = [],
            Meta.ctor_PrimaryName = (Meta.NameValue ""),
            Meta.ctor_PrimaryParamss = []},
          Meta.defn_EnumTemplate = Meta.Template {
            Meta.templateEarly = [],
            Meta.templateInits = [],
            Meta.templateSelf = (Meta.Self ()),
            Meta.templateStats = cases}}))))
        Core.TypeWrap v0 -> Eithers.bind (encodeType cx g v0) (\styp -> Right (Meta.StatDefn (Meta.DefnType (Meta.Defn_Type {
          Meta.defn_TypeMods = [],
          Meta.defn_TypeName = tname,
          Meta.defn_TypeTparams = tparams,
          Meta.defn_TypeBody = styp}))))
        _ -> Eithers.bind (encodeType cx g typ) (\styp -> Right (Meta.StatDefn (Meta.DefnType (Meta.Defn_Type {
          Meta.defn_TypeMods = [],
          Meta.defn_TypeName = Meta.Type_Name {
            Meta.type_NameValue = lname},
          Meta.defn_TypeTparams = tparams,
          Meta.defn_TypeBody = styp}))))

-- | Encode a Hydra function as a Scala expression
encodeFunction :: Context.Context -> Graph.Graph -> M.Map Core.Name Core.Term -> Core.Function -> Maybe Core.Term -> Either (Context.InContext Error.Error) Meta.Data
encodeFunction cx g meta fun arg =
    case fun of
      Core.FunctionLambda v0 ->
        let v = Core.unName (Core.lambdaParameter v0)
            body = Core.lambdaBody v0
        in (Eithers.bind (encodeTerm cx g body) (\sbody -> Eithers.bind (findSdom cx g meta) (\sdom -> Right (Utils.slambda v sbody sdom))))
      Core.FunctionPrimitive v0 -> Right (Utils.sprim v0)
      Core.FunctionElimination v0 -> case v0 of
        Core.EliminationWrap v1 -> Right (Utils.sname (Strings.cat2 (Strings.cat2 "ELIM-NOMINAL(" (Core.unName v1)) ")"))
        Core.EliminationRecord _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unapplied projection not yet supported")),
          Context.inContextContext = cx})
        Core.EliminationUnion v1 ->
          let v = "v"
          in (Eithers.bind (findDomain cx g meta) (\dom -> Eithers.bind (Schemas.fieldTypes cx g dom) (\ftypes ->
            let sn = Utils.nameOfType g dom
                cases = Core.caseStatementCases v1
            in (Eithers.bind (Eithers.mapList (\f -> encodeCase cx g ftypes sn f) cases) (\scases -> Maybes.maybe (Eithers.bind (findSdom cx g meta) (\sdom -> Right (Utils.slambda v (Meta.DataMatch (Meta.Data_Match {
              Meta.data_MatchExpr = (Utils.sname v),
              Meta.data_MatchCases = scases})) sdom))) (\a -> Eithers.bind (encodeTerm cx g a) (\sa -> Right (Meta.DataMatch (Meta.Data_Match {
              Meta.data_MatchExpr = sa,
              Meta.data_MatchCases = scases})))) arg)))))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unsupported elimination")),
          Context.inContextContext = cx})
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unsupported function")),
        Context.inContextContext = cx})

-- | Encode a literal value as a Scala literal
encodeLiteral :: Context.Context -> t0 -> Core.Literal -> Either (Context.InContext Error.Error) Meta.Lit
encodeLiteral cx g av =
    case av of
      Core.LiteralBoolean v0 -> Right (Meta.LitBoolean v0)
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueFloat32 v1 -> Right (Meta.LitFloat v1)
        Core.FloatValueFloat64 v1 -> Right (Meta.LitDouble v1)
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected float value")),
          Context.inContextContext = cx})
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueInt16 v1 -> Right (Meta.LitShort v1)
        Core.IntegerValueInt32 v1 -> Right (Meta.LitInt v1)
        Core.IntegerValueInt64 v1 -> Right (Meta.LitLong v1)
        Core.IntegerValueUint8 v1 -> Right (Meta.LitByte (Literals.bigintToInt8 (Literals.uint8ToBigint v1)))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected integer value")),
          Context.inContextContext = cx})
      Core.LiteralString v0 -> Right (Meta.LitString v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected literal")),
        Context.inContextContext = cx})

-- | Encode a Hydra term as a Scala expression
encodeTerm :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Meta.Data
encodeTerm cx g term =
    case (Rewriting.deannotateTerm term) of
      Core.TermApplication v0 ->
        let fun = Core.applicationFunction v0
            arg = Core.applicationArgument v0
        in case (Rewriting.deannotateTerm fun) of
          Core.TermFunction v1 -> case v1 of
            Core.FunctionElimination v2 -> case v2 of
              Core.EliminationRecord v3 ->
                let fname = Core.unName (Core.projectionField v3)
                in (Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Meta.DataRef (Meta.Data_RefSelect (Meta.Data_Select {
                  Meta.data_SelectQual = sarg,
                  Meta.data_SelectName = Meta.Data_Name {
                    Meta.data_NameValue = (Meta.PredefString fname)}})))))
              Core.EliminationUnion _ -> encodeFunction cx g (Annotations.termAnnotationInternal fun) v1 (Just arg)
              _ -> Eithers.bind (encodeTerm cx g fun) (\sfun -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Utils.sapply sfun [
                sarg])))
            _ -> Eithers.bind (encodeTerm cx g fun) (\sfun -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Utils.sapply sfun [
              sarg])))
          _ -> Eithers.bind (encodeTerm cx g fun) (\sfun -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Utils.sapply sfun [
            sarg])))
      Core.TermFunction v0 -> encodeFunction cx g (Annotations.termAnnotationInternal term) v0 Nothing
      Core.TermList v0 -> Eithers.bind (Eithers.mapList (\e -> encodeTerm cx g e) v0) (\sels -> Right (Utils.sapply (Utils.sname "Seq") sels))
      Core.TermLiteral v0 -> Eithers.bind (encodeLiteral cx g v0) (\slit -> Right (Meta.DataLit slit))
      Core.TermMap v0 -> Eithers.bind (Eithers.mapList (\kv -> Eithers.bind (encodeTerm cx g (Pairs.first kv)) (\sk -> Eithers.bind (encodeTerm cx g (Pairs.second kv)) (\sv -> Right (Utils.sassign sk sv)))) (Maps.toList v0)) (\spairs -> Right (Utils.sapply (Utils.sname "Map") spairs))
      Core.TermWrap v0 -> encodeTerm cx g (Core.wrappedTermBody v0)
      Core.TermMaybe v0 -> Maybes.maybe (Right (Utils.sname "None")) (\t -> Eithers.bind (encodeTerm cx g t) (\s -> Right (Utils.sapply (Utils.sname "Some") [
        s]))) v0
      Core.TermRecord v0 ->
        let rname = Core.recordTypeName v0
            fields = Core.recordFields v0
            n = Utils.scalaTypeName False rname
        in (Eithers.bind (Eithers.mapList (\f -> encodeTerm cx g (Core.fieldTerm f)) fields) (\args -> Right (Utils.sapply (Utils.sname n) args)))
      Core.TermSet v0 -> Eithers.bind (Eithers.mapList (\e -> encodeTerm cx g e) (Sets.toList v0)) (\sels -> Right (Utils.sapply (Utils.sname "Set") sels))
      Core.TermUnion v0 ->
        let sn = Core.injectionTypeName v0
            fn = Core.fieldName (Core.injectionField v0)
            ft = Core.fieldTerm (Core.injectionField v0)
            lhs = Utils.sname (Utils.qualifyUnionFieldName "UNION." (Just sn) fn)
        in case (Rewriting.deannotateTerm ft) of
          Core.TermRecord v1 -> Logic.ifElse (Equality.equal (Lists.length (Core.recordFields v1)) 0) (Right (Utils.sapply lhs [])) (Eithers.bind (encodeTerm cx g ft) (\sarg -> Right (Utils.sapply lhs [
            sarg])))
          _ -> Eithers.bind (encodeTerm cx g ft) (\sarg -> Right (Utils.sapply lhs [
            sarg]))
      Core.TermVariable v0 -> Right (Utils.sname (Core.unName v0))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected term")),
        Context.inContextContext = cx})

-- | Encode a Hydra type as a Scala type
encodeType :: Context.Context -> t0 -> Core.Type -> Either (Context.InContext Error.Error) Meta.Type
encodeType cx g t =
    case (Rewriting.deannotateType t) of
      Core.TypeUnit -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
        Meta.type_NameValue = "Unit"})))
      Core.TypeEither v0 ->
        let lt = Core.eitherTypeLeft v0
            rt = Core.eitherTypeRight v0
        in (Eithers.bind (encodeType cx g lt) (\slt -> Eithers.bind (encodeType cx g rt) (\srt -> Right (Utils.stapply2 (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
          Meta.type_NameValue = "Either"}))) slt srt))))
      Core.TypeFunction v0 ->
        let dom = Core.functionTypeDomain v0
            cod = Core.functionTypeCodomain v0
        in (Eithers.bind (encodeType cx g dom) (\sdom -> Eithers.bind (encodeType cx g cod) (\scod -> Right (Meta.TypeFunctionType (Meta.Type_FunctionTypeFunction (Meta.Type_Function {
          Meta.type_FunctionParams = [
            sdom],
          Meta.type_FunctionRes = scod}))))))
      Core.TypeList v0 -> Eithers.bind (encodeType cx g v0) (\slt -> Right (Utils.stapply1 (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
        Meta.type_NameValue = "Seq"}))) slt))
      Core.TypeLiteral v0 -> case v0 of
        Core.LiteralTypeBinary -> Right (Utils.stapply (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
          Meta.type_NameValue = "Array"}))) [
          Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Byte"}))])
        Core.LiteralTypeBoolean -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
          Meta.type_NameValue = "Boolean"})))
        Core.LiteralTypeFloat v1 -> case v1 of
          Core.FloatTypeBigfloat -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "BigDecimal"})))
          Core.FloatTypeFloat32 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Float"})))
          Core.FloatTypeFloat64 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Double"})))
          _ -> Left (Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError "unsupported float type")),
            Context.inContextContext = cx})
        Core.LiteralTypeInteger v1 -> case v1 of
          Core.IntegerTypeBigint -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "BigInt"})))
          Core.IntegerTypeInt8 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Byte"})))
          Core.IntegerTypeInt16 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Short"})))
          Core.IntegerTypeInt32 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Int"})))
          Core.IntegerTypeInt64 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Long"})))
          Core.IntegerTypeUint8 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Byte"})))
          Core.IntegerTypeUint16 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Int"})))
          Core.IntegerTypeUint32 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "Long"})))
          Core.IntegerTypeUint64 -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
            Meta.type_NameValue = "BigInt"})))
          _ -> Left (Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError "unsupported integer type")),
            Context.inContextContext = cx})
        Core.LiteralTypeString -> Right (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
          Meta.type_NameValue = "String"})))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unsupported literal type")),
          Context.inContextContext = cx})
      Core.TypeMap v0 ->
        let kt = Core.mapTypeKeys v0
            vt = Core.mapTypeValues v0
        in (Eithers.bind (encodeType cx g kt) (\skt -> Eithers.bind (encodeType cx g vt) (\svt -> Right (Utils.stapply2 (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
          Meta.type_NameValue = "Map"}))) skt svt))))
      Core.TypeMaybe v0 -> Eithers.bind (encodeType cx g v0) (\sot -> Right (Utils.stapply1 (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
        Meta.type_NameValue = "Option"}))) sot))
      Core.TypePair v0 ->
        let ft = Core.pairTypeFirst v0
            st = Core.pairTypeSecond v0
        in (Eithers.bind (encodeType cx g ft) (\sft -> Eithers.bind (encodeType cx g st) (\sst -> Right (Utils.stapply2 (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
          Meta.type_NameValue = "Tuple2"}))) sft sst))))
      Core.TypeRecord _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous record type")),
        Context.inContextContext = cx})
      Core.TypeSet v0 -> Eithers.bind (encodeType cx g v0) (\sst -> Right (Utils.stapply1 (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
        Meta.type_NameValue = "Set"}))) sst))
      Core.TypeUnion _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous union type")),
        Context.inContextContext = cx})
      Core.TypeWrap _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous wrap type")),
        Context.inContextContext = cx})
      Core.TypeForall v0 ->
        let v = Core.forallTypeParameter v0
            body = Core.forallTypeBody v0
        in (Eithers.bind (encodeType cx g body) (\sbody -> Right (Meta.TypeLambda (Meta.Type_Lambda {
          Meta.type_LambdaTparams = [
            Utils.stparam v],
          Meta.type_LambdaTpe = sbody}))))
      Core.TypeVariable v0 -> Right (Meta.TypeVar (Meta.Type_Var {
        Meta.type_VarName = Meta.Type_Name {
          Meta.type_NameValue = (Core.unName v0)}}))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unsupported type")),
        Context.inContextContext = cx})

-- | Encode an untyped application term by first inferring types
encodeUntypeApplicationTerm :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Meta.Data
encodeUntypeApplicationTerm cx g term =
    Eithers.bind (Inference.inferInGraphContext cx g term) (\result -> encodeTerm cx g (Typing.inferenceResultTerm result))

-- | Convert a field type to a Scala parameter
fieldToParam :: Context.Context -> t0 -> Core.FieldType -> Either (Context.InContext Error.Error) Meta.Data_Param
fieldToParam cx g ft =

      let fname = Core.unName (Core.fieldTypeName ft)
          ftyp = Core.fieldTypeType ft
      in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Meta.Data_Param {
        Meta.data_ParamMods = [],
        Meta.data_ParamName = (Meta.NameValue fname),
        Meta.data_ParamDecltpe = (Just sftyp),
        Meta.data_ParamDefault = Nothing})))

-- | Convert a field type to a Scala enum case
fieldToEnumCase :: Context.Context -> t0 -> String -> [Meta.Type_Param] -> Core.FieldType -> Either (Context.InContext Error.Error) Meta.Stat
fieldToEnumCase cx g parentName tparams ft =

      let fname = Core.unName (Core.fieldTypeName ft)
          ftyp = Core.fieldTypeType ft
          caseName = Meta.Data_Name {
                Meta.data_NameValue = (Meta.PredefString fname)}
          isUnit =
                  case (Rewriting.deannotateType ftyp) of
                    Core.TypeUnit -> True
                    Core.TypeRecord v0 -> Equality.equal (Lists.length v0) 0
                    _ -> False
          parentType =
                  Logic.ifElse (Lists.null tparams) (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
                    Meta.type_NameValue = parentName}))) (Meta.TypeApply (Meta.Type_Apply {
                    Meta.type_ApplyTpe = (Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
                      Meta.type_NameValue = parentName}))),
                    Meta.type_ApplyArgs = (Lists.map typeParamToTypeVar tparams)}))
      in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Meta.StatDefn (Meta.DefnEnumCase (Meta.Defn_EnumCase {
        Meta.defn_EnumCaseMods = [],
        Meta.defn_EnumCaseName = caseName,
        Meta.defn_EnumCaseTparams = [],
        Meta.defn_EnumCaseCtor = Meta.Ctor_Primary {
          Meta.ctor_PrimaryMods = [],
          Meta.ctor_PrimaryName = (Meta.NameValue ""),
          Meta.ctor_PrimaryParamss = [
            Logic.ifElse isUnit [] [
              Meta.Data_Param {
                Meta.data_ParamMods = [],
                Meta.data_ParamName = (Meta.NameValue "value"),
                Meta.data_ParamDecltpe = (Just sftyp),
                Meta.data_ParamDefault = Nothing}]]},
        Meta.defn_EnumCaseInits = [
          Meta.Init {
            Meta.initTpe = parentType,
            Meta.initName = (Meta.NameValue ""),
            Meta.initArgss = []}]})))))

-- | Convert a type parameter to a type variable reference
typeParamToTypeVar :: Meta.Type_Param -> Meta.Type
typeParamToTypeVar tp =

      let n = Meta.type_ParamName tp
          s =
                  case n of
                    Meta.NameValue v0 -> v0
                    _ -> ""
      in (Meta.TypeVar (Meta.Type_Var {
        Meta.type_VarName = Meta.Type_Name {
          Meta.type_NameValue = s}}))

-- | Encode a term definition as a Scala statement
encodeTermDefinition :: Context.Context -> Graph.Graph -> Module.TermDefinition -> Either (Context.InContext Error.Error) Meta.Stat
encodeTermDefinition cx g td =

      let name = Module.termDefinitionName td
          term = Module.termDefinitionTerm td
          typ = Module.termDefinitionType td
          lname = Names.localNameOf name
          typ_ = Core.typeSchemeType typ
          toVal =
                  \ln -> \r -> Meta.DefnVal (Meta.Defn_Val {
                    Meta.defn_ValMods = [],
                    Meta.defn_ValPats = [
                      Meta.PatVar (Meta.Pat_Var {
                        Meta.pat_VarName = Meta.Data_Name {
                          Meta.data_NameValue = (Meta.PredefString ln)}})],
                    Meta.defn_ValDecltpe = Nothing,
                    Meta.defn_ValRhs = r})
      in (Eithers.bind (encodeTerm cx g term) (\rhs -> case rhs of
        Meta.DataFunctionData v0 -> case (Rewriting.deannotateType typ_) of
          Core.TypeFunction v1 ->
            let cod = Core.functionTypeCodomain v1
                freeTypeVars = Sets.toList (Rewriting.freeVariablesInType typ_)
                tparams = Lists.map (\tv -> Utils.stparam tv) freeTypeVars
            in (Eithers.bind (encodeType cx g cod) (\scod -> case v0 of
              Meta.Data_FunctionDataFunction v2 ->
                let params = Meta.data_FunctionParams v2
                    body = Meta.data_FunctionBody v2
                in (Right (Meta.StatDefn (Meta.DefnDef (Meta.Defn_Def {
                  Meta.defn_DefMods = [],
                  Meta.defn_DefName = Meta.Data_Name {
                    Meta.data_NameValue = (Meta.PredefString lname)},
                  Meta.defn_DefTparams = tparams,
                  Meta.defn_DefParamss = [
                    params],
                  Meta.defn_DefDecltpe = (Just scod),
                  Meta.defn_DefBody = body}))))
              _ -> Right (Meta.StatDefn (toVal lname rhs))))
          _ -> Right (Meta.StatDefn (toVal lname rhs))
        _ -> Right (Meta.StatDefn (toVal lname rhs))))

-- | Find the Scala domain type for a function
findSdom :: Context.Context -> Graph.Graph -> M.Map Core.Name Core.Term -> Either (Context.InContext Error.Error) (Maybe Meta.Type)
findSdom cx g meta =
    Eithers.bind (findDomain cx g meta) (\dom -> Eithers.bind (encodeType cx g dom) (\sdom -> Right (Just sdom)))

-- | Find the domain type from annotations
findDomain :: Context.Context -> Graph.Graph -> M.Map Core.Name Core.Term -> Either (Context.InContext Error.Error) Core.Type
findDomain cx g meta =
    Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
      Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g meta)) (\r -> Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected a typed term")),
      Context.inContextContext = cx})) (\t -> case (Rewriting.deannotateType t) of
      Core.TypeFunction v0 -> Right (Core.functionTypeDomain v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected a function type")),
        Context.inContextContext = cx})) r)

-- | Encode a case branch
encodeCase :: Context.Context -> Graph.Graph -> M.Map Core.Name Core.Type -> Maybe Core.Name -> Core.Field -> Either (Context.InContext Error.Error) Meta.Case
encodeCase cx g ftypes sn f =

      let fname = Core.fieldName f
          fterm = Core.fieldTerm f
          dom = Maybes.fromJust (Maps.lookup fname ftypes)
          v = Core.Name "y"
          patArgs = Logic.ifElse (Equality.equal dom Core.TypeUnit) [] [
                Utils.svar v]
          pat =
                  Meta.PatExtract (Meta.Pat_Extract {
                    Meta.pat_ExtractFun = (Utils.sname (Utils.qualifyUnionFieldName "MATCHED." sn fname)),
                    Meta.pat_ExtractArgs = patArgs})
          applied = applyVar fterm v
      in (Eithers.bind (encodeTerm cx g applied) (\body -> Right (Meta.Case {
        Meta.casePat = pat,
        Meta.caseCond = Nothing,
        Meta.caseBody = body})))

-- | Apply a variable to a term, performing substitution for lambdas
applyVar :: Core.Term -> Core.Name -> Core.Term
applyVar fterm avar =

      let v = Core.unName avar
      in case (Rewriting.deannotateTerm fterm) of
        Core.TermFunction v0 -> case v0 of
          Core.FunctionLambda v1 ->
            let lamParam = Core.lambdaParameter v1
                lamBody = Core.lambdaBody v1
            in (Logic.ifElse (Rewriting.isFreeVariableInTerm lamParam lamBody) lamBody (Rewriting.substituteVariable lamParam avar lamBody))
          _ -> Core.TermApplication (Core.Application {
            Core.applicationFunction = fterm,
            Core.applicationArgument = (Core.TermVariable avar)})
        _ -> Core.TermApplication (Core.Application {
          Core.applicationFunction = fterm,
          Core.applicationArgument = (Core.TermVariable avar)})
