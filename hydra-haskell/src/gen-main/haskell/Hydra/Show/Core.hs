-- | String representations of hydra.core types

module Hydra.Show.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A placeholder for reading terms from their serialized form. Not implemented.
readTerm :: (String -> Maybe Core.Term)
readTerm s = (Just (Core.TermLiteral (Core.LiteralString s)))

-- | Show a binding as a string
binding :: (Core.Binding -> String)
binding el =  
  let name = (Core.unName (Core.bindingName el))
  in  
    let t = (Core.bindingTerm el)
    in  
      let typeStr = (Maybes.maybe "" (\ts -> Strings.cat [
              ":(",
              typeScheme ts,
              ")"]) (Core.bindingType el))
      in (Strings.cat [
        name,
        typeStr,
        " = ",
        (term t)])

-- | Show an elimination as a string
elimination :: (Core.Elimination -> String)
elimination elm = ((\x -> case x of
  Core.EliminationProduct v1 ->  
    let arity = (Core.tupleProjectionArity v1)
    in  
      let index = (Core.tupleProjectionIndex v1)
      in (Strings.cat [
        "[",
        Literals.showInt32 index,
        "/",
        Literals.showInt32 arity,
        "]"])
  Core.EliminationRecord v1 ->  
    let tname = (Core.unName (Core.projectionTypeName v1))
    in  
      let fname = (Core.unName (Core.projectionField v1))
      in (Strings.cat [
        "project(",
        tname,
        "){",
        fname,
        "}"])
  Core.EliminationUnion v1 ->  
    let tname = (Core.unName (Core.caseStatementTypeName v1))
    in  
      let mdef = (Core.caseStatementDefault v1)
      in  
        let cases = (Core.caseStatementCases v1)
        in  
          let defaultField = (Maybes.maybe [] (\d -> [
                  Core.Field {
                    Core.fieldName = (Core.Name "[default]"),
                    Core.fieldTerm = d}]) mdef)
          in  
            let allFields = (Lists.concat [
                    cases,
                    defaultField])
            in (Strings.cat [
              "case(",
              tname,
              ")",
              (fields allFields)])
  Core.EliminationWrap v1 -> (Strings.cat [
    "unwrap(",
    Core.unName v1,
    ")"])) elm)

field :: (Core.Field -> String)
field field =  
  let fname = (Core.unName (Core.fieldName field))
  in  
    let fterm = (Core.fieldTerm field)
    in (Strings.cat2 fname (Strings.cat2 "=" (term fterm)))

fieldType :: (Core.FieldType -> String)
fieldType ft =  
  let fname = (Core.unName (Core.fieldTypeName ft))
  in  
    let ftyp = (Core.fieldTypeType ft)
    in (Strings.cat [
      fname,
      ":",
      (type_ ftyp)])

-- | Show a list of fields as a string
fields :: ([Core.Field] -> String)
fields flds =  
  let fieldStrs = (Lists.map field flds)
  in (Strings.cat [
    "{",
    Strings.intercalate ", " fieldStrs,
    "}"])

-- | Show a float value as a string
float :: (Core.FloatValue -> String)
float fv = ((\x -> case x of
  Core.FloatValueBigfloat v1 -> (Strings.cat [
    Literals.showBigfloat v1,
    ":bigfloat"])
  Core.FloatValueFloat32 v1 -> (Strings.cat [
    Literals.showFloat32 v1,
    ":float32"])
  Core.FloatValueFloat64 v1 -> (Strings.cat [
    Literals.showFloat64 v1,
    ":float64"])) fv)

-- | Show a float type as a string
floatType :: (Core.FloatType -> String)
floatType ft = ((\x -> case x of
  Core.FloatTypeBigfloat -> "bigfloat"
  Core.FloatTypeFloat32 -> "float32"
  Core.FloatTypeFloat64 -> "float64") ft)

-- | Show a function as a string
function :: (Core.Function -> String)
function f = ((\x -> case x of
  Core.FunctionElimination v1 -> (elimination v1)
  Core.FunctionLambda v1 -> (lambda v1)
  Core.FunctionPrimitive v1 -> (Strings.cat2 (Core.unName v1) "!")) f)

-- | Show an injection as a string
injection :: (Core.Injection -> String)
injection inj =  
  let tname = (Core.injectionTypeName inj)
  in  
    let f = (Core.injectionField inj)
    in (Strings.cat [
      "inject(",
      Core.unName tname,
      ")",
      (fields [
        f])])

-- | Show an integer value as a string
integer :: (Core.IntegerValue -> String)
integer iv = ((\x -> case x of
  Core.IntegerValueBigint v1 -> (Strings.cat [
    Literals.showBigint v1,
    ":bigint"])
  Core.IntegerValueInt8 v1 -> (Strings.cat [
    Literals.showInt8 v1,
    ":int8"])
  Core.IntegerValueInt16 v1 -> (Strings.cat [
    Literals.showInt16 v1,
    ":int16"])
  Core.IntegerValueInt32 v1 -> (Strings.cat [
    Literals.showInt32 v1,
    ":int32"])
  Core.IntegerValueInt64 v1 -> (Strings.cat [
    Literals.showInt64 v1,
    ":int64"])
  Core.IntegerValueUint8 v1 -> (Strings.cat [
    Literals.showUint8 v1,
    ":uint8"])
  Core.IntegerValueUint16 v1 -> (Strings.cat [
    Literals.showUint16 v1,
    ":uint16"])
  Core.IntegerValueUint32 v1 -> (Strings.cat [
    Literals.showUint32 v1,
    ":uint32"])
  Core.IntegerValueUint64 v1 -> (Strings.cat [
    Literals.showUint64 v1,
    ":uint64"])) iv)

-- | Show an integer type as a string
integerType :: (Core.IntegerType -> String)
integerType it = ((\x -> case x of
  Core.IntegerTypeBigint -> "bigint"
  Core.IntegerTypeInt8 -> "int8"
  Core.IntegerTypeInt16 -> "int16"
  Core.IntegerTypeInt32 -> "int32"
  Core.IntegerTypeInt64 -> "int64"
  Core.IntegerTypeUint8 -> "uint8"
  Core.IntegerTypeUint16 -> "uint16"
  Core.IntegerTypeUint32 -> "uint32"
  Core.IntegerTypeUint64 -> "uint64") it)

-- | Show a lambda as a string
lambda :: (Core.Lambda -> String)
lambda l =  
  let v = (Core.unName (Core.lambdaParameter l))
  in  
    let mt = (Core.lambdaDomain l)
    in  
      let body = (Core.lambdaBody l)
      in  
        let typeStr = (Maybes.maybe "" (\t -> Strings.cat2 ":" (type_ t)) mt)
        in (Strings.cat [
          "\955",
          v,
          typeStr,
          ".",
          (term body)])

list :: ((t0 -> String) -> [t0] -> String)
list f xs =  
  let elementStrs = (Lists.map f xs)
  in (Strings.cat [
    "[",
    Strings.intercalate ", " elementStrs,
    "]"])

-- | Show a literal as a string
literal :: (Core.Literal -> String)
literal l = ((\x -> case x of
  Core.LiteralBinary _ -> "[binary]"
  Core.LiteralBoolean v1 -> (Logic.ifElse v1 "true" "false")
  Core.LiteralFloat v1 -> (float v1)
  Core.LiteralInteger v1 -> (integer v1)
  Core.LiteralString v1 -> (Literals.showString v1)) l)

-- | Show a literal type as a string
literalType :: (Core.LiteralType -> String)
literalType lt = ((\x -> case x of
  Core.LiteralTypeBinary -> "binary"
  Core.LiteralTypeBoolean -> "boolean"
  Core.LiteralTypeFloat v1 -> (floatType v1)
  Core.LiteralTypeInteger v1 -> (integerType v1)
  Core.LiteralTypeString -> "string") lt)

-- | Show a term as a string
term :: (Core.Term -> String)
term t =  
  let gatherTerms = (\prev -> \app ->  
          let lhs = (Core.applicationFunction app)
          in  
            let rhs = (Core.applicationArgument app)
            in ((\x -> case x of
              Core.TermApplication v1 -> (gatherTerms (Lists.cons rhs prev) v1)
              _ -> (Lists.cons lhs (Lists.cons rhs prev))) lhs))
  in ((\x -> case x of
    Core.TermAnnotated v1 -> (term (Core.annotatedTermBody v1))
    Core.TermApplication v1 ->  
      let terms = (gatherTerms [] v1)
      in  
        let termStrs = (Lists.map term terms)
        in (Strings.cat [
          "(",
          Strings.intercalate " @ " termStrs,
          ")"])
    Core.TermEither v1 -> (Eithers.either (\l -> Strings.cat [
      "left(",
      term l,
      ")"]) (\r -> Strings.cat [
      "right(",
      term r,
      ")"]) v1)
    Core.TermFunction v1 -> (function v1)
    Core.TermLet v1 ->  
      let bindings = (Core.letBindings v1)
      in  
        let env = (Core.letBody v1)
        in  
          let bindingStrs = (Lists.map binding bindings)
          in (Strings.cat [
            "let ",
            Strings.intercalate ", " bindingStrs,
            " in ",
            (term env)])
    Core.TermList v1 ->  
      let termStrs = (Lists.map term v1)
      in (Strings.cat [
        "[",
        Strings.intercalate ", " termStrs,
        "]"])
    Core.TermLiteral v1 -> (literal v1)
    Core.TermMap v1 ->  
      let entry = (\p -> Strings.cat [
              term (fst p),
              "=",
              (term (snd p))])
      in (Strings.cat [
        "{",
        Strings.intercalate ", " (Lists.map entry (Maps.toList v1)),
        "}"])
    Core.TermMaybe v1 -> (Maybes.maybe "nothing" (\t -> Strings.cat [
      "just(",
      term t,
      ")"]) v1)
    Core.TermPair v1 -> (Strings.cat [
      "(",
      term (fst v1),
      ", ",
      term (snd v1),
      ")"])
    Core.TermProduct v1 ->  
      let termStrs = (Lists.map term v1)
      in (Strings.cat [
        "(",
        Strings.intercalate ", " termStrs,
        ")"])
    Core.TermRecord v1 ->  
      let tname = (Core.unName (Core.recordTypeName v1))
      in  
        let flds = (Core.recordFields v1)
        in (Strings.cat [
          "record(",
          tname,
          ")",
          (fields flds)])
    Core.TermSet v1 -> (Strings.cat [
      "{",
      Strings.intercalate ", " (Lists.map term (Sets.toList v1)),
      "}"])
    Core.TermSum v1 ->  
      let index = (Core.sumIndex v1)
      in  
        let size = (Core.sumSize v1)
        in  
          let t2 = (Core.sumTerm v1)
          in (Strings.cat [
            "(",
            Literals.showInt32 index,
            "/",
            Literals.showInt32 size,
            "=",
            term t2,
            ")"])
    Core.TermTypeLambda v1 ->  
      let param = (Core.unName (Core.typeLambdaParameter v1))
      in  
        let body = (Core.typeLambdaBody v1)
        in (Strings.cat [
          "\923",
          param,
          ".",
          (term body)])
    Core.TermTypeApplication v1 ->  
      let t2 = (Core.typeApplicationTermBody v1)
      in  
        let typ = (Core.typeApplicationTermType v1)
        in (Strings.cat [
          term t2,
          "\10216",
          type_ typ,
          "\10217"])
    Core.TermUnion v1 -> (injection v1)
    Core.TermUnit -> "unit"
    Core.TermVariable v1 -> (Core.unName v1)
    Core.TermWrap v1 ->  
      let tname = (Core.unName (Core.wrappedTermTypeName v1))
      in  
        let term1 = (Core.wrappedTermBody v1)
        in (Strings.cat [
          "wrap(",
          tname,
          "){",
          term term1,
          "}"])) t)

-- | Show a type as a string
type_ :: (Core.Type -> String)
type_ typ =  
  let showRowType = (\rt ->  
          let flds = (Core.rowTypeFields rt)
          in  
            let fieldStrs = (Lists.map fieldType flds)
            in (Strings.cat [
              "{",
              Strings.intercalate ", " fieldStrs,
              "}"]))
  in  
    let gatherTypes = (\prev -> \app ->  
            let lhs = (Core.applicationTypeFunction app)
            in  
              let rhs = (Core.applicationTypeArgument app)
              in ((\x -> case x of
                Core.TypeApplication v1 -> (gatherTypes (Lists.cons rhs prev) v1)
                _ -> (Lists.cons lhs (Lists.cons rhs prev))) lhs))
    in  
      let gatherFunctionTypes = (\prev -> \t -> (\x -> case x of
              Core.TypeFunction v1 ->  
                let dom = (Core.functionTypeDomain v1)
                in  
                  let cod = (Core.functionTypeCodomain v1)
                  in (gatherFunctionTypes (Lists.cons dom prev) cod)
              _ -> (Lists.reverse (Lists.cons t prev))) t)
      in ((\x -> case x of
        Core.TypeAnnotated v1 -> (type_ (Core.annotatedTypeBody v1))
        Core.TypeApplication v1 ->  
          let types = (gatherTypes [] v1)
          in  
            let typeStrs = (Lists.map type_ types)
            in (Strings.cat [
              "(",
              Strings.intercalate " @ " typeStrs,
              ")"])
        Core.TypeEither v1 ->  
          let leftTyp = (Core.eitherTypeLeft v1)
          in  
            let rightTyp = (Core.eitherTypeRight v1)
            in (Strings.cat [
              "either<",
              type_ leftTyp,
              ", ",
              type_ rightTyp,
              ">"])
        Core.TypeForall v1 ->  
          let var = (Core.unName (Core.forallTypeParameter v1))
          in  
            let body = (Core.forallTypeBody v1)
            in (Strings.cat [
              "(\8704",
              var,
              ".",
              type_ body,
              ")"])
        Core.TypeFunction _ ->  
          let types = (gatherFunctionTypes [] typ)
          in  
            let typeStrs = (Lists.map type_ types)
            in (Strings.cat [
              "(",
              Strings.intercalate " \8594 " typeStrs,
              ")"])
        Core.TypeList v1 -> (Strings.cat [
          "list<",
          type_ v1,
          ">"])
        Core.TypeLiteral v1 -> (literalType v1)
        Core.TypeMap v1 ->  
          let keyTyp = (Core.mapTypeKeys v1)
          in  
            let valTyp = (Core.mapTypeValues v1)
            in (Strings.cat [
              "map<",
              type_ keyTyp,
              ", ",
              type_ valTyp,
              ">"])
        Core.TypeMaybe v1 -> (Strings.cat [
          "maybe<",
          type_ v1,
          ">"])
        Core.TypePair v1 ->  
          let firstTyp = (Core.pairTypeFirst v1)
          in  
            let secondTyp = (Core.pairTypeSecond v1)
            in (Strings.cat [
              "(",
              type_ firstTyp,
              ", ",
              type_ secondTyp,
              ")"])
        Core.TypeProduct v1 ->  
          let typeStrs = (Lists.map type_ v1)
          in (Strings.intercalate "\215" typeStrs)
        Core.TypeRecord v1 -> (Strings.cat2 "record" (showRowType v1))
        Core.TypeSet v1 -> (Strings.cat [
          "set<",
          type_ v1,
          ">"])
        Core.TypeSum v1 ->  
          let typeStrs = (Lists.map type_ v1)
          in (Strings.intercalate "+" typeStrs)
        Core.TypeUnion v1 -> (Strings.cat2 "union" (showRowType v1))
        Core.TypeUnit -> "unit"
        Core.TypeVariable v1 -> (Core.unName v1)
        Core.TypeWrap v1 ->  
          let tname = (Core.unName (Core.wrappedTypeTypeName v1))
          in  
            let typ1 = (Core.wrappedTypeBody v1)
            in (Strings.cat [
              "wrap[",
              tname,
              "](",
              type_ typ1,
              ")"])) typ)

-- | Show a type scheme as a string
typeScheme :: (Core.TypeScheme -> String)
typeScheme ts =  
  let vars = (Core.typeSchemeVariables ts)
  in  
    let body = (Core.typeSchemeType ts)
    in  
      let varNames = (Lists.map Core.unName vars)
      in  
        let fa = (Logic.ifElse (Lists.null vars) "" (Strings.cat [
                "\8704[",
                Strings.intercalate "," varNames,
                "]."]))
        in (Strings.cat [
          "(",
          fa,
          type_ body,
          ")"])
