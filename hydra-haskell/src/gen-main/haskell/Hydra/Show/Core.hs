-- | Haskell implementations of hydra.lib.io primitives

module Hydra.Show.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Strip as Strip
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

readTerm :: (t0 -> Maybe t1)
readTerm _ = Nothing

-- | Show an element as a string
showElement :: (Graph.Element -> String)
showElement el =  
  let name = (Core.unName (Graph.elementName el)) 
      term = (Graph.elementTerm el)
      typeStr = (Optionals.maybe "" (\ts -> Strings.cat2 " : " (showTypeScheme ts)) (Graph.elementType el))
  in (Strings.cat [
    name,
    " = ",
    showTerm term,
    typeStr])

-- | Show a float value as a string
showFloat :: (Core.FloatValue -> String)
showFloat fv = ((\x -> case x of
  Core.FloatValueBigfloat v1 -> (Literals.showBigfloat v1)
  Core.FloatValueFloat32 v1 -> (Literals.showFloat32 v1)
  Core.FloatValueFloat64 v1 -> (Literals.showFloat64 v1)) fv)

-- | Show a float type as a string
showFloatType :: (Core.FloatType -> String)
showFloatType ft = ((\x -> case x of
  Core.FloatTypeBigfloat -> "bigfloat"
  Core.FloatTypeFloat32 -> "float32"
  Core.FloatTypeFloat64 -> "float64") ft)

-- | Show a graph as a string
showGraph :: (Graph.Graph -> String)
showGraph graph =  
  let elements = (Maps.elems (Graph.graphElements graph)) 
      elementStrs = (Lists.map showElement elements)
  in (Strings.cat [
    "{",
    Strings.intercalate ", " elementStrs,
    "}"])

-- | Show an integer value as a string
showInteger :: (Core.IntegerValue -> String)
showInteger iv = ((\x -> case x of
  Core.IntegerValueBigint v1 -> (Literals.showBigint v1)
  Core.IntegerValueInt8 v1 -> (Literals.showInt8 v1)
  Core.IntegerValueInt16 v1 -> (Literals.showInt16 v1)
  Core.IntegerValueInt32 v1 -> (Literals.showInt32 v1)
  Core.IntegerValueInt64 v1 -> (Literals.showInt64 v1)
  Core.IntegerValueUint8 v1 -> (Literals.showUint8 v1)
  Core.IntegerValueUint16 v1 -> (Literals.showUint16 v1)
  Core.IntegerValueUint32 v1 -> (Literals.showUint32 v1)
  Core.IntegerValueUint64 v1 -> (Literals.showUint64 v1)) iv)

-- | Show an integer type as a string
showIntegerType :: (Core.IntegerType -> String)
showIntegerType it = ((\x -> case x of
  Core.IntegerTypeBigint -> "bigint"
  Core.IntegerTypeInt8 -> "int8"
  Core.IntegerTypeInt16 -> "int16"
  Core.IntegerTypeInt32 -> "int32"
  Core.IntegerTypeInt64 -> "int64"
  Core.IntegerTypeUint8 -> "uint8"
  Core.IntegerTypeUint16 -> "uint16"
  Core.IntegerTypeUint32 -> "uint32"
  Core.IntegerTypeUint64 -> "uint64") it)

showList :: ((t0 -> String) -> [t0] -> String)
showList f xs =  
  let elementStrs = (Lists.map f xs)
  in (Strings.cat [
    "[",
    Strings.intercalate ", " elementStrs,
    "]"])

-- | Show a literal as a string
showLiteral :: (Core.Literal -> String)
showLiteral l = ((\x -> case x of
  Core.LiteralBinary _ -> "[binary]"
  Core.LiteralBoolean v1 -> (Logic.ifElse v1 "true" "false")
  Core.LiteralFloat v1 -> (showFloat v1)
  Core.LiteralInteger v1 -> (showInteger v1)
  Core.LiteralString v1 -> (Literals.showString v1)) l)

-- | Show a literal type as a string
showLiteralType :: (Core.LiteralType -> String)
showLiteralType lt = ((\x -> case x of
  Core.LiteralTypeBinary -> "binary"
  Core.LiteralTypeBoolean -> "boolean"
  Core.LiteralTypeFloat v1 -> (showFloatType v1)
  Core.LiteralTypeInteger v1 -> (showIntegerType v1)
  Core.LiteralTypeString -> "string") lt)

-- | Show a term as a string
showTerm :: (Core.Term -> String)
showTerm term =  
  let showField = (\field ->  
          let fname = (Core.unName (Core.fieldName field)) 
              fterm = (Core.fieldTerm field)
          in (Strings.cat2 fname (Strings.cat2 "=" (showTerm fterm)))) 
      showFields = (\fields ->  
              let fieldStrs = (Lists.map showField fields)
              in (Strings.cat [
                "{",
                Strings.intercalate ", " fieldStrs,
                "}"]))
      gatherTerms = (\prev -> \app ->  
              let lhs = (Core.applicationFunction app) 
                  rhs = (Core.applicationArgument app)
                  strippedLhs = (Strip.stripTerm lhs)
              in ((\x -> case x of
                Core.TermApplication v1 -> (gatherTerms (Lists.cons rhs prev) v1)
                _ -> (Lists.cons strippedLhs (Lists.cons rhs prev))) strippedLhs))
      showBinding = (\binding ->  
              let v = (Core.unName (Core.letBindingName binding)) 
                  bindingTerm = (Core.letBindingTerm binding)
                  typeStr = (Optionals.maybe "" (\ts -> Strings.cat2 ":" (showTypeScheme ts)) (Core.letBindingType binding))
              in (Strings.cat [
                v,
                "=",
                showTerm bindingTerm,
                typeStr]))
  in ((\x -> case x of
    Core.TermApplication v1 ->  
      let terms = (gatherTerms [] v1) 
          termStrs = (Lists.map showTerm terms)
      in (Strings.cat [
        "(",
        Strings.intercalate " @ " termStrs,
        ")"])
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionElimination v2 -> ((\x -> case x of
        Core.EliminationRecord v3 ->  
          let tname = (Core.unName (Core.projectionTypeName v3)) 
              fname = (Core.unName (Core.projectionField v3))
          in (Strings.cat [
            "project(",
            tname,
            "){",
            fname,
            "}"])
        Core.EliminationUnion v3 ->  
          let tname = (Core.unName (Core.caseStatementTypeName v3)) 
              mdef = (Core.caseStatementDefault v3)
              cases = (Core.caseStatementCases v3)
              defaultField = (Optionals.maybe [] (\d -> [
                      Core.Field {
                        Core.fieldName = (Core.Name "[default]"),
                        Core.fieldTerm = d}]) mdef)
              allFields = (Lists.concat [
                      cases,
                      defaultField])
          in (Strings.cat [
            "case(",
            tname,
            ")",
            (showFields allFields)])
        Core.EliminationWrap v3 -> (Strings.cat [
          "unwrap(",
          Core.unName v3,
          ")"])) v2)
      Core.FunctionLambda v2 ->  
        let v = (Core.unName (Core.lambdaParameter v2)) 
            mt = (Core.lambdaDomain v2)
            body = (Core.lambdaBody v2)
            typeStr = (Optionals.maybe "" (\t -> Strings.cat2 ":" (showType t)) mt)
        in (Strings.cat [
          "\955",
          v,
          typeStr,
          ".",
          (showTerm body)])
      Core.FunctionPrimitive v2 -> (Strings.cat2 (Core.unName v2) "!")) v1)
    Core.TermLet v1 ->  
      let bindings = (Core.letBindings v1) 
          env = (Core.letEnvironment v1)
          bindingStrs = (Lists.map showBinding bindings)
      in (Strings.cat [
        "let ",
        Strings.intercalate ", " bindingStrs,
        " in ",
        (showTerm env)])
    Core.TermList v1 ->  
      let termStrs = (Lists.map showTerm v1)
      in (Strings.cat [
        "[",
        Strings.intercalate ", " termStrs,
        "]"])
    Core.TermLiteral v1 -> (showLiteral v1)
    Core.TermOptional v1 -> (Optionals.maybe "nothing" (\t -> Strings.cat [
      "just(",
      showTerm t,
      ")"]) v1)
    Core.TermProduct v1 ->  
      let termStrs = (Lists.map showTerm v1)
      in (Strings.cat [
        "(",
        Strings.intercalate ", " termStrs,
        ")"])
    Core.TermRecord v1 ->  
      let tname = (Core.unName (Core.recordTypeName v1)) 
          fields = (Core.recordFields v1)
      in (Strings.cat [
        "record(",
        tname,
        ")",
        (showFields fields)])
    Core.TermTypeAbstraction v1 ->  
      let param = (Core.unName (Core.typeAbstractionParameter v1)) 
          body = (Core.typeAbstractionBody v1)
      in (Strings.cat [
        "\923",
        param,
        ".",
        (showTerm body)])
    Core.TermTypeApplication v1 ->  
      let term = (Core.typedTermTerm v1) 
          typ = (Core.typedTermType v1)
      in (Strings.cat [
        showTerm term,
        "\10216",
        showType typ,
        "\10217"])
    Core.TermTyped v1 ->  
      let term = (Core.typedTermTerm v1) 
          typ = (Core.typedTermType v1)
      in (Strings.cat [
        "(",
        showTerm term,
        " : ",
        showType typ,
        ")"])
    Core.TermUnion v1 ->  
      let tname = (Core.unName (Core.injectionTypeName v1)) 
          f = (Core.injectionField v1)
      in (Strings.cat [
        "inject(",
        tname,
        ")",
        (showFields [
          f])])
    Core.TermVariable v1 -> (Core.unName v1)
    Core.TermWrap v1 ->  
      let tname = (Core.unName (Core.wrappedTermTypeName v1)) 
          term1 = (Core.wrappedTermObject v1)
      in (Strings.cat [
        "wrap(",
        tname,
        "){",
        showTerm term1,
        "}"])) (Strip.stripTerm term))

-- | Show a type as a string
showType :: (Core.Type -> String)
showType typ =  
  let showFieldType = (\ft ->  
          let fname = (Core.unName (Core.fieldTypeName ft)) 
              ftyp = (Core.fieldTypeType ft)
          in (Strings.cat [
            fname,
            " = ",
            (showType ftyp)])) 
      showRowType = (\rt ->  
              let fields = (Core.rowTypeFields rt) 
                  fieldStrs = (Lists.map showFieldType fields)
              in (Strings.cat [
                "{",
                Strings.intercalate ", " fieldStrs,
                "}"]))
      gatherTypes = (\prev -> \app ->  
              let lhs = (Core.applicationTypeFunction app) 
                  rhs = (Core.applicationTypeArgument app)
                  strippedLhs = (Strip.stripType lhs)
              in ((\x -> case x of
                Core.TypeApplication v1 -> (gatherTypes (Lists.cons rhs prev) v1)
                _ -> (Lists.cons strippedLhs (Lists.cons rhs prev))) strippedLhs))
      gatherFunctionTypes = (\prev -> \t -> (\x -> case x of
              Core.TypeFunction v1 ->  
                let dom = (Core.functionTypeDomain v1) 
                    cod = (Core.functionTypeCodomain v1)
                in (gatherFunctionTypes (Lists.cons dom prev) cod)
              _ -> (Lists.reverse (Lists.cons t prev))) (Strip.stripType t))
  in ((\x -> case x of
    Core.TypeApplication v1 ->  
      let types = (gatherTypes [] v1) 
          typeStrs = (Lists.map showType types)
      in (Strings.cat [
        "(",
        Strings.intercalate " @ " typeStrs,
        ")"])
    Core.TypeFunction _ ->  
      let types = (gatherFunctionTypes [] typ) 
          typeStrs = (Lists.map showType types)
      in (Strings.cat [
        "(",
        Strings.intercalate " \8594 " typeStrs,
        ")"])
    Core.TypeList v1 -> (Strings.cat [
      "list<",
      showType v1,
      ">"])
    Core.TypeForall v1 ->  
      let var = (Core.unName (Core.forallTypeParameter v1)) 
          body = (Core.forallTypeBody v1)
      in (Strings.cat [
        "(\8704",
        var,
        ".",
        showType body,
        ")"])
    Core.TypeLiteral v1 -> (showLiteralType v1)
    Core.TypeMap v1 ->  
      let keyTyp = (Core.mapTypeKeys v1) 
          valTyp = (Core.mapTypeValues v1)
      in (Strings.cat [
        "map<",
        showType keyTyp,
        ", ",
        showType valTyp,
        ">"])
    Core.TypeOptional v1 -> (Strings.cat [
      "optional<",
      showType v1,
      ">"])
    Core.TypeProduct v1 ->  
      let typeStrs = (Lists.map showType v1)
      in (Strings.intercalate "\215" typeStrs)
    Core.TypeRecord v1 -> (Strings.cat2 "record" (showRowType v1))
    Core.TypeSet v1 -> (Strings.cat [
      "set<",
      showType v1,
      ">"])
    Core.TypeUnion v1 -> (Strings.cat2 "union" (showRowType v1))
    Core.TypeVariable v1 -> (Core.unName v1)
    Core.TypeWrap v1 ->  
      let tname = (Core.unName (Core.wrappedTypeTypeName v1)) 
          typ1 = (Core.wrappedTypeObject v1)
      in (Strings.cat [
        "wrap[",
        tname,
        "](",
        showType typ1,
        ")"])) (Strip.stripType typ))

-- | Show a type constraint as a string
showTypeConstraint :: (Typing.TypeConstraint -> String)
showTypeConstraint tc =  
  let ltyp = (Typing.typeConstraintLeft tc) 
      rtyp = (Typing.typeConstraintRight tc)
  in (Strings.cat [
    showType ltyp,
    "\8801",
    (showType rtyp)])

-- | Show a type scheme as a string
showTypeScheme :: (Core.TypeScheme -> String)
showTypeScheme ts =  
  let vars = (Core.typeSchemeVariables ts) 
      body = (Core.typeSchemeType ts)
      varNames = (Lists.map Core.unName vars)
      fa = (Logic.ifElse (Lists.null vars) "" (Strings.cat [
              "\8704[",
              Strings.intercalate "," varNames,
              "]."]))
  in (Strings.cat [
    "(",
    fa,
    showType body,
    ")"])

-- | Show a type substitution as a string
showTypeSubst :: (Typing.TypeSubst -> String)
showTypeSubst ts =  
  let subst = (Typing.unTypeSubst ts) 
      pairs = (Maps.toList subst)
      showPair = (\pair ->  
              let name = (Core.unName (fst pair)) 
                  typ = (snd pair)
              in (Strings.cat [
                name,
                "\8614",
                (showType typ)]))
      pairStrs = (Lists.map showPair pairs)
  in (Strings.cat [
    "{",
    Strings.intercalate "," pairStrs,
    "}"])
