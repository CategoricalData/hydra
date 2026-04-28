-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.core types

module Hydra.Show.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Show a binding as a string
binding :: Core.Binding -> String
binding el =

      let name = Core.unName (Core.bindingName el)
          t = Core.bindingTerm el
          typeStr =
                  Maybes.maybe "" (\ts -> Strings.cat [
                    ":(",
                    (typeScheme ts),
                    ")"]) (Core.bindingTypeScheme el)
      in (Strings.cat [
        name,
        typeStr,
        " = ",
        (term t)])
-- | Show a case statement as a string
caseStatement :: Core.CaseStatement -> String
caseStatement cs =

      let tname = Core.unName (Core.caseStatementTypeName cs)
          mdef = Core.caseStatementDefault cs
          csCases = Core.caseStatementCases cs
          defaultField =
                  Maybes.maybe [] (\d -> [
                    Core.Field {
                      Core.fieldName = (Core.Name "[default]"),
                      Core.fieldTerm = d}]) mdef
          allFields =
                  Lists.concat [
                    csCases,
                    defaultField]
      in (Strings.cat [
        "case(",
        tname,
        ")",
        (fields allFields)])
-- | Show an Either value using given functions for left and right
either :: (t0 -> String) -> (t1 -> String) -> Either t0 t1 -> String
either showA showB e =
    Eithers.either (\a -> Strings.cat2 "left(" (Strings.cat2 (showA a) ")")) (\b -> Strings.cat2 "right(" (Strings.cat2 (showB b) ")")) e
field :: Core.Field -> String
field field =

      let fname = Core.unName (Core.fieldName field)
          fterm = Core.fieldTerm field
      in (Strings.cat [
        fname,
        "=",
        (term fterm)])
fieldType :: Core.FieldType -> String
fieldType ft =

      let fname = Core.unName (Core.fieldTypeName ft)
          ftyp = Core.fieldTypeType ft
      in (Strings.cat [
        fname,
        ":",
        (type_ ftyp)])
-- | Show a list of fields as a string
fields :: [Core.Field] -> String
fields flds =

      let fieldStrs = Lists.map field flds
      in (Strings.cat [
        "{",
        (Strings.intercalate ", " fieldStrs),
        "}"])
-- | Show a float value as a string
float :: Core.FloatValue -> String
float fv =
    case fv of
      Core.FloatValueBigfloat v0 -> Strings.cat2 (Literals.showBigfloat v0) ":bigfloat"
      Core.FloatValueFloat32 v0 -> Strings.cat2 (Literals.showFloat32 v0) ":float32"
      Core.FloatValueFloat64 v0 -> Strings.cat2 (Literals.showFloat64 v0) ":float64"
-- | Show a float type as a string
floatType :: Core.FloatType -> String
floatType ft =
    case ft of
      Core.FloatTypeBigfloat -> "bigfloat"
      Core.FloatTypeFloat32 -> "float32"
      Core.FloatTypeFloat64 -> "float64"
-- | Show an injection as a string
injection :: Core.Injection -> String
injection inj =

      let tname = Core.injectionTypeName inj
          f = Core.injectionField inj
      in (Strings.cat [
        "inject(",
        (Core.unName tname),
        ")",
        (fields [
          f])])
-- | Show an integer value as a string
integer :: Core.IntegerValue -> String
integer iv =
    case iv of
      Core.IntegerValueBigint v0 -> Strings.cat2 (Literals.showBigint v0) ":bigint"
      Core.IntegerValueInt8 v0 -> Strings.cat2 (Literals.showInt8 v0) ":int8"
      Core.IntegerValueInt16 v0 -> Strings.cat2 (Literals.showInt16 v0) ":int16"
      Core.IntegerValueInt32 v0 -> Strings.cat2 (Literals.showInt32 v0) ":int32"
      Core.IntegerValueInt64 v0 -> Strings.cat2 (Literals.showInt64 v0) ":int64"
      Core.IntegerValueUint8 v0 -> Strings.cat2 (Literals.showUint8 v0) ":uint8"
      Core.IntegerValueUint16 v0 -> Strings.cat2 (Literals.showUint16 v0) ":uint16"
      Core.IntegerValueUint32 v0 -> Strings.cat2 (Literals.showUint32 v0) ":uint32"
      Core.IntegerValueUint64 v0 -> Strings.cat2 (Literals.showUint64 v0) ":uint64"
-- | Show an integer type as a string
integerType :: Core.IntegerType -> String
integerType it =
    case it of
      Core.IntegerTypeBigint -> "bigint"
      Core.IntegerTypeInt8 -> "int8"
      Core.IntegerTypeInt16 -> "int16"
      Core.IntegerTypeInt32 -> "int32"
      Core.IntegerTypeInt64 -> "int64"
      Core.IntegerTypeUint8 -> "uint8"
      Core.IntegerTypeUint16 -> "uint16"
      Core.IntegerTypeUint32 -> "uint32"
      Core.IntegerTypeUint64 -> "uint64"
-- | Show a lambda as a string
lambda :: Core.Lambda -> String
lambda l =

      let v = Core.unName (Core.lambdaParameter l)
          mt = Core.lambdaDomain l
          body = Core.lambdaBody l
          typeStr = Maybes.maybe "" (\t -> Strings.cat2 ":" (type_ t)) mt
      in (Strings.cat [
        "\955",
        v,
        typeStr,
        ".",
        (term body)])
-- | Show a let expression as a string
let_ :: Core.Let -> String
let_ l =

      let bindings = Core.letBindings l
          env = Core.letBody l
          bindingStrs = Lists.map binding bindings
      in (Strings.cat [
        "let ",
        (Strings.intercalate ", " bindingStrs),
        " in ",
        (term env)])
-- | Show a list using a given function to show each element
list :: (t0 -> String) -> [t0] -> String
list f xs =

      let elementStrs = Lists.map f xs
      in (Strings.cat [
        "[",
        (Strings.intercalate ", " elementStrs),
        "]"])
-- | Show a literal as a string
literal :: Core.Literal -> String
literal l =
    case l of
      Core.LiteralBinary _ -> "[binary]"
      Core.LiteralBoolean v0 -> Logic.ifElse v0 "true" "false"
      Core.LiteralDecimal v0 -> Literals.showDecimal v0
      Core.LiteralFloat v0 -> float v0
      Core.LiteralInteger v0 -> integer v0
      Core.LiteralString v0 -> Literals.showString v0
-- | Show a literal type as a string
literalType :: Core.LiteralType -> String
literalType lt =
    case lt of
      Core.LiteralTypeBinary -> "binary"
      Core.LiteralTypeBoolean -> "boolean"
      Core.LiteralTypeDecimal -> "decimal"
      Core.LiteralTypeFloat v0 -> floatType v0
      Core.LiteralTypeInteger v0 -> integerType v0
      Core.LiteralTypeString -> "string"
-- | Show a map using given functions to show keys and values
map :: Ord t0 => ((t0 -> String) -> (t1 -> String) -> M.Map t0 t1 -> String)
map showK showV m =

      let pairStrs =
              Lists.map (\p -> Strings.cat [
                showK (Pairs.first p),
                ": ",
                (showV (Pairs.second p))]) (Maps.toList m)
      in (Strings.cat [
        "{",
        (Strings.intercalate ", " pairStrs),
        "}"])
-- | Show a Maybe value using a given function to show the element
maybe :: (t0 -> String) -> Maybe t0 -> String
maybe f mx = Maybes.maybe "nothing" (\x -> Strings.cat2 "just(" (Strings.cat2 (f x) ")")) mx
-- | Show a pair using given functions to show each element
pair :: (t0 -> String) -> (t1 -> String) -> (t0, t1) -> String
pair showA showB p =
    Strings.cat [
      "(",
      (showA (Pairs.first p)),
      ", ",
      (showB (Pairs.second p)),
      ")"]
-- | Show a projection as a string
projection :: Core.Projection -> String
projection proj =

      let tname = Core.unName (Core.projectionTypeName proj)
          fname = Core.unName (Core.projectionField proj)
      in (Strings.cat [
        "project(",
        tname,
        "){",
        fname,
        "}"])
-- | A placeholder for reading terms from their serialized form. Not implemented.
readTerm :: String -> Maybe Core.Term
readTerm s = Just (Core.TermLiteral (Core.LiteralString s))
-- | Show a set using a given function to show each element
set :: Ord t0 => ((t0 -> String) -> S.Set t0 -> String)
set f xs =

      let elementStrs = Lists.map f (Sets.toList xs)
      in (Strings.cat [
        "{",
        (Strings.intercalate ", " elementStrs),
        "}"])
-- | Show a term as a string
term :: Core.Term -> String
term t =

      let gatherTerms =
              \prev -> \app ->
                let lhs = Core.applicationFunction app
                    rhs = Core.applicationArgument app
                in case lhs of
                  Core.TermApplication v0 -> gatherTerms (Lists.cons rhs prev) v0
                  _ -> Lists.cons lhs (Lists.cons rhs prev)
      in case t of
        Core.TermAnnotated v0 -> term (Core.annotatedTermBody v0)
        Core.TermApplication v0 ->
          let terms = gatherTerms [] v0
              termStrs = Lists.map term terms
          in (Strings.cat [
            "(",
            (Strings.intercalate " @ " termStrs),
            ")"])
        Core.TermCases v0 -> caseStatement v0
        Core.TermEither v0 -> Eithers.either (\l -> Strings.cat [
          "left(",
          (term l),
          ")"]) (\r -> Strings.cat [
          "right(",
          (term r),
          ")"]) v0
        Core.TermLambda v0 -> lambda v0
        Core.TermLet v0 -> let_ v0
        Core.TermList v0 ->
          let termStrs = Lists.map term v0
          in (Strings.cat [
            "[",
            (Strings.intercalate ", " termStrs),
            "]"])
        Core.TermLiteral v0 -> literal v0
        Core.TermMap v0 ->
          let entry =
                  \p -> Strings.cat [
                    term (Pairs.first p),
                    "=",
                    (term (Pairs.second p))]
          in (Strings.cat [
            "{",
            (Strings.intercalate ", " (Lists.map entry (Maps.toList v0))),
            "}"])
        Core.TermMaybe v0 -> Maybes.maybe "nothing" (\t2 -> Strings.cat [
          "just(",
          (term t2),
          ")"]) v0
        Core.TermPair v0 -> Strings.cat [
          "(",
          (term (Pairs.first v0)),
          ", ",
          (term (Pairs.second v0)),
          ")"]
        Core.TermProject v0 -> projection v0
        Core.TermRecord v0 ->
          let tname = Core.unName (Core.recordTypeName v0)
              flds = Core.recordFields v0
          in (Strings.cat [
            "record(",
            tname,
            ")",
            (fields flds)])
        Core.TermSet v0 -> Strings.cat [
          "{",
          (Strings.intercalate ", " (Lists.map term (Sets.toList v0))),
          "}"]
        Core.TermTypeLambda v0 ->
          let param = Core.unName (Core.typeLambdaParameter v0)
              body = Core.typeLambdaBody v0
          in (Strings.cat [
            "\923",
            param,
            ".",
            (term body)])
        Core.TermTypeApplication v0 ->
          let t2 = Core.typeApplicationTermBody v0
              typ = Core.typeApplicationTermType v0
          in (Strings.cat [
            term t2,
            "\10216",
            (type_ typ),
            "\10217"])
        Core.TermInject v0 -> injection v0
        Core.TermUnit -> "unit"
        Core.TermUnwrap v0 -> Strings.cat [
          "unwrap(",
          (Core.unName v0),
          ")"]
        Core.TermVariable v0 -> Core.unName v0
        Core.TermWrap v0 ->
          let tname = Core.unName (Core.wrappedTermTypeName v0)
              term1 = Core.wrappedTermBody v0
          in (Strings.cat [
            "wrap(",
            tname,
            "){",
            (term term1),
            "}"])
-- | Show a type as a string
type_ :: Core.Type -> String
type_ typ =

      let showRowType =
              \flds ->
                let fieldStrs = Lists.map fieldType flds
                in (Strings.cat [
                  "{",
                  (Strings.intercalate ", " fieldStrs),
                  "}"])
          gatherTypes =
                  \prev -> \app ->
                    let lhs = Core.applicationTypeFunction app
                        rhs = Core.applicationTypeArgument app
                    in case lhs of
                      Core.TypeApplication v0 -> gatherTypes (Lists.cons rhs prev) v0
                      _ -> Lists.cons lhs (Lists.cons rhs prev)
          gatherFunctionTypes =
                  \prev -> \t -> case t of
                    Core.TypeFunction v0 ->
                      let dom = Core.functionTypeDomain v0
                          cod = Core.functionTypeCodomain v0
                      in (gatherFunctionTypes (Lists.cons dom prev) cod)
                    _ -> Lists.reverse (Lists.cons t prev)
      in case typ of
        Core.TypeAnnotated v0 -> type_ (Core.annotatedTypeBody v0)
        Core.TypeApplication v0 ->
          let types = gatherTypes [] v0
              typeStrs = Lists.map type_ types
          in (Strings.cat [
            "(",
            (Strings.intercalate " @ " typeStrs),
            ")"])
        Core.TypeEither v0 ->
          let leftTyp = Core.eitherTypeLeft v0
              rightTyp = Core.eitherTypeRight v0
          in (Strings.cat [
            "either<",
            (type_ leftTyp),
            ", ",
            (type_ rightTyp),
            ">"])
        Core.TypeForall v0 ->
          let var = Core.unName (Core.forallTypeParameter v0)
              body = Core.forallTypeBody v0
          in (Strings.cat [
            "(\8704",
            var,
            ".",
            (type_ body),
            ")"])
        Core.TypeFunction _ ->
          let types = gatherFunctionTypes [] typ
              typeStrs = Lists.map type_ types
          in (Strings.cat [
            "(",
            (Strings.intercalate " \8594 " typeStrs),
            ")"])
        Core.TypeList v0 -> Strings.cat [
          "list<",
          (type_ v0),
          ">"]
        Core.TypeLiteral v0 -> literalType v0
        Core.TypeMap v0 ->
          let keyTyp = Core.mapTypeKeys v0
              valTyp = Core.mapTypeValues v0
          in (Strings.cat [
            "map<",
            (type_ keyTyp),
            ", ",
            (type_ valTyp),
            ">"])
        Core.TypeMaybe v0 -> Strings.cat [
          "maybe<",
          (type_ v0),
          ">"]
        Core.TypePair v0 ->
          let firstTyp = Core.pairTypeFirst v0
              secondTyp = Core.pairTypeSecond v0
          in (Strings.cat [
            "(",
            (type_ firstTyp),
            ", ",
            (type_ secondTyp),
            ")"])
        Core.TypeRecord v0 -> Strings.cat2 "record" (showRowType v0)
        Core.TypeSet v0 -> Strings.cat [
          "set<",
          (type_ v0),
          ">"]
        Core.TypeUnion v0 -> Strings.cat2 "union" (showRowType v0)
        Core.TypeUnit -> "unit"
        Core.TypeVariable v0 -> Core.unName v0
        Core.TypeVoid -> "void"
        Core.TypeWrap v0 -> Strings.cat [
          "wrap(",
          (type_ v0),
          ")"]
-- | Show a type scheme as a string
typeScheme :: Core.TypeScheme -> String
typeScheme ts =

      let vars = Core.typeSchemeVariables ts
          body = Core.typeSchemeBody ts
          varNames = Lists.map Core.unName vars
          fa =
                  Logic.ifElse (Lists.null vars) "" (Strings.cat [
                    "forall ",
                    (Strings.intercalate "," varNames),
                    ". "])
          toConstraintPair =
                  \v -> \c -> Strings.cat [
                    Core.unName c,
                    " ",
                    (Core.unName v)]
          toConstraintPairs =
                  \p -> Lists.map (toConstraintPair (Pairs.first p)) (Sets.toList (Core.typeVariableMetadataClasses (Pairs.second p)))
          tc = Maybes.maybe [] (\m -> Lists.concat (Lists.map toConstraintPairs (Maps.toList m))) (Core.typeSchemeConstraints ts)
      in (Strings.cat [
        "(",
        fa,
        (Logic.ifElse (Lists.null tc) "" (Strings.cat [
          "(",
          (Strings.intercalate ", " tc),
          ") => "])),
        (type_ body),
        ")"])
