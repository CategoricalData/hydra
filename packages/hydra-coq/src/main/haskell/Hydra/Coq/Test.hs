-- | End-to-end test for the Coq coder.
-- Constructs Hydra types and terms, encodes them to Coq AST via the generated
-- DSL runtime, serializes to text via the generated Serde, and prints the result.

module Hydra.Coq.Test where

import qualified Hydra.Core as Core
import qualified Hydra.Coq.Syntax as Coq
import qualified Hydra.Coq.Coder as Coder
import qualified Hydra.Coq.Serde as Serde
import qualified Hydra.Serialization as Serialization

-- | End-to-end: Hydra types/terms -> Coq source text
generateCoq :: [(String, Core.Type)] -> [(String, Core.Term)] -> String
generateCoq typeDefs termDefs =
  let doc = Coder.moduleToCoq typeDefs termDefs
      expr = Serde.documentToExpr doc
  in Serialization.printExpr expr

-- | A sample enum type (Color with three variants)
colorType :: (String, Core.Type)
colorType = ("Color", Core.TypeUnion [
  Core.FieldType (Core.Name "red") Core.TypeUnit,
  Core.FieldType (Core.Name "green") Core.TypeUnit,
  Core.FieldType (Core.Name "blue") Core.TypeUnit])

-- | A record type (Person with name and age)
personType :: (String, Core.Type)
personType = ("Person", Core.TypeRecord [
  Core.FieldType (Core.Name "name") (Core.TypeLiteral Core.LiteralTypeString),
  Core.FieldType (Core.Name "age") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))])

-- | A wrapped/newtype (PersonId wrapping string)
personIdType :: (String, Core.Type)
personIdType = ("PersonId", Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString))

-- | A function type (nat -> bool)
isAdultType :: (String, Core.Type)
isAdultType = ("IsAdult", Core.TypeFunction (Core.FunctionType
  (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))
  (Core.TypeLiteral Core.LiteralTypeBoolean)))

-- | A polymorphic type: forall a. list a
listAliasType :: (String, Core.Type)
listAliasType = ("MyList", Core.TypeForall (Core.ForallType (Core.Name "A")
  (Core.TypeList (Core.TypeVariable (Core.Name "A")))))

-- | A term: the literal integer 42
literal42 :: (String, Core.Term)
literal42 = ("the_answer", Core.TermLiteral
  (Core.LiteralInteger (Core.IntegerValueBigint 42)))

-- | A term: the boolean true
trueVal :: (String, Core.Term)
trueVal = ("is_valid", Core.TermLiteral (Core.LiteralBoolean True))

-- | A term: a lambda  (fun x : Z => x)
identityTerm :: (String, Core.Term)
identityTerm = ("identity", Core.TermFunction (Core.FunctionLambda
  (Core.Lambda (Core.Name "x")
    (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)))
    (Core.TermVariable (Core.Name "x")))))

-- | A list term: [1, 2, 3]
sampleList :: (String, Core.Term)
sampleList = ("nums", Core.TermList [
  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1)),
  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 2)),
  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 3))])

-- | Run the end-to-end pipeline and print the result
runTest :: String
runTest = generateCoq
  [colorType, personType, personIdType, isAdultType, listAliasType]
  [literal42, trueVal, sampleList]

-- | Print the generated Coq to stdout
printTest :: IO ()
printTest = putStrLn runTest
