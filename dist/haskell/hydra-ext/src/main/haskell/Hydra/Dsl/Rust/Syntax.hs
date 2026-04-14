-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.rust.syntax

module Hydra.Dsl.Rust.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Rust.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I

angleBracketedArgs :: Phantoms.TTerm [Syntax.GenericArg] -> Phantoms.TTerm Syntax.AngleBracketedArgs
angleBracketedArgs args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.AngleBracketedArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

angleBracketedArgsArgs :: Phantoms.TTerm Syntax.AngleBracketedArgs -> Phantoms.TTerm [Syntax.GenericArg]
angleBracketedArgsArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.AngleBracketedArgs"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

angleBracketedArgsWithArgs :: Phantoms.TTerm Syntax.AngleBracketedArgs -> Phantoms.TTerm [Syntax.GenericArg] -> Phantoms.TTerm Syntax.AngleBracketedArgs
angleBracketedArgsWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.AngleBracketedArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayExprElements :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ArrayExpr
arrayExprElements x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.ArrayExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elements"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayExprRepeat :: Phantoms.TTerm Syntax.ArrayRepeat -> Phantoms.TTerm Syntax.ArrayExpr
arrayExprRepeat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.ArrayExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayRepeat :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayRepeat
arrayRepeat element length =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ArrayRepeat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm element)},
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm length)}]}))

arrayRepeatElement :: Phantoms.TTerm Syntax.ArrayRepeat -> Phantoms.TTerm Syntax.Expression
arrayRepeatElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayRepeat"),
        Core.projectionField = (Core.Name "element")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayRepeatLength :: Phantoms.TTerm Syntax.ArrayRepeat -> Phantoms.TTerm Syntax.Expression
arrayRepeatLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayRepeat"),
        Core.projectionField = (Core.Name "length")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayRepeatWithElement :: Phantoms.TTerm Syntax.ArrayRepeat -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayRepeat
arrayRepeatWithElement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ArrayRepeat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayRepeat"),
              Core.projectionField = (Core.Name "length")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayRepeatWithLength :: Phantoms.TTerm Syntax.ArrayRepeat -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayRepeat
arrayRepeatWithLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ArrayRepeat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayRepeat"),
              Core.projectionField = (Core.Name "element")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayType
arrayType element length =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm element)},
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm length)}]}))

arrayTypeElement :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Type
arrayTypeElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayType"),
        Core.projectionField = (Core.Name "element")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayTypeLength :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Expression
arrayTypeLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayType"),
        Core.projectionField = (Core.Name "length")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayTypeWithElement :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithElement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayType"),
              Core.projectionField = (Core.Name "length")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayTypeWithLength :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ArrayType"),
              Core.projectionField = (Core.Name "element")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

assignExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignExpr
assignExpr target value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.AssignExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

assignExprTarget :: Phantoms.TTerm Syntax.AssignExpr -> Phantoms.TTerm Syntax.Expression
assignExprTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.AssignExpr"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignExprValue :: Phantoms.TTerm Syntax.AssignExpr -> Phantoms.TTerm Syntax.Expression
assignExprValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.AssignExpr"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignExprWithTarget :: Phantoms.TTerm Syntax.AssignExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignExpr
assignExprWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.AssignExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.AssignExpr"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignExprWithValue :: Phantoms.TTerm Syntax.AssignExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignExpr
assignExprWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.AssignExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.AssignExpr"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

attribute :: Phantoms.TTerm Bool -> Phantoms.TTerm [String] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Attribute
attribute inner path tokens =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Phantoms.unTTerm tokens)}]}))

attributeInner :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm Bool
attributeInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
        Core.projectionField = (Core.Name "inner")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

attributePath :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm [String]
attributePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

attributeTokens :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm (Maybe String)
attributeTokens x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
        Core.projectionField = (Core.Name "tokens")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

attributeWithInner :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Attribute
attributeWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
              Core.projectionField = (Core.Name "tokens")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

attributeWithPath :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.Attribute
attributeWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
              Core.projectionField = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
              Core.projectionField = (Core.Name "tokens")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

attributeWithTokens :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Attribute
attributeWithTokens original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
              Core.projectionField = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Attribute"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryOp -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr
binaryExpr left op right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

binaryExprLeft :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression
binaryExprLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExprOp :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.BinaryOp
binaryExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExprRight :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression
binaryExprRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExprWithLeft :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr
binaryExprWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExprWithOp :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.BinaryOp -> Phantoms.TTerm Syntax.BinaryExpr
binaryExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExprWithRight :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr
binaryExprWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.BinaryExpr"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryOpAdd :: Phantoms.TTerm Syntax.BinaryOp
binaryOpAdd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpAnd :: Phantoms.TTerm Syntax.BinaryOp
binaryOpAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpBitAnd :: Phantoms.TTerm Syntax.BinaryOp
binaryOpBitAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitAnd"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpBitOr :: Phantoms.TTerm Syntax.BinaryOp
binaryOpBitOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitOr"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpBitXor :: Phantoms.TTerm Syntax.BinaryOp
binaryOpBitXor =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitXor"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpDiv :: Phantoms.TTerm Syntax.BinaryOp
binaryOpDiv =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpEq :: Phantoms.TTerm Syntax.BinaryOp
binaryOpEq =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpGe :: Phantoms.TTerm Syntax.BinaryOp
binaryOpGe =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ge"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpGt :: Phantoms.TTerm Syntax.BinaryOp
binaryOpGt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpLe :: Phantoms.TTerm Syntax.BinaryOp
binaryOpLe =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "le"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpLt :: Phantoms.TTerm Syntax.BinaryOp
binaryOpLt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpMul :: Phantoms.TTerm Syntax.BinaryOp
binaryOpMul =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mul"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpNe :: Phantoms.TTerm Syntax.BinaryOp
binaryOpNe =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ne"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpOr :: Phantoms.TTerm Syntax.BinaryOp
binaryOpOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpRem :: Phantoms.TTerm Syntax.BinaryOp
binaryOpRem =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rem"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpShl :: Phantoms.TTerm Syntax.BinaryOp
binaryOpShl =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shl"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpShr :: Phantoms.TTerm Syntax.BinaryOp
binaryOpShr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shr"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOpSub :: Phantoms.TTerm Syntax.BinaryOp
binaryOpSub =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sub"),
        Core.fieldTerm = Core.TermUnit}}))

block :: Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Block
block statements expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Block"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

blockExpression :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Expression)
blockExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Block"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockStatements :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.Statement]
blockStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Block"),
        Core.projectionField = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockWithExpression :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Block
blockWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Block"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Block"),
              Core.projectionField = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

blockWithStatements :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.Block
blockWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Block"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Block"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

callExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.CallExpr
callExpr function args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

callExprArgs :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm [Syntax.Expression]
callExprArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CallExpr"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

callExprFunction :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.Expression
callExprFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CallExpr"),
        Core.projectionField = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

callExprWithArgs :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.CallExpr
callExprWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CallExpr"),
              Core.projectionField = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

callExprWithFunction :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CallExpr
callExprWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CallExpr"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

castExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.CastExpr
castExpr expr type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CastExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

castExprExpr :: Phantoms.TTerm Syntax.CastExpr -> Phantoms.TTerm Syntax.Expression
castExprExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CastExpr"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExprType :: Phantoms.TTerm Syntax.CastExpr -> Phantoms.TTerm Syntax.Type
castExprType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CastExpr"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExprWithExpr :: Phantoms.TTerm Syntax.CastExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CastExpr
castExprWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CastExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CastExpr"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

castExprWithType :: Phantoms.TTerm Syntax.CastExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.CastExpr
castExprWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CastExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CastExpr"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

closureExpr :: Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.ClosureParam] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ClosureExpr
closureExpr move params returnType body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "move"),
          Core.fieldTerm = (Phantoms.unTTerm move)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

closureExprBody :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm Syntax.Expression
closureExprBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

closureExprMove :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm Bool
closureExprMove x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
        Core.projectionField = (Core.Name "move")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

closureExprParams :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm [Syntax.ClosureParam]
closureExprParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

closureExprReturnType :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm (Maybe Syntax.Type)
closureExprReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
        Core.projectionField = (Core.Name "returnType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

closureExprWithBody :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ClosureExpr
closureExprWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "move"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "move")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

closureExprWithMove :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ClosureExpr
closureExprWithMove original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "move"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

closureExprWithParams :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm [Syntax.ClosureParam] -> Phantoms.TTerm Syntax.ClosureExpr
closureExprWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "move"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "move")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

closureExprWithReturnType :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ClosureExpr
closureExprWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "move"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "move")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

closureParam :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ClosureParam
closureParam pattern type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

closureParamPattern :: Phantoms.TTerm Syntax.ClosureParam -> Phantoms.TTerm Syntax.Pattern
closureParamPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureParam"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

closureParamType :: Phantoms.TTerm Syntax.ClosureParam -> Phantoms.TTerm (Maybe Syntax.Type)
closureParamType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureParam"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

closureParamWithPattern :: Phantoms.TTerm Syntax.ClosureParam -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.ClosureParam
closureParamWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureParam"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

closureParamWithType :: Phantoms.TTerm Syntax.ClosureParam -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ClosureParam
closureParamWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ClosureParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ClosureParam"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

compoundAssignExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CompoundAssignOp -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CompoundAssignExpr
compoundAssignExpr target op value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

compoundAssignExprOp :: Phantoms.TTerm Syntax.CompoundAssignExpr -> Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compoundAssignExprTarget :: Phantoms.TTerm Syntax.CompoundAssignExpr -> Phantoms.TTerm Syntax.Expression
compoundAssignExprTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compoundAssignExprValue :: Phantoms.TTerm Syntax.CompoundAssignExpr -> Phantoms.TTerm Syntax.Expression
compoundAssignExprValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compoundAssignExprWithOp :: Phantoms.TTerm Syntax.CompoundAssignExpr -> Phantoms.TTerm Syntax.CompoundAssignOp -> Phantoms.TTerm Syntax.CompoundAssignExpr
compoundAssignExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

compoundAssignExprWithTarget :: Phantoms.TTerm Syntax.CompoundAssignExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CompoundAssignExpr
compoundAssignExprWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

compoundAssignExprWithValue :: Phantoms.TTerm Syntax.CompoundAssignExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CompoundAssignExpr
compoundAssignExprWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignExpr"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

compoundAssignOpAddAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpAddAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpBitAndAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpBitAndAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitAndAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpBitOrAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpBitOrAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitOrAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpBitXorAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpBitXorAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitXorAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpDivAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpDivAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpMulAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpMulAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mulAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpRemAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpRemAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "remAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpShlAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpShlAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shlAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpShrAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpShrAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shrAssign"),
        Core.fieldTerm = Core.TermUnit}}))

compoundAssignOpSubAssign :: Phantoms.TTerm Syntax.CompoundAssignOp
compoundAssignOpSubAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.CompoundAssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subAssign"),
        Core.fieldTerm = Core.TermUnit}}))

constDef :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ConstDef
constDef name type_ value public doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

constDefDoc :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm (Maybe String)
constDefDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constDefName :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm String
constDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constDefPublic :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Bool
constDefPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constDefType :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Syntax.Type
constDefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constDefValue :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Syntax.Expression
constDefValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constDefWithDoc :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ConstDef
constDefWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constDefWithName :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ConstDef
constDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constDefWithPublic :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ConstDef
constDefWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constDefWithType :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ConstDef
constDefWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constDefWithValue :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConstDef
constDefWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ConstDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

crate :: Phantoms.TTerm [Syntax.ItemWithComments] -> Phantoms.TTerm Syntax.Crate
crate items =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Crate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm items)}]}))

crateItems :: Phantoms.TTerm Syntax.Crate -> Phantoms.TTerm [Syntax.ItemWithComments]
crateItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Crate"),
        Core.projectionField = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

crateWithItems :: Phantoms.TTerm Syntax.Crate -> Phantoms.TTerm [Syntax.ItemWithComments] -> Phantoms.TTerm Syntax.Crate
crateWithItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Crate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumDef :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm [Syntax.EnumVariant] -> Phantoms.TTerm [String] -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.EnumDef
enumDef name generics whereClause variants derives public doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm whereClause)},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Phantoms.unTTerm variants)},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Phantoms.unTTerm derives)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

enumDefDerives :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm [String]
enumDefDerives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
        Core.projectionField = (Core.Name "derives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefDoc :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm (Maybe String)
enumDefDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefGenerics :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm [Syntax.GenericParam]
enumDefGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefName :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm String
enumDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefPublic :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm Bool
enumDefPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefVariants :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm [Syntax.EnumVariant]
enumDefVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
        Core.projectionField = (Core.Name "variants")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefWhereClause :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm (Maybe Syntax.WhereClause)
enumDefWhereClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
        Core.projectionField = (Core.Name "whereClause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefWithDerives :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.EnumDef
enumDefWithDerives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "variants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefWithDoc :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.EnumDef
enumDefWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "variants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumDefWithGenerics :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.EnumDef
enumDefWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "variants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefWithName :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.EnumDef
enumDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "variants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefWithPublic :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.EnumDef
enumDefWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "variants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefWithVariants :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm [Syntax.EnumVariant] -> Phantoms.TTerm Syntax.EnumDef
enumDefWithVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefWithWhereClause :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.EnumDef
enumDefWithWhereClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "variants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumVariant :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.EnumVariantBody -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.EnumVariant
enumVariant name body doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

enumVariantBody :: Phantoms.TTerm Syntax.EnumVariant -> Phantoms.TTerm Syntax.EnumVariantBody
enumVariantBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumVariantBodyStruct :: Phantoms.TTerm [Syntax.StructField] -> Phantoms.TTerm Syntax.EnumVariantBody
enumVariantBodyStruct x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariantBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumVariantBodyTuple :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.EnumVariantBody
enumVariantBodyTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariantBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumVariantBodyUnit :: Phantoms.TTerm Syntax.EnumVariantBody
enumVariantBodyUnit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariantBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

enumVariantDoc :: Phantoms.TTerm Syntax.EnumVariant -> Phantoms.TTerm (Maybe String)
enumVariantDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumVariantName :: Phantoms.TTerm Syntax.EnumVariant -> Phantoms.TTerm String
enumVariantName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumVariantWithBody :: Phantoms.TTerm Syntax.EnumVariant -> Phantoms.TTerm Syntax.EnumVariantBody -> Phantoms.TTerm Syntax.EnumVariant
enumVariantWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumVariantWithDoc :: Phantoms.TTerm Syntax.EnumVariant -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.EnumVariant
enumVariantWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumVariantWithName :: Phantoms.TTerm Syntax.EnumVariant -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.EnumVariant
enumVariantWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.EnumVariant"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exprPath :: Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.PathSegment] -> Phantoms.TTerm Syntax.ExprPath
exprPath global segments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ExprPath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "global"),
          Core.fieldTerm = (Phantoms.unTTerm global)},
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Phantoms.unTTerm segments)}]}))

exprPathGlobal :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm Bool
exprPathGlobal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ExprPath"),
        Core.projectionField = (Core.Name "global")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exprPathSegments :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm [Syntax.PathSegment]
exprPathSegments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ExprPath"),
        Core.projectionField = (Core.Name "segments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exprPathWithGlobal :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ExprPath
exprPathWithGlobal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ExprPath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "global"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ExprPath"),
              Core.projectionField = (Core.Name "segments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exprPathWithSegments :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm [Syntax.PathSegment] -> Phantoms.TTerm Syntax.ExprPath
exprPathWithSegments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ExprPath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "global"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ExprPath"),
              Core.projectionField = (Core.Name "global")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

expressionArray :: Phantoms.TTerm Syntax.ArrayExpr -> Phantoms.TTerm Syntax.Expression
expressionArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionAssign :: Phantoms.TTerm Syntax.AssignExpr -> Phantoms.TTerm Syntax.Expression
expressionAssign x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionAwait :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression
expressionAwait x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "await"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBinary :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression
expressionBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Expression
expressionBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBreak :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Expression
expressionBreak x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCall :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.Expression
expressionCall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCast :: Phantoms.TTerm Syntax.CastExpr -> Phantoms.TTerm Syntax.Expression
expressionCast x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionClosure :: Phantoms.TTerm Syntax.ClosureExpr -> Phantoms.TTerm Syntax.Expression
expressionClosure x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "closure"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCompoundAssign :: Phantoms.TTerm Syntax.CompoundAssignExpr -> Phantoms.TTerm Syntax.Expression
expressionCompoundAssign x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compoundAssign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionContinue :: Phantoms.TTerm Syntax.Expression
expressionContinue =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = Core.TermUnit}}))

expressionDereference :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression
expressionDereference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dereference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionFieldAccess :: Phantoms.TTerm Syntax.FieldAccessExpr -> Phantoms.TTerm Syntax.Expression
expressionFieldAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionFor :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm Syntax.Expression
expressionFor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionIf :: Phantoms.TTerm Syntax.IfExpr -> Phantoms.TTerm Syntax.Expression
expressionIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionIndex :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression
expressionIndex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLoop :: Phantoms.TTerm Syntax.LoopExpr -> Phantoms.TTerm Syntax.Expression
expressionLoop x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "loop"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionMacro :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm Syntax.Expression
expressionMacro x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionMatch :: Phantoms.TTerm Syntax.MatchExpr -> Phantoms.TTerm Syntax.Expression
expressionMatch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionMethodCall :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm Syntax.Expression
expressionMethodCall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodCall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionParen :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression
expressionParen x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paren"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionPath :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm Syntax.Expression
expressionPath x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionRange :: Phantoms.TTerm Syntax.RangeExpr -> Phantoms.TTerm Syntax.Expression
expressionRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionReference :: Phantoms.TTerm Syntax.RefExpr -> Phantoms.TTerm Syntax.Expression
expressionReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionReturn :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Expression
expressionReturn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionStruct :: Phantoms.TTerm Syntax.StructExpr -> Phantoms.TTerm Syntax.Expression
expressionStruct x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTry :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression
expressionTry x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTuple :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression
expressionTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTupleIndex :: Phantoms.TTerm Syntax.TupleIndexExpr -> Phantoms.TTerm Syntax.Expression
expressionTupleIndex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tupleIndex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTypeAscription :: Phantoms.TTerm Syntax.TypeAscriptionExpr -> Phantoms.TTerm Syntax.Expression
expressionTypeAscription x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAscription"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionUnary :: Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.Expression
expressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionWhile :: Phantoms.TTerm Syntax.WhileExpr -> Phantoms.TTerm Syntax.Expression
expressionWhile x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fieldAccessExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.FieldAccessExpr
fieldAccessExpr object field =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldAccessExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)}]}))

fieldAccessExprField :: Phantoms.TTerm Syntax.FieldAccessExpr -> Phantoms.TTerm String
fieldAccessExprField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldAccessExpr"),
        Core.projectionField = (Core.Name "field")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAccessExprObject :: Phantoms.TTerm Syntax.FieldAccessExpr -> Phantoms.TTerm Syntax.Expression
fieldAccessExprObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldAccessExpr"),
        Core.projectionField = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAccessExprWithField :: Phantoms.TTerm Syntax.FieldAccessExpr -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.FieldAccessExpr
fieldAccessExprWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldAccessExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldAccessExpr"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldAccessExprWithObject :: Phantoms.TTerm Syntax.FieldAccessExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldAccessExpr
fieldAccessExprWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldAccessExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldAccessExpr"),
              Core.projectionField = (Core.Name "field")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldPattern :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.FieldPattern
fieldPattern name pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

fieldPatternName :: Phantoms.TTerm Syntax.FieldPattern -> Phantoms.TTerm String
fieldPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldPattern"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldPatternPattern :: Phantoms.TTerm Syntax.FieldPattern -> Phantoms.TTerm (Maybe Syntax.Pattern)
fieldPatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldPattern"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldPatternWithName :: Phantoms.TTerm Syntax.FieldPattern -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.FieldPattern
fieldPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldPattern"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldPatternWithPattern :: Phantoms.TTerm Syntax.FieldPattern -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.FieldPattern
fieldPatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldPattern"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldValue :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FieldValue
fieldValue name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

fieldValueName :: Phantoms.TTerm Syntax.FieldValue -> Phantoms.TTerm String
fieldValueName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldValue"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldValueValue :: Phantoms.TTerm Syntax.FieldValue -> Phantoms.TTerm (Maybe Syntax.Expression)
fieldValueValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldValue"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldValueWithName :: Phantoms.TTerm Syntax.FieldValue -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.FieldValue
fieldValueWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldValue"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldValueWithValue :: Phantoms.TTerm Syntax.FieldValue -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FieldValue
fieldValueWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FieldValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FieldValue"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

floatLiteral :: Phantoms.TTerm Double -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteral value suffix =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "suffix"),
          Core.fieldTerm = (Phantoms.unTTerm suffix)}]}))

floatLiteralSuffix :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm (Maybe String)
floatLiteralSuffix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FloatLiteral"),
        Core.projectionField = (Core.Name "suffix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

floatLiteralValue :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Double
floatLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FloatLiteral"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

floatLiteralWithSuffix :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteralWithSuffix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FloatLiteral"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

floatLiteralWithValue :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "suffix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FloatLiteral"),
              Core.projectionField = (Core.Name "suffix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDef :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm [Syntax.FnParam] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FnDef
fnDef name generics whereClause params returnType body public async const unsafe doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm whereClause)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Phantoms.unTTerm const)},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Phantoms.unTTerm unsafe)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

fnDefAsync :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool
fnDefAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "async")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefBody :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Syntax.Block
fnDefBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefConst :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool
fnDefConst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "const")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefDoc :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm (Maybe String)
fnDefDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefGenerics :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm [Syntax.GenericParam]
fnDefGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefName :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm String
fnDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefParams :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm [Syntax.FnParam]
fnDefParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefPublic :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool
fnDefPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefReturnType :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm (Maybe Syntax.Type)
fnDefReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "returnType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefUnsafe :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool
fnDefUnsafe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "unsafe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefWhereClause :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm (Maybe Syntax.WhereClause)
fnDefWhereClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
        Core.projectionField = (Core.Name "whereClause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnDefWithAsync :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FnDef
fnDefWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithBody :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.FnDef
fnDefWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithConst :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FnDef
fnDefWithConst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithDoc :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FnDef
fnDefWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fnDefWithGenerics :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.FnDef
fnDefWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithName :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.FnDef
fnDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithParams :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm [Syntax.FnParam] -> Phantoms.TTerm Syntax.FnDef
fnDefWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithPublic :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FnDef
fnDefWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithReturnType :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.FnDef
fnDefWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithUnsafe :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FnDef
fnDefWithUnsafe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnDefWithWhereClause :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.FnDef
fnDefWithWhereClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "const")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnParam :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FnParam
fnParam pattern type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

fnParamPattern :: Phantoms.TTerm Syntax.FnParam -> Phantoms.TTerm Syntax.Pattern
fnParamPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnParam"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnParamType :: Phantoms.TTerm Syntax.FnParam -> Phantoms.TTerm Syntax.Type
fnParamType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnParam"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnParamWithPattern :: Phantoms.TTerm Syntax.FnParam -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.FnParam
fnParamWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnParam"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnParamWithType :: Phantoms.TTerm Syntax.FnParam -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FnParam
fnParamWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnParam"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fnPointerType :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FnPointerType
fnPointerType params returnType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnPointerType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)}]}))

fnPointerTypeParams :: Phantoms.TTerm Syntax.FnPointerType -> Phantoms.TTerm [Syntax.Type]
fnPointerTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnPointerType"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnPointerTypeReturnType :: Phantoms.TTerm Syntax.FnPointerType -> Phantoms.TTerm Syntax.Type
fnPointerTypeReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnPointerType"),
        Core.projectionField = (Core.Name "returnType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fnPointerTypeWithParams :: Phantoms.TTerm Syntax.FnPointerType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.FnPointerType
fnPointerTypeWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnPointerType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnPointerType"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fnPointerTypeWithReturnType :: Phantoms.TTerm Syntax.FnPointerType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FnPointerType
fnPointerTypeWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.FnPointerType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.FnPointerType"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forExpr :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForExpr
forExpr label pattern iter body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "iter"),
          Core.fieldTerm = (Phantoms.unTTerm iter)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

forExprBody :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm Syntax.Block
forExprBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forExprIter :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm Syntax.Expression
forExprIter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
        Core.projectionField = (Core.Name "iter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forExprLabel :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm (Maybe String)
forExprLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forExprPattern :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm Syntax.Pattern
forExprPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forExprWithBody :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForExpr
forExprWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "iter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forExprWithIter :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForExpr
forExprWithIter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forExprWithLabel :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ForExpr
forExprWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "iter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forExprWithPattern :: Phantoms.TTerm Syntax.ForExpr -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.ForExpr
forExprWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "iter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "iter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ForExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

genericArgBinding :: Phantoms.TTerm Syntax.TypeBinding -> Phantoms.TTerm Syntax.GenericArg
genericArgBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.GenericArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericArgConst :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.GenericArg
genericArgConst x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.GenericArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericArgLifetime :: Phantoms.TTerm Syntax.Lifetime -> Phantoms.TTerm Syntax.GenericArg
genericArgLifetime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.GenericArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lifetime"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericArgType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GenericArg
genericArgType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.GenericArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericArgumentsAngleBracketed :: Phantoms.TTerm Syntax.AngleBracketedArgs -> Phantoms.TTerm Syntax.GenericArguments
genericArgumentsAngleBracketed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.GenericArguments"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "angleBracketed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericArgumentsNone :: Phantoms.TTerm Syntax.GenericArguments
genericArgumentsNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.GenericArguments"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

genericArgumentsParenthesized :: Phantoms.TTerm Syntax.ParenthesizedArgs -> Phantoms.TTerm Syntax.GenericArguments
genericArgumentsParenthesized x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.GenericArguments"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

genericParam :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.GenericParam
genericParam name bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.GenericParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))

genericParamBounds :: Phantoms.TTerm Syntax.GenericParam -> Phantoms.TTerm [Syntax.TypeParamBound]
genericParamBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.GenericParam"),
        Core.projectionField = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericParamName :: Phantoms.TTerm Syntax.GenericParam -> Phantoms.TTerm String
genericParamName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.GenericParam"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

genericParamWithBounds :: Phantoms.TTerm Syntax.GenericParam -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.GenericParam
genericParamWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.GenericParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.GenericParam"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

genericParamWithName :: Phantoms.TTerm Syntax.GenericParam -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.GenericParam
genericParamWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.GenericParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.GenericParam"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

identifierPattern :: Phantoms.TTerm String -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.IdentifierPattern
identifierPattern name mutable atPattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)},
        Core.Field {
          Core.fieldName = (Core.Name "atPattern"),
          Core.fieldTerm = (Phantoms.unTTerm atPattern)}]}))

identifierPatternAtPattern :: Phantoms.TTerm Syntax.IdentifierPattern -> Phantoms.TTerm (Maybe Syntax.Pattern)
identifierPatternAtPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
        Core.projectionField = (Core.Name "atPattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identifierPatternMutable :: Phantoms.TTerm Syntax.IdentifierPattern -> Phantoms.TTerm Bool
identifierPatternMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
        Core.projectionField = (Core.Name "mutable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identifierPatternName :: Phantoms.TTerm Syntax.IdentifierPattern -> Phantoms.TTerm String
identifierPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identifierPatternWithAtPattern :: Phantoms.TTerm Syntax.IdentifierPattern -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.IdentifierPattern
identifierPatternWithAtPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "atPattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

identifierPatternWithMutable :: Phantoms.TTerm Syntax.IdentifierPattern -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IdentifierPattern
identifierPatternWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "atPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
              Core.projectionField = (Core.Name "atPattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

identifierPatternWithName :: Phantoms.TTerm Syntax.IdentifierPattern -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.IdentifierPattern
identifierPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "atPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IdentifierPattern"),
              Core.projectionField = (Core.Name "atPattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifConditionBool :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfCondition
ifConditionBool x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.IfCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bool"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ifConditionLet :: Phantoms.TTerm Syntax.LetCondition -> Phantoms.TTerm Syntax.IfCondition
ifConditionLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.IfCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ifExpr :: Phantoms.TTerm Syntax.IfCondition -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfExpr
ifExpr condition thenBlock elseBranch =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "thenBlock"),
          Core.fieldTerm = (Phantoms.unTTerm thenBlock)},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Phantoms.unTTerm elseBranch)}]}))

ifExprCondition :: Phantoms.TTerm Syntax.IfExpr -> Phantoms.TTerm Syntax.IfCondition
ifExprCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
        Core.projectionField = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExprElseBranch :: Phantoms.TTerm Syntax.IfExpr -> Phantoms.TTerm (Maybe Syntax.Expression)
ifExprElseBranch x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
        Core.projectionField = (Core.Name "elseBranch")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExprThenBlock :: Phantoms.TTerm Syntax.IfExpr -> Phantoms.TTerm Syntax.Block
ifExprThenBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
        Core.projectionField = (Core.Name "thenBlock")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExprWithCondition :: Phantoms.TTerm Syntax.IfExpr -> Phantoms.TTerm Syntax.IfCondition -> Phantoms.TTerm Syntax.IfExpr
ifExprWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thenBlock"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
              Core.projectionField = (Core.Name "thenBlock")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
              Core.projectionField = (Core.Name "elseBranch")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifExprWithElseBranch :: Phantoms.TTerm Syntax.IfExpr -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfExpr
ifExprWithElseBranch original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenBlock"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
              Core.projectionField = (Core.Name "thenBlock")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifExprWithThenBlock :: Phantoms.TTerm Syntax.IfExpr -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.IfExpr
ifExprWithThenBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenBlock"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IfExpr"),
              Core.projectionField = (Core.Name "elseBranch")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implBlock :: Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm (Maybe Syntax.TypePath) -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.ImplItem] -> Phantoms.TTerm Syntax.ImplBlock
implBlock generics whereClause trait negative selfType items =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm whereClause)},
        Core.Field {
          Core.fieldName = (Core.Name "trait"),
          Core.fieldTerm = (Phantoms.unTTerm trait)},
        Core.Field {
          Core.fieldName = (Core.Name "negative"),
          Core.fieldTerm = (Phantoms.unTTerm negative)},
        Core.Field {
          Core.fieldName = (Core.Name "selfType"),
          Core.fieldTerm = (Phantoms.unTTerm selfType)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm items)}]}))

implBlockGenerics :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm [Syntax.GenericParam]
implBlockGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implBlockItems :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm [Syntax.ImplItem]
implBlockItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
        Core.projectionField = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implBlockNegative :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm Bool
implBlockNegative x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
        Core.projectionField = (Core.Name "negative")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implBlockSelfType :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm Syntax.Type
implBlockSelfType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
        Core.projectionField = (Core.Name "selfType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implBlockTrait :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm (Maybe Syntax.TypePath)
implBlockTrait x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
        Core.projectionField = (Core.Name "trait")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implBlockWhereClause :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm (Maybe Syntax.WhereClause)
implBlockWhereClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
        Core.projectionField = (Core.Name "whereClause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implBlockWithGenerics :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.ImplBlock
implBlockWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trait"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "trait")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "negative"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "negative")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "selfType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "selfType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implBlockWithItems :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm [Syntax.ImplItem] -> Phantoms.TTerm Syntax.ImplBlock
implBlockWithItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trait"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "trait")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "negative"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "negative")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "selfType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "selfType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

implBlockWithNegative :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ImplBlock
implBlockWithNegative original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trait"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "trait")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "negative"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "selfType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "selfType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implBlockWithSelfType :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ImplBlock
implBlockWithSelfType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trait"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "trait")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "negative"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "negative")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "selfType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implBlockWithTrait :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm (Maybe Syntax.TypePath) -> Phantoms.TTerm Syntax.ImplBlock
implBlockWithTrait original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trait"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "negative"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "negative")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "selfType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "selfType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implBlockWithWhereClause :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.ImplBlock
implBlockWithWhereClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trait"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "trait")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "negative"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "negative")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "selfType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "selfType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplBlock"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implItemConst :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Syntax.ImplItem
implItemConst x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.ImplItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

implItemMethod :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm Syntax.ImplItem
implItemMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.ImplItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

implItemType :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.ImplItem
implItemType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.ImplItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

implMethod :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm [Syntax.MethodParam] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ImplMethod
implMethod name generics whereClause params returnType body public default_ doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm whereClause)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

implMethodBody :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm Syntax.Block
implMethodBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodDefault :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm Bool
implMethodDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodDoc :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm (Maybe String)
implMethodDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodGenerics :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm [Syntax.GenericParam]
implMethodGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodName :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm String
implMethodName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodParams :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm [Syntax.MethodParam]
implMethodParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodPublic :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm Bool
implMethodPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodReturnType :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm (Maybe Syntax.Type)
implMethodReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "returnType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodWhereClause :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm (Maybe Syntax.WhereClause)
implMethodWhereClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
        Core.projectionField = (Core.Name "whereClause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implMethodWithBody :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implMethodWithDefault :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implMethodWithDoc :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

implMethodWithGenerics :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implMethodWithName :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implMethodWithParams :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm [Syntax.MethodParam] -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implMethodWithPublic :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implMethodWithReturnType :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implMethodWithWhereClause :: Phantoms.TTerm Syntax.ImplMethod -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.ImplMethod
implMethodWithWhereClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ImplMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

indexExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IndexExpr
indexExpr object index =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm index)}]}))

indexExprIndex :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression
indexExprIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IndexExpr"),
        Core.projectionField = (Core.Name "index")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

indexExprObject :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression
indexExprObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IndexExpr"),
        Core.projectionField = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

indexExprWithIndex :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IndexExpr
indexExprWithIndex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IndexExpr"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

indexExprWithObject :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IndexExpr
indexExprWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IndexExpr"),
              Core.projectionField = (Core.Name "index")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

integerLiteral :: Phantoms.TTerm Integer -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteral value suffix =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "suffix"),
          Core.fieldTerm = (Phantoms.unTTerm suffix)}]}))

integerLiteralSuffix :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm (Maybe String)
integerLiteralSuffix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IntegerLiteral"),
        Core.projectionField = (Core.Name "suffix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

integerLiteralValue :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer
integerLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IntegerLiteral"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

integerLiteralWithSuffix :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralWithSuffix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IntegerLiteral"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

integerLiteralWithValue :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "suffix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.IntegerLiteral"),
              Core.projectionField = (Core.Name "suffix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

itemConst :: Phantoms.TTerm Syntax.ConstDef -> Phantoms.TTerm Syntax.Item
itemConst x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemEnum :: Phantoms.TTerm Syntax.EnumDef -> Phantoms.TTerm Syntax.Item
itemEnum x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemFn :: Phantoms.TTerm Syntax.FnDef -> Phantoms.TTerm Syntax.Item
itemFn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemImpl :: Phantoms.TTerm Syntax.ImplBlock -> Phantoms.TTerm Syntax.Item
itemImpl x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "impl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemMacro :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm Syntax.Item
itemMacro x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemMod :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm Syntax.Item
itemMod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemStatic :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Syntax.Item
itemStatic x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemStruct :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm Syntax.Item
itemStruct x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemTrait :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm Syntax.Item
itemTrait x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trait"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemTypeAlias :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Item
itemTypeAlias x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAlias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemUse :: Phantoms.TTerm Syntax.UseDeclaration -> Phantoms.TTerm Syntax.Item
itemUse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Item"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "use"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemWithComments :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Visibility -> Phantoms.TTerm Syntax.Item -> Phantoms.TTerm Syntax.ItemWithComments
itemWithComments doc visibility item =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "visibility"),
          Core.fieldTerm = (Phantoms.unTTerm visibility)},
        Core.Field {
          Core.fieldName = (Core.Name "item"),
          Core.fieldTerm = (Phantoms.unTTerm item)}]}))

itemWithCommentsDoc :: Phantoms.TTerm Syntax.ItemWithComments -> Phantoms.TTerm (Maybe String)
itemWithCommentsDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemWithCommentsItem :: Phantoms.TTerm Syntax.ItemWithComments -> Phantoms.TTerm Syntax.Item
itemWithCommentsItem x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
        Core.projectionField = (Core.Name "item")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemWithCommentsVisibility :: Phantoms.TTerm Syntax.ItemWithComments -> Phantoms.TTerm Syntax.Visibility
itemWithCommentsVisibility x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
        Core.projectionField = (Core.Name "visibility")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemWithCommentsWithDoc :: Phantoms.TTerm Syntax.ItemWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ItemWithComments
itemWithCommentsWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "visibility"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
              Core.projectionField = (Core.Name "visibility")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "item"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
              Core.projectionField = (Core.Name "item")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

itemWithCommentsWithItem :: Phantoms.TTerm Syntax.ItemWithComments -> Phantoms.TTerm Syntax.Item -> Phantoms.TTerm Syntax.ItemWithComments
itemWithCommentsWithItem original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "visibility"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
              Core.projectionField = (Core.Name "visibility")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "item"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

itemWithCommentsWithVisibility :: Phantoms.TTerm Syntax.ItemWithComments -> Phantoms.TTerm Syntax.Visibility -> Phantoms.TTerm Syntax.ItemWithComments
itemWithCommentsWithVisibility original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "visibility"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "item"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ItemWithComments"),
              Core.projectionField = (Core.Name "item")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letCondition :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LetCondition
letCondition pattern expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

letConditionExpr :: Phantoms.TTerm Syntax.LetCondition -> Phantoms.TTerm Syntax.Expression
letConditionExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetCondition"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letConditionPattern :: Phantoms.TTerm Syntax.LetCondition -> Phantoms.TTerm Syntax.Pattern
letConditionPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetCondition"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letConditionWithExpr :: Phantoms.TTerm Syntax.LetCondition -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LetCondition
letConditionWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetCondition"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letConditionWithPattern :: Phantoms.TTerm Syntax.LetCondition -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetCondition
letConditionWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetCondition"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letStatement :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.LetStatement
letStatement pattern mutable type_ init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

letStatementInit :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
letStatementInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
        Core.projectionField = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letStatementMutable :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm Bool
letStatementMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
        Core.projectionField = (Core.Name "mutable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letStatementPattern :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm Syntax.Pattern
letStatementPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letStatementType :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm (Maybe Syntax.Type)
letStatementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letStatementWithInit :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.LetStatement
letStatementWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letStatementWithMutable :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.LetStatement
letStatementWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letStatementWithPattern :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetStatement
letStatementWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letStatementWithType :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.LetStatement
letStatementWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LetStatement"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lifetime :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Lifetime
lifetime name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Lifetime"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

lifetimeName :: Phantoms.TTerm Syntax.Lifetime -> Phantoms.TTerm String
lifetimeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.Lifetime"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lifetimeWithName :: Phantoms.TTerm Syntax.Lifetime -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Lifetime
lifetimeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.Lifetime"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

literalBool :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Literal
literalBool x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bool"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalByte :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Syntax.Literal
literalByte x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalByteString :: Phantoms.TTerm B.ByteString -> Phantoms.TTerm Syntax.Literal
literalByteString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byteString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalChar :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.Literal
literalChar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalFloat :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Syntax.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInteger :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalRawString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalRawString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rawString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

loopExpr :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.LoopExpr
loopExpr label body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LoopExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

loopExprBody :: Phantoms.TTerm Syntax.LoopExpr -> Phantoms.TTerm Syntax.Block
loopExprBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LoopExpr"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

loopExprLabel :: Phantoms.TTerm Syntax.LoopExpr -> Phantoms.TTerm (Maybe String)
loopExprLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LoopExpr"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

loopExprWithBody :: Phantoms.TTerm Syntax.LoopExpr -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.LoopExpr
loopExprWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LoopExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LoopExpr"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

loopExprWithLabel :: Phantoms.TTerm Syntax.LoopExpr -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.LoopExpr
loopExprWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.LoopExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.LoopExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

macroDelimiterBrace :: Phantoms.TTerm Syntax.MacroDelimiter
macroDelimiterBrace =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.MacroDelimiter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "brace"),
        Core.fieldTerm = Core.TermUnit}}))

macroDelimiterBracket :: Phantoms.TTerm Syntax.MacroDelimiter
macroDelimiterBracket =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.MacroDelimiter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bracket"),
        Core.fieldTerm = Core.TermUnit}}))

macroDelimiterParen :: Phantoms.TTerm Syntax.MacroDelimiter
macroDelimiterParen =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.MacroDelimiter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paren"),
        Core.fieldTerm = Core.TermUnit}}))

macroInvocation :: Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.MacroDelimiter -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MacroInvocation
macroInvocation path delimiter tokens =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Phantoms.unTTerm delimiter)},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Phantoms.unTTerm tokens)}]}))

macroInvocationDelimiter :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm Syntax.MacroDelimiter
macroInvocationDelimiter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
        Core.projectionField = (Core.Name "delimiter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

macroInvocationPath :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm [String]
macroInvocationPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

macroInvocationTokens :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm String
macroInvocationTokens x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
        Core.projectionField = (Core.Name "tokens")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

macroInvocationWithDelimiter :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm Syntax.MacroDelimiter -> Phantoms.TTerm Syntax.MacroInvocation
macroInvocationWithDelimiter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
              Core.projectionField = (Core.Name "tokens")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

macroInvocationWithPath :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.MacroInvocation
macroInvocationWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
              Core.projectionField = (Core.Name "delimiter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
              Core.projectionField = (Core.Name "tokens")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

macroInvocationWithTokens :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MacroInvocation
macroInvocationWithTokens original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "delimiter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MacroInvocation"),
              Core.projectionField = (Core.Name "delimiter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tokens"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchArm :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MatchArm
matchArm pattern guard body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm guard)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

matchArmBody :: Phantoms.TTerm Syntax.MatchArm -> Phantoms.TTerm Syntax.Expression
matchArmBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchArmGuard :: Phantoms.TTerm Syntax.MatchArm -> Phantoms.TTerm (Maybe Syntax.Expression)
matchArmGuard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
        Core.projectionField = (Core.Name "guard")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchArmPattern :: Phantoms.TTerm Syntax.MatchArm -> Phantoms.TTerm Syntax.Pattern
matchArmPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchArmWithBody :: Phantoms.TTerm Syntax.MatchArm -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MatchArm
matchArmWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
              Core.projectionField = (Core.Name "guard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchArmWithGuard :: Phantoms.TTerm Syntax.MatchArm -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.MatchArm
matchArmWithGuard original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchArmWithPattern :: Phantoms.TTerm Syntax.MatchArm -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.MatchArm
matchArmWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
              Core.projectionField = (Core.Name "guard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchArm"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.MatchArm] -> Phantoms.TTerm Syntax.MatchExpr
matchExpr scrutinee arms =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MatchExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Phantoms.unTTerm scrutinee)},
        Core.Field {
          Core.fieldName = (Core.Name "arms"),
          Core.fieldTerm = (Phantoms.unTTerm arms)}]}))

matchExprArms :: Phantoms.TTerm Syntax.MatchExpr -> Phantoms.TTerm [Syntax.MatchArm]
matchExprArms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchExpr"),
        Core.projectionField = (Core.Name "arms")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchExprScrutinee :: Phantoms.TTerm Syntax.MatchExpr -> Phantoms.TTerm Syntax.Expression
matchExprScrutinee x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchExpr"),
        Core.projectionField = (Core.Name "scrutinee")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchExprWithArms :: Phantoms.TTerm Syntax.MatchExpr -> Phantoms.TTerm [Syntax.MatchArm] -> Phantoms.TTerm Syntax.MatchExpr
matchExprWithArms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MatchExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchExpr"),
              Core.projectionField = (Core.Name "scrutinee")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchExprWithScrutinee :: Phantoms.TTerm Syntax.MatchExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MatchExpr
matchExprWithScrutinee original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MatchExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MatchExpr"),
              Core.projectionField = (Core.Name "arms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodCallExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm String -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MethodCallExpr
methodCallExpr receiver method turbofish args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Phantoms.unTTerm receiver)},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Phantoms.unTTerm method)},
        Core.Field {
          Core.fieldName = (Core.Name "turbofish"),
          Core.fieldTerm = (Phantoms.unTTerm turbofish)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

methodCallExprArgs :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm [Syntax.Expression]
methodCallExprArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodCallExprMethod :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm String
methodCallExprMethod x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
        Core.projectionField = (Core.Name "method")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodCallExprReceiver :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm Syntax.Expression
methodCallExprReceiver x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
        Core.projectionField = (Core.Name "receiver")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodCallExprTurbofish :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm [Syntax.Type]
methodCallExprTurbofish x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
        Core.projectionField = (Core.Name "turbofish")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodCallExprWithArgs :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MethodCallExpr
methodCallExprWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "receiver")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "method")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "turbofish"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "turbofish")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodCallExprWithMethod :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MethodCallExpr
methodCallExprWithMethod original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "receiver")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "turbofish"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "turbofish")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodCallExprWithReceiver :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MethodCallExpr
methodCallExprWithReceiver original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "method")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "turbofish"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "turbofish")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodCallExprWithTurbofish :: Phantoms.TTerm Syntax.MethodCallExpr -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.MethodCallExpr
methodCallExprWithTurbofish original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "receiver")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "method")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "turbofish"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.MethodCallExpr"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodParamRegular :: Phantoms.TTerm Syntax.FnParam -> Phantoms.TTerm Syntax.MethodParam
methodParamRegular x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.MethodParam"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regular"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodParamSelf :: Phantoms.TTerm Syntax.SelfParam -> Phantoms.TTerm Syntax.MethodParam
methodParamSelf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.MethodParam"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

modDef :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe [Syntax.Item]) -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ModDef
modDef name body public doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

modDefBody :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm (Maybe [Syntax.Item])
modDefBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

modDefDoc :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm (Maybe String)
modDefDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

modDefName :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm String
modDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

modDefPublic :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm Bool
modDefPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

modDefWithBody :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm (Maybe [Syntax.Item]) -> Phantoms.TTerm Syntax.ModDef
modDefWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

modDefWithDoc :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ModDef
modDefWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

modDefWithName :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ModDef
modDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

modDefWithPublic :: Phantoms.TTerm Syntax.ModDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ModDef
modDefWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ModDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parenthesizedArgs :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ParenthesizedArgs
parenthesizedArgs inputs output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ParenthesizedArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inputs"),
          Core.fieldTerm = (Phantoms.unTTerm inputs)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

parenthesizedArgsInputs :: Phantoms.TTerm Syntax.ParenthesizedArgs -> Phantoms.TTerm [Syntax.Type]
parenthesizedArgsInputs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ParenthesizedArgs"),
        Core.projectionField = (Core.Name "inputs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parenthesizedArgsOutput :: Phantoms.TTerm Syntax.ParenthesizedArgs -> Phantoms.TTerm (Maybe Syntax.Type)
parenthesizedArgsOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ParenthesizedArgs"),
        Core.projectionField = (Core.Name "output")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parenthesizedArgsWithInputs :: Phantoms.TTerm Syntax.ParenthesizedArgs -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ParenthesizedArgs
parenthesizedArgsWithInputs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ParenthesizedArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inputs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ParenthesizedArgs"),
              Core.projectionField = (Core.Name "output")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parenthesizedArgsWithOutput :: Phantoms.TTerm Syntax.ParenthesizedArgs -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ParenthesizedArgs
parenthesizedArgsWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ParenthesizedArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inputs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ParenthesizedArgs"),
              Core.projectionField = (Core.Name "inputs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pathSegment :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.GenericArguments -> Phantoms.TTerm Syntax.PathSegment
pathSegment name arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.PathSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

pathSegmentArguments :: Phantoms.TTerm Syntax.PathSegment -> Phantoms.TTerm Syntax.GenericArguments
pathSegmentArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.PathSegment"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathSegmentName :: Phantoms.TTerm Syntax.PathSegment -> Phantoms.TTerm String
pathSegmentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.PathSegment"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathSegmentWithArguments :: Phantoms.TTerm Syntax.PathSegment -> Phantoms.TTerm Syntax.GenericArguments -> Phantoms.TTerm Syntax.PathSegment
pathSegmentWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.PathSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.PathSegment"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pathSegmentWithName :: Phantoms.TTerm Syntax.PathSegment -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.PathSegment
pathSegmentWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.PathSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.PathSegment"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternIdentifier :: Phantoms.TTerm Syntax.IdentifierPattern -> Phantoms.TTerm Syntax.Pattern
patternIdentifier x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Pattern
patternLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternOr :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern
patternOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternParen :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Pattern
patternParen x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paren"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternPath :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm Syntax.Pattern
patternPath x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternRange :: Phantoms.TTerm Syntax.RangePattern -> Phantoms.TTerm Syntax.Pattern
patternRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternReference :: Phantoms.TTerm Syntax.RefPattern -> Phantoms.TTerm Syntax.Pattern
patternReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternRest :: Phantoms.TTerm Syntax.Pattern
patternRest =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = Core.TermUnit}}))

patternSlice :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern
patternSlice x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternStruct :: Phantoms.TTerm Syntax.StructPattern -> Phantoms.TTerm Syntax.Pattern
patternStruct x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTuple :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern
patternTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTupleStruct :: Phantoms.TTerm Syntax.TupleStructPattern -> Phantoms.TTerm Syntax.Pattern
patternTupleStruct x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tupleStruct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternWildcard :: Phantoms.TTerm Syntax.Pattern
patternWildcard =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))

rangeExpr :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RangeExpr
rangeExpr from to inclusive =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm from)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm to)},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Phantoms.unTTerm inclusive)}]}))

rangeExprFrom :: Phantoms.TTerm Syntax.RangeExpr -> Phantoms.TTerm (Maybe Syntax.Expression)
rangeExprFrom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
        Core.projectionField = (Core.Name "from")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeExprInclusive :: Phantoms.TTerm Syntax.RangeExpr -> Phantoms.TTerm Bool
rangeExprInclusive x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
        Core.projectionField = (Core.Name "inclusive")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeExprTo :: Phantoms.TTerm Syntax.RangeExpr -> Phantoms.TTerm (Maybe Syntax.Expression)
rangeExprTo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
        Core.projectionField = (Core.Name "to")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeExprWithFrom :: Phantoms.TTerm Syntax.RangeExpr -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.RangeExpr
rangeExprWithFrom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
              Core.projectionField = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
              Core.projectionField = (Core.Name "inclusive")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangeExprWithInclusive :: Phantoms.TTerm Syntax.RangeExpr -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RangeExpr
rangeExprWithInclusive original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
              Core.projectionField = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangeExprWithTo :: Phantoms.TTerm Syntax.RangeExpr -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.RangeExpr
rangeExprWithTo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangeExpr"),
              Core.projectionField = (Core.Name "inclusive")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangePattern :: Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RangePattern
rangePattern from to inclusive =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm from)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm to)},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Phantoms.unTTerm inclusive)}]}))

rangePatternFrom :: Phantoms.TTerm Syntax.RangePattern -> Phantoms.TTerm (Maybe Syntax.Pattern)
rangePatternFrom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
        Core.projectionField = (Core.Name "from")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangePatternInclusive :: Phantoms.TTerm Syntax.RangePattern -> Phantoms.TTerm Bool
rangePatternInclusive x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
        Core.projectionField = (Core.Name "inclusive")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangePatternTo :: Phantoms.TTerm Syntax.RangePattern -> Phantoms.TTerm (Maybe Syntax.Pattern)
rangePatternTo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
        Core.projectionField = (Core.Name "to")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangePatternWithFrom :: Phantoms.TTerm Syntax.RangePattern -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.RangePattern
rangePatternWithFrom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
              Core.projectionField = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
              Core.projectionField = (Core.Name "inclusive")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangePatternWithInclusive :: Phantoms.TTerm Syntax.RangePattern -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RangePattern
rangePatternWithInclusive original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
              Core.projectionField = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangePatternWithTo :: Phantoms.TTerm Syntax.RangePattern -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.RangePattern
rangePatternWithTo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inclusive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RangePattern"),
              Core.projectionField = (Core.Name "inclusive")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rawPointerType :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RawPointerType
rawPointerType mutable type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RawPointerType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

rawPointerTypeMutable :: Phantoms.TTerm Syntax.RawPointerType -> Phantoms.TTerm Bool
rawPointerTypeMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RawPointerType"),
        Core.projectionField = (Core.Name "mutable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rawPointerTypeType :: Phantoms.TTerm Syntax.RawPointerType -> Phantoms.TTerm Syntax.Type
rawPointerTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RawPointerType"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rawPointerTypeWithMutable :: Phantoms.TTerm Syntax.RawPointerType -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RawPointerType
rawPointerTypeWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RawPointerType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RawPointerType"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rawPointerTypeWithType :: Phantoms.TTerm Syntax.RawPointerType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RawPointerType
rawPointerTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RawPointerType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RawPointerType"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

refExpr :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RefExpr
refExpr mutable expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RefExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

refExprExpr :: Phantoms.TTerm Syntax.RefExpr -> Phantoms.TTerm Syntax.Expression
refExprExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefExpr"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

refExprMutable :: Phantoms.TTerm Syntax.RefExpr -> Phantoms.TTerm Bool
refExprMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefExpr"),
        Core.projectionField = (Core.Name "mutable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

refExprWithExpr :: Phantoms.TTerm Syntax.RefExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RefExpr
refExprWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RefExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefExpr"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

refExprWithMutable :: Phantoms.TTerm Syntax.RefExpr -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RefExpr
refExprWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RefExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefExpr"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

refPattern :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.RefPattern
refPattern mutable pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RefPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

refPatternMutable :: Phantoms.TTerm Syntax.RefPattern -> Phantoms.TTerm Bool
refPatternMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefPattern"),
        Core.projectionField = (Core.Name "mutable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

refPatternPattern :: Phantoms.TTerm Syntax.RefPattern -> Phantoms.TTerm Syntax.Pattern
refPatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefPattern"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

refPatternWithMutable :: Phantoms.TTerm Syntax.RefPattern -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RefPattern
refPatternWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RefPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefPattern"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

refPatternWithPattern :: Phantoms.TTerm Syntax.RefPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.RefPattern
refPatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.RefPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.RefPattern"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

referenceType :: Phantoms.TTerm (Maybe Syntax.Lifetime) -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ReferenceType
referenceType lifetime mutable type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lifetime"),
          Core.fieldTerm = (Phantoms.unTTerm lifetime)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

referenceTypeLifetime :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm (Maybe Syntax.Lifetime)
referenceTypeLifetime x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
        Core.projectionField = (Core.Name "lifetime")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceTypeMutable :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Bool
referenceTypeMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
        Core.projectionField = (Core.Name "mutable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceTypeType :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.Type
referenceTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceTypeWithLifetime :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm (Maybe Syntax.Lifetime) -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeWithLifetime original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lifetime"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

referenceTypeWithMutable :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lifetime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
              Core.projectionField = (Core.Name "lifetime")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

referenceTypeWithType :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lifetime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
              Core.projectionField = (Core.Name "lifetime")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.ReferenceType"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selfParamOwned :: Phantoms.TTerm Syntax.SelfParam
selfParamOwned =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.SelfParam"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "owned"),
        Core.fieldTerm = Core.TermUnit}}))

selfParamRef :: Phantoms.TTerm Syntax.SelfParam
selfParamRef =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.SelfParam"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = Core.TermUnit}}))

selfParamRefMut :: Phantoms.TTerm Syntax.SelfParam
selfParamRefMut =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.SelfParam"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "refMut"),
        Core.fieldTerm = Core.TermUnit}}))

statementEmpty :: Phantoms.TTerm Syntax.Statement
statementEmpty =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))

statementExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement
statementExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementItem :: Phantoms.TTerm Syntax.Item -> Phantoms.TTerm Syntax.Statement
statementItem x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "item"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementLet :: Phantoms.TTerm Syntax.LetStatement -> Phantoms.TTerm Syntax.Statement
statementLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

staticDef :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.StaticDef
staticDef name type_ value mutable public doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

staticDefDoc :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm (Maybe String)
staticDefDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticDefMutable :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Bool
staticDefMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
        Core.projectionField = (Core.Name "mutable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticDefName :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm String
staticDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticDefPublic :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Bool
staticDefPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticDefType :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Syntax.Type
staticDefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticDefValue :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Syntax.Expression
staticDefValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticDefWithDoc :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.StaticDef
staticDefWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

staticDefWithMutable :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StaticDef
staticDefWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

staticDefWithName :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StaticDef
staticDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

staticDefWithPublic :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StaticDef
staticDefWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

staticDefWithType :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.StaticDef
staticDefWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

staticDefWithValue :: Phantoms.TTerm Syntax.StaticDef -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.StaticDef
staticDefWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "mutable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StaticDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structBodyNamed :: Phantoms.TTerm [Syntax.StructField] -> Phantoms.TTerm Syntax.StructBody
structBodyNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.StructBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

structBodyTuple :: Phantoms.TTerm [Syntax.TupleField] -> Phantoms.TTerm Syntax.StructBody
structBodyTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.StructBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

structBodyUnit :: Phantoms.TTerm Syntax.StructBody
structBodyUnit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.StructBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

structDef :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.StructBody -> Phantoms.TTerm [String] -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.StructDef
structDef name generics whereClause body derives public doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm whereClause)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Phantoms.unTTerm derives)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

structDefBody :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm Syntax.StructBody
structDefBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structDefDerives :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm [String]
structDefDerives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
        Core.projectionField = (Core.Name "derives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structDefDoc :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm (Maybe String)
structDefDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structDefGenerics :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm [Syntax.GenericParam]
structDefGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structDefName :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm String
structDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structDefPublic :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm Bool
structDefPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structDefWhereClause :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm (Maybe Syntax.WhereClause)
structDefWhereClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
        Core.projectionField = (Core.Name "whereClause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structDefWithBody :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm Syntax.StructBody -> Phantoms.TTerm Syntax.StructDef
structDefWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structDefWithDerives :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.StructDef
structDefWithDerives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structDefWithDoc :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.StructDef
structDefWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

structDefWithGenerics :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.StructDef
structDefWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structDefWithName :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StructDef
structDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structDefWithPublic :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StructDef
structDefWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structDefWithWhereClause :: Phantoms.TTerm Syntax.StructDef -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.StructDef
structDefWithWhereClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "derives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "derives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structExpr :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm [Syntax.FieldValue] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StructExpr
structExpr path fields rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

structExprFields :: Phantoms.TTerm Syntax.StructExpr -> Phantoms.TTerm [Syntax.FieldValue]
structExprFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
        Core.projectionField = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structExprPath :: Phantoms.TTerm Syntax.StructExpr -> Phantoms.TTerm Syntax.ExprPath
structExprPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structExprRest :: Phantoms.TTerm Syntax.StructExpr -> Phantoms.TTerm (Maybe Syntax.Expression)
structExprRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
        Core.projectionField = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structExprWithFields :: Phantoms.TTerm Syntax.StructExpr -> Phantoms.TTerm [Syntax.FieldValue] -> Phantoms.TTerm Syntax.StructExpr
structExprWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structExprWithPath :: Phantoms.TTerm Syntax.StructExpr -> Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm Syntax.StructExpr
structExprWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
              Core.projectionField = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structExprWithRest :: Phantoms.TTerm Syntax.StructExpr -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StructExpr
structExprWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructExpr"),
              Core.projectionField = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

structField :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.StructField
structField name type_ public doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

structFieldDoc :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm (Maybe String)
structFieldDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structFieldName :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm String
structFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structFieldPublic :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm Bool
structFieldPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structFieldType :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm Syntax.Type
structFieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structFieldWithDoc :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.StructField
structFieldWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

structFieldWithName :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StructField
structFieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structFieldWithPublic :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StructField
structFieldWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structFieldWithType :: Phantoms.TTerm Syntax.StructField -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.StructField
structFieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructField"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structPattern :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm [Syntax.FieldPattern] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StructPattern
structPattern path fields rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

structPatternFields :: Phantoms.TTerm Syntax.StructPattern -> Phantoms.TTerm [Syntax.FieldPattern]
structPatternFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
        Core.projectionField = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structPatternPath :: Phantoms.TTerm Syntax.StructPattern -> Phantoms.TTerm Syntax.ExprPath
structPatternPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structPatternRest :: Phantoms.TTerm Syntax.StructPattern -> Phantoms.TTerm Bool
structPatternRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
        Core.projectionField = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

structPatternWithFields :: Phantoms.TTerm Syntax.StructPattern -> Phantoms.TTerm [Syntax.FieldPattern] -> Phantoms.TTerm Syntax.StructPattern
structPatternWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structPatternWithPath :: Phantoms.TTerm Syntax.StructPattern -> Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm Syntax.StructPattern
structPatternWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
              Core.projectionField = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

structPatternWithRest :: Phantoms.TTerm Syntax.StructPattern -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StructPattern
structPatternWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.StructPattern"),
              Core.projectionField = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traitConst :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitConst
traitConst name type_ default_ doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

traitConstDefault :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm (Maybe Syntax.Expression)
traitConstDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitConstDoc :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm (Maybe String)
traitConstDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitConstName :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm String
traitConstName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitConstType :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm Syntax.Type
traitConstType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitConstWithDefault :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.TraitConst
traitConstWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitConstWithDoc :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitConst
traitConstWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traitConstWithName :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TraitConst
traitConstWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitConstWithType :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TraitConst
traitConstWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitConst"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDef :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm [Syntax.TraitItem] -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitDef
traitDef name generics whereClause superTraits items public unsafe doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm whereClause)},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Phantoms.unTTerm superTraits)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm items)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Phantoms.unTTerm unsafe)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

traitDefDoc :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm (Maybe String)
traitDefDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefGenerics :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm [Syntax.GenericParam]
traitDefGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefItems :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm [Syntax.TraitItem]
traitDefItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefName :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm String
traitDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefPublic :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm Bool
traitDefPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefSuperTraits :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm [Syntax.TypeParamBound]
traitDefSuperTraits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "superTraits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefUnsafe :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm Bool
traitDefUnsafe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "unsafe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefWhereClause :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm (Maybe Syntax.WhereClause)
traitDefWhereClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
        Core.projectionField = (Core.Name "whereClause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefWithDoc :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitDef
traitDefWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "superTraits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traitDefWithGenerics :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.TraitDef
traitDefWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "superTraits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefWithItems :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm [Syntax.TraitItem] -> Phantoms.TTerm Syntax.TraitDef
traitDefWithItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "superTraits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefWithName :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TraitDef
traitDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "superTraits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefWithPublic :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TraitDef
traitDefWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "superTraits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefWithSuperTraits :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.TraitDef
traitDefWithSuperTraits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefWithUnsafe :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TraitDef
traitDefWithUnsafe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "superTraits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefWithWhereClause :: Phantoms.TTerm Syntax.TraitDef -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.TraitDef
traitDefWithWhereClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superTraits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "superTraits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unsafe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "unsafe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitDef"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitItemConst :: Phantoms.TTerm Syntax.TraitConst -> Phantoms.TTerm Syntax.TraitItem
traitItemConst x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.TraitItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traitItemMethod :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm Syntax.TraitItem
traitItemMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.TraitItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traitItemType :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm Syntax.TraitItem
traitItemType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.TraitItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

traitMethod :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm [Syntax.MethodParam] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitMethod
traitMethod name generics whereClause params returnType defaultBody doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm whereClause)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Phantoms.unTTerm defaultBody)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

traitMethodDefaultBody :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe Syntax.Block)
traitMethodDefaultBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
        Core.projectionField = (Core.Name "defaultBody")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitMethodDoc :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe String)
traitMethodDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitMethodGenerics :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm [Syntax.GenericParam]
traitMethodGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitMethodName :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm String
traitMethodName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitMethodParams :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm [Syntax.MethodParam]
traitMethodParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitMethodReturnType :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe Syntax.Type)
traitMethodReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
        Core.projectionField = (Core.Name "returnType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitMethodWhereClause :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe Syntax.WhereClause)
traitMethodWhereClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
        Core.projectionField = (Core.Name "whereClause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitMethodWithDefaultBody :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TraitMethod
traitMethodWithDefaultBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitMethodWithDoc :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitMethod
traitMethodWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "defaultBody")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traitMethodWithGenerics :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.TraitMethod
traitMethodWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "defaultBody")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitMethodWithName :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TraitMethod
traitMethodWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "defaultBody")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitMethodWithParams :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm [Syntax.MethodParam] -> Phantoms.TTerm Syntax.TraitMethod
traitMethodWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "defaultBody")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitMethodWithReturnType :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TraitMethod
traitMethodWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "whereClause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "defaultBody")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitMethodWithWhereClause :: Phantoms.TTerm Syntax.TraitMethod -> Phantoms.TTerm (Maybe Syntax.WhereClause) -> Phantoms.TTerm Syntax.TraitMethod
traitMethodWithWhereClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "whereClause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultBody"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "defaultBody")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitMethod"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitType :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitType
traitType name bounds default_ doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

traitTypeBounds :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm [Syntax.TypeParamBound]
traitTypeBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
        Core.projectionField = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitTypeDefault :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm (Maybe Syntax.Type)
traitTypeDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitTypeDoc :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm (Maybe String)
traitTypeDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitTypeName :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm String
traitTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitTypeWithBounds :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.TraitType
traitTypeWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitTypeWithDefault :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TraitType
traitTypeWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitTypeWithDoc :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TraitType
traitTypeWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traitTypeWithName :: Phantoms.TTerm Syntax.TraitType -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TraitType
traitTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TraitType"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tupleField :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TupleField
tupleField type_ public =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)}]}))

tupleFieldPublic :: Phantoms.TTerm Syntax.TupleField -> Phantoms.TTerm Bool
tupleFieldPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleField"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleFieldType :: Phantoms.TTerm Syntax.TupleField -> Phantoms.TTerm Syntax.Type
tupleFieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleField"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleFieldWithPublic :: Phantoms.TTerm Syntax.TupleField -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TupleField
tupleFieldWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleField"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tupleFieldWithType :: Phantoms.TTerm Syntax.TupleField -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TupleField
tupleFieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleField"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tupleIndexExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Int -> Phantoms.TTerm Syntax.TupleIndexExpr
tupleIndexExpr tuple index =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleIndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tuple"),
          Core.fieldTerm = (Phantoms.unTTerm tuple)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm index)}]}))

tupleIndexExprIndex :: Phantoms.TTerm Syntax.TupleIndexExpr -> Phantoms.TTerm Int
tupleIndexExprIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleIndexExpr"),
        Core.projectionField = (Core.Name "index")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleIndexExprTuple :: Phantoms.TTerm Syntax.TupleIndexExpr -> Phantoms.TTerm Syntax.Expression
tupleIndexExprTuple x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleIndexExpr"),
        Core.projectionField = (Core.Name "tuple")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleIndexExprWithIndex :: Phantoms.TTerm Syntax.TupleIndexExpr -> Phantoms.TTerm Int -> Phantoms.TTerm Syntax.TupleIndexExpr
tupleIndexExprWithIndex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleIndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tuple"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleIndexExpr"),
              Core.projectionField = (Core.Name "tuple")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tupleIndexExprWithTuple :: Phantoms.TTerm Syntax.TupleIndexExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TupleIndexExpr
tupleIndexExprWithTuple original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleIndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tuple"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleIndexExpr"),
              Core.projectionField = (Core.Name "index")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tupleStructPattern :: Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.TupleStructPattern
tupleStructPattern path elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleStructPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

tupleStructPatternElements :: Phantoms.TTerm Syntax.TupleStructPattern -> Phantoms.TTerm [Syntax.Pattern]
tupleStructPatternElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleStructPattern"),
        Core.projectionField = (Core.Name "elements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleStructPatternPath :: Phantoms.TTerm Syntax.TupleStructPattern -> Phantoms.TTerm Syntax.ExprPath
tupleStructPatternPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleStructPattern"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleStructPatternWithElements :: Phantoms.TTerm Syntax.TupleStructPattern -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.TupleStructPattern
tupleStructPatternWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleStructPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleStructPattern"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tupleStructPatternWithPath :: Phantoms.TTerm Syntax.TupleStructPattern -> Phantoms.TTerm Syntax.ExprPath -> Phantoms.TTerm Syntax.TupleStructPattern
tupleStructPatternWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TupleStructPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TupleStructPattern"),
              Core.projectionField = (Core.Name "elements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeAlias :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeAlias
typeAlias name generics type_ public doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm generics)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

typeAliasDoc :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm (Maybe String)
typeAliasDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAliasGenerics :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm [Syntax.GenericParam]
typeAliasGenerics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "generics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAliasName :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm String
typeAliasName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAliasPublic :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Bool
typeAliasPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAliasType :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Type
typeAliasType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAliasWithDoc :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeAliasWithGenerics :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm [Syntax.GenericParam] -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithGenerics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeAliasWithName :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeAliasWithPublic :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeAliasWithType :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "generics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeArray :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Type
typeArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAscriptionExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAscriptionExpr
typeAscriptionExpr expr type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAscriptionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeAscriptionExprExpr :: Phantoms.TTerm Syntax.TypeAscriptionExpr -> Phantoms.TTerm Syntax.Expression
typeAscriptionExprExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAscriptionExpr"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAscriptionExprType :: Phantoms.TTerm Syntax.TypeAscriptionExpr -> Phantoms.TTerm Syntax.Type
typeAscriptionExprType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAscriptionExpr"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAscriptionExprWithExpr :: Phantoms.TTerm Syntax.TypeAscriptionExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeAscriptionExpr
typeAscriptionExprWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAscriptionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAscriptionExpr"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeAscriptionExprWithType :: Phantoms.TTerm Syntax.TypeAscriptionExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAscriptionExpr
typeAscriptionExprWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeAscriptionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeAscriptionExpr"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeBinding :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeBinding
typeBinding name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeBindingName :: Phantoms.TTerm Syntax.TypeBinding -> Phantoms.TTerm String
typeBindingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeBinding"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBindingType :: Phantoms.TTerm Syntax.TypeBinding -> Phantoms.TTerm Syntax.Type
typeBindingType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeBinding"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBindingWithName :: Phantoms.TTerm Syntax.TypeBinding -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TypeBinding
typeBindingWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeBinding"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeBindingWithType :: Phantoms.TTerm Syntax.TypeBinding -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeBinding
typeBindingWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypeBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypeBinding"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeDynTrait :: Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.Type
typeDynTrait x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dynTrait"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeFnPointer :: Phantoms.TTerm Syntax.FnPointerType -> Phantoms.TTerm Syntax.Type
typeFnPointer x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fnPointer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeImplTrait :: Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.Type
typeImplTrait x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implTrait"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeInferred :: Phantoms.TTerm Syntax.Type
typeInferred =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inferred"),
        Core.fieldTerm = Core.TermUnit}}))

typeMacro :: Phantoms.TTerm Syntax.MacroInvocation -> Phantoms.TTerm Syntax.Type
typeMacro x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeNever :: Phantoms.TTerm Syntax.Type
typeNever =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "never"),
        Core.fieldTerm = Core.TermUnit}}))

typeParamBoundLifetime :: Phantoms.TTerm Syntax.Lifetime -> Phantoms.TTerm Syntax.TypeParamBound
typeParamBoundLifetime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.TypeParamBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lifetime"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeParamBoundTrait :: Phantoms.TTerm Syntax.TypePath -> Phantoms.TTerm Syntax.TypeParamBound
typeParamBoundTrait x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.TypeParamBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trait"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typePath :: Phantoms.TTerm Syntax.TypePath -> Phantoms.TTerm Syntax.Type
typePath x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typePathGlobal :: Phantoms.TTerm Syntax.TypePath -> Phantoms.TTerm Bool
typePathGlobal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypePath"),
        Core.projectionField = (Core.Name "global")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typePathSegments :: Phantoms.TTerm Syntax.TypePath -> Phantoms.TTerm [Syntax.PathSegment]
typePathSegments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypePath"),
        Core.projectionField = (Core.Name "segments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typePathWithGlobal :: Phantoms.TTerm Syntax.TypePath -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypePath
typePathWithGlobal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypePath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "global"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypePath"),
              Core.projectionField = (Core.Name "segments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typePathWithSegments :: Phantoms.TTerm Syntax.TypePath -> Phantoms.TTerm [Syntax.PathSegment] -> Phantoms.TTerm Syntax.TypePath
typePathWithSegments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypePath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "global"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.TypePath"),
              Core.projectionField = (Core.Name "global")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typePath_ :: Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.PathSegment] -> Phantoms.TTerm Syntax.TypePath
typePath_ global segments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.TypePath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "global"),
          Core.fieldTerm = (Phantoms.unTTerm global)},
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Phantoms.unTTerm segments)}]}))

typeRawPointer :: Phantoms.TTerm Syntax.RawPointerType -> Phantoms.TTerm Syntax.Type
typeRawPointer x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rawPointer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeReference :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.Type
typeReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSlice :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type
typeSlice x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeTuple :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type
typeTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeUnit :: Phantoms.TTerm Syntax.Type
typeUnit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

unaryExpr :: Phantoms.TTerm Syntax.UnaryOp -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UnaryExpr
unaryExpr op operand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UnaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)}]}))

unaryExprOp :: Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.UnaryOp
unaryExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UnaryExpr"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExprOperand :: Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.Expression
unaryExprOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UnaryExpr"),
        Core.projectionField = (Core.Name "operand")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExprWithOp :: Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.UnaryOp -> Phantoms.TTerm Syntax.UnaryExpr
unaryExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UnaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UnaryExpr"),
              Core.projectionField = (Core.Name "operand")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unaryExprWithOperand :: Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UnaryExpr
unaryExprWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UnaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UnaryExpr"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryOpNeg :: Phantoms.TTerm Syntax.UnaryOp
unaryOpNeg =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "neg"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOpNot :: Phantoms.TTerm Syntax.UnaryOp
unaryOpNot =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = Core.TermUnit}}))

useDeclaration :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.UseTree -> Phantoms.TTerm Syntax.UseDeclaration
useDeclaration public tree =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm public)},
        Core.Field {
          Core.fieldName = (Core.Name "tree"),
          Core.fieldTerm = (Phantoms.unTTerm tree)}]}))

useDeclarationPublic :: Phantoms.TTerm Syntax.UseDeclaration -> Phantoms.TTerm Bool
useDeclarationPublic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseDeclaration"),
        Core.projectionField = (Core.Name "public")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

useDeclarationTree :: Phantoms.TTerm Syntax.UseDeclaration -> Phantoms.TTerm Syntax.UseTree
useDeclarationTree x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseDeclaration"),
        Core.projectionField = (Core.Name "tree")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

useDeclarationWithPublic :: Phantoms.TTerm Syntax.UseDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.UseDeclaration
useDeclarationWithPublic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tree"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseDeclaration"),
              Core.projectionField = (Core.Name "tree")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

useDeclarationWithTree :: Phantoms.TTerm Syntax.UseDeclaration -> Phantoms.TTerm Syntax.UseTree -> Phantoms.TTerm Syntax.UseDeclaration
useDeclarationWithTree original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "public"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseDeclaration"),
              Core.projectionField = (Core.Name "public")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tree"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

useGroup :: Phantoms.TTerm [String] -> Phantoms.TTerm [Syntax.UseTree] -> Phantoms.TTerm Syntax.UseGroup
useGroup prefix trees =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "trees"),
          Core.fieldTerm = (Phantoms.unTTerm trees)}]}))

useGroupPrefix :: Phantoms.TTerm Syntax.UseGroup -> Phantoms.TTerm [String]
useGroupPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseGroup"),
        Core.projectionField = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

useGroupTrees :: Phantoms.TTerm Syntax.UseGroup -> Phantoms.TTerm [Syntax.UseTree]
useGroupTrees x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseGroup"),
        Core.projectionField = (Core.Name "trees")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

useGroupWithPrefix :: Phantoms.TTerm Syntax.UseGroup -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.UseGroup
useGroupWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trees"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseGroup"),
              Core.projectionField = (Core.Name "trees")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

useGroupWithTrees :: Phantoms.TTerm Syntax.UseGroup -> Phantoms.TTerm [Syntax.UseTree] -> Phantoms.TTerm Syntax.UseGroup
useGroupWithTrees original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseGroup"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trees"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

usePath :: Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.UsePath
usePath segments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UsePath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Phantoms.unTTerm segments)}]}))

usePathSegments :: Phantoms.TTerm Syntax.UsePath -> Phantoms.TTerm [String]
usePathSegments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UsePath"),
        Core.projectionField = (Core.Name "segments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

usePathWithSegments :: Phantoms.TTerm Syntax.UsePath -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.UsePath
usePathWithSegments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UsePath"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

useRename :: Phantoms.TTerm [String] -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.UseRename
useRename path alias =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseRename"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm alias)}]}))

useRenameAlias :: Phantoms.TTerm Syntax.UseRename -> Phantoms.TTerm String
useRenameAlias x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseRename"),
        Core.projectionField = (Core.Name "alias")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

useRenamePath :: Phantoms.TTerm Syntax.UseRename -> Phantoms.TTerm [String]
useRenamePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseRename"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

useRenameWithAlias :: Phantoms.TTerm Syntax.UseRename -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.UseRename
useRenameWithAlias original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseRename"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseRename"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

useRenameWithPath :: Phantoms.TTerm Syntax.UseRename -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.UseRename
useRenameWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.UseRename"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.UseRename"),
              Core.projectionField = (Core.Name "alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

useTreeGlob :: Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.UseTree
useTreeGlob x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.UseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "glob"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

useTreeGroup :: Phantoms.TTerm Syntax.UseGroup -> Phantoms.TTerm Syntax.UseTree
useTreeGroup x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.UseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

useTreePath :: Phantoms.TTerm Syntax.UsePath -> Phantoms.TTerm Syntax.UseTree
useTreePath x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.UseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

useTreeRename :: Phantoms.TTerm Syntax.UseRename -> Phantoms.TTerm Syntax.UseTree
useTreeRename x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.UseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

visibilityCrate :: Phantoms.TTerm Syntax.Visibility
visibilityCrate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Visibility"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "crate"),
        Core.fieldTerm = Core.TermUnit}}))

visibilityPrivate :: Phantoms.TTerm Syntax.Visibility
visibilityPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Visibility"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

visibilityPublic :: Phantoms.TTerm Syntax.Visibility
visibilityPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Visibility"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

visibilityRestricted :: Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.Visibility
visibilityRestricted x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rust.syntax.Visibility"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "restricted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

whereClause :: Phantoms.TTerm [Syntax.WherePredicate] -> Phantoms.TTerm Syntax.WhereClause
whereClause predicates =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WhereClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicates"),
          Core.fieldTerm = (Phantoms.unTTerm predicates)}]}))

whereClausePredicates :: Phantoms.TTerm Syntax.WhereClause -> Phantoms.TTerm [Syntax.WherePredicate]
whereClausePredicates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhereClause"),
        Core.projectionField = (Core.Name "predicates")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whereClauseWithPredicates :: Phantoms.TTerm Syntax.WhereClause -> Phantoms.TTerm [Syntax.WherePredicate] -> Phantoms.TTerm Syntax.WhereClause
whereClauseWithPredicates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WhereClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

wherePredicate :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.WherePredicate
wherePredicate type_ bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WherePredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))

wherePredicateBounds :: Phantoms.TTerm Syntax.WherePredicate -> Phantoms.TTerm [Syntax.TypeParamBound]
wherePredicateBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WherePredicate"),
        Core.projectionField = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wherePredicateType :: Phantoms.TTerm Syntax.WherePredicate -> Phantoms.TTerm Syntax.Type
wherePredicateType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WherePredicate"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wherePredicateWithBounds :: Phantoms.TTerm Syntax.WherePredicate -> Phantoms.TTerm [Syntax.TypeParamBound] -> Phantoms.TTerm Syntax.WherePredicate
wherePredicateWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WherePredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WherePredicate"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

wherePredicateWithType :: Phantoms.TTerm Syntax.WherePredicate -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.WherePredicate
wherePredicateWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WherePredicate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WherePredicate"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

whileExpr :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.IfCondition -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WhileExpr
whileExpr label condition body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

whileExprBody :: Phantoms.TTerm Syntax.WhileExpr -> Phantoms.TTerm Syntax.Block
whileExprBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileExprCondition :: Phantoms.TTerm Syntax.WhileExpr -> Phantoms.TTerm Syntax.IfCondition
whileExprCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
        Core.projectionField = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileExprLabel :: Phantoms.TTerm Syntax.WhileExpr -> Phantoms.TTerm (Maybe String)
whileExprLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileExprWithBody :: Phantoms.TTerm Syntax.WhileExpr -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WhileExpr
whileExprWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

whileExprWithCondition :: Phantoms.TTerm Syntax.WhileExpr -> Phantoms.TTerm Syntax.IfCondition -> Phantoms.TTerm Syntax.WhileExpr
whileExprWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

whileExprWithLabel :: Phantoms.TTerm Syntax.WhileExpr -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.WhileExpr
whileExprWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rust.syntax.WhileExpr"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
