-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.wasm.syntax

module Hydra.Dsl.Wasm.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Wasm.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Int as I

blockInstruction :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.BlockType -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.BlockInstruction
blockInstruction label blockType body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Phantoms.unTTerm blockType)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

blockInstructionBlockType :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm Syntax.BlockType
blockInstructionBlockType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
        Core.projectionField = (Core.Name "blockType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockInstructionBody :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm [Syntax.Instruction]
blockInstructionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockInstructionLabel :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm (Maybe String)
blockInstructionLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockInstructionWithBlockType :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm Syntax.BlockType -> Phantoms.TTerm Syntax.BlockInstruction
blockInstructionWithBlockType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

blockInstructionWithBody :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.BlockInstruction
blockInstructionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
              Core.projectionField = (Core.Name "blockType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

blockInstructionWithLabel :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.BlockInstruction
blockInstructionWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
              Core.projectionField = (Core.Name "blockType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BlockInstruction"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

blockTypeEmpty :: Phantoms.TTerm Syntax.BlockType
blockTypeEmpty =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.BlockType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))

blockTypeTypeUse :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm Syntax.BlockType
blockTypeTypeUse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.BlockType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeUse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

blockTypeValue :: Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.BlockType
blockTypeValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.BlockType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

brTableArgs :: Phantoms.TTerm [String] -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.BrTableArgs
brTableArgs labels default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.BrTableArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm labels)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))

brTableArgsDefault :: Phantoms.TTerm Syntax.BrTableArgs -> Phantoms.TTerm String
brTableArgsDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BrTableArgs"),
        Core.projectionField = (Core.Name "default")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

brTableArgsLabels :: Phantoms.TTerm Syntax.BrTableArgs -> Phantoms.TTerm [String]
brTableArgsLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BrTableArgs"),
        Core.projectionField = (Core.Name "labels")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

brTableArgsWithDefault :: Phantoms.TTerm Syntax.BrTableArgs -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.BrTableArgs
brTableArgsWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.BrTableArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BrTableArgs"),
              Core.projectionField = (Core.Name "labels")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

brTableArgsWithLabels :: Phantoms.TTerm Syntax.BrTableArgs -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.BrTableArgs
brTableArgsWithLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.BrTableArgs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.BrTableArgs"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constValueF32 :: Phantoms.TTerm Float -> Phantoms.TTerm Syntax.ConstValue
constValueF32 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ConstValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "f32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constValueF64 :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.ConstValue
constValueF64 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ConstValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "f64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constValueI32 :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.ConstValue
constValueI32 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ConstValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "i32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constValueI64 :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.ConstValue
constValueI64 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ConstValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "i64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataModeActive :: Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.DataMode
dataModeActive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.DataMode"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "active"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataModePassive :: Phantoms.TTerm Syntax.DataMode
dataModePassive =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.DataMode"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "passive"),
        Core.fieldTerm = Core.TermUnit}}))

dataSegment :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DataMode -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DataSegment
dataSegment name mode bytes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Phantoms.unTTerm mode)},
        Core.Field {
          Core.fieldName = (Core.Name "bytes"),
          Core.fieldTerm = (Phantoms.unTTerm bytes)}]}))

dataSegmentBytes :: Phantoms.TTerm Syntax.DataSegment -> Phantoms.TTerm String
dataSegmentBytes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
        Core.projectionField = (Core.Name "bytes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataSegmentMode :: Phantoms.TTerm Syntax.DataSegment -> Phantoms.TTerm Syntax.DataMode
dataSegmentMode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
        Core.projectionField = (Core.Name "mode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataSegmentName :: Phantoms.TTerm Syntax.DataSegment -> Phantoms.TTerm (Maybe String)
dataSegmentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataSegmentWithBytes :: Phantoms.TTerm Syntax.DataSegment -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DataSegment
dataSegmentWithBytes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
              Core.projectionField = (Core.Name "mode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bytes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataSegmentWithMode :: Phantoms.TTerm Syntax.DataSegment -> Phantoms.TTerm Syntax.DataMode -> Phantoms.TTerm Syntax.DataSegment
dataSegmentWithMode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bytes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
              Core.projectionField = (Core.Name "bytes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataSegmentWithName :: Phantoms.TTerm Syntax.DataSegment -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DataSegment
dataSegmentWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
              Core.projectionField = (Core.Name "mode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bytes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.DataSegment"),
              Core.projectionField = (Core.Name "bytes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elemActive :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.ElemActive
elemActive table offset =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemActive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "table"),
          Core.fieldTerm = (Phantoms.unTTerm table)},
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Phantoms.unTTerm offset)}]}))

elemActiveOffset :: Phantoms.TTerm Syntax.ElemActive -> Phantoms.TTerm [Syntax.Instruction]
elemActiveOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemActive"),
        Core.projectionField = (Core.Name "offset")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elemActiveTable :: Phantoms.TTerm Syntax.ElemActive -> Phantoms.TTerm String
elemActiveTable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemActive"),
        Core.projectionField = (Core.Name "table")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elemActiveWithOffset :: Phantoms.TTerm Syntax.ElemActive -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.ElemActive
elemActiveWithOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemActive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "table"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemActive"),
              Core.projectionField = (Core.Name "table")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elemActiveWithTable :: Phantoms.TTerm Syntax.ElemActive -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ElemActive
elemActiveWithTable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemActive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "table"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemActive"),
              Core.projectionField = (Core.Name "offset")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elemModeActive :: Phantoms.TTerm Syntax.ElemActive -> Phantoms.TTerm Syntax.ElemMode
elemModeActive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ElemMode"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "active"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elemModeDeclarative :: Phantoms.TTerm Syntax.ElemMode
elemModeDeclarative =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ElemMode"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declarative"),
        Core.fieldTerm = Core.TermUnit}}))

elemModePassive :: Phantoms.TTerm Syntax.ElemMode
elemModePassive =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ElemMode"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "passive"),
        Core.fieldTerm = Core.TermUnit}}))

elemSegment :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ElemMode -> Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm [[Syntax.Instruction]] -> Phantoms.TTerm Syntax.ElemSegment
elemSegment name mode type_ init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Phantoms.unTTerm mode)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

elemSegmentInit :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm [[Syntax.Instruction]]
elemSegmentInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elemSegmentMode :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm Syntax.ElemMode
elemSegmentMode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
        Core.projectionField = (Core.Name "mode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elemSegmentName :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm (Maybe String)
elemSegmentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elemSegmentType :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm Syntax.RefType
elemSegmentType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elemSegmentWithInit :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm [[Syntax.Instruction]] -> Phantoms.TTerm Syntax.ElemSegment
elemSegmentWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "mode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elemSegmentWithMode :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm Syntax.ElemMode -> Phantoms.TTerm Syntax.ElemSegment
elemSegmentWithMode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elemSegmentWithName :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ElemSegment
elemSegmentWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "mode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elemSegmentWithType :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.ElemSegment
elemSegmentWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "mode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ElemSegment"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exportDef :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ExportDesc -> Phantoms.TTerm Syntax.ExportDef
exportDef name desc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ExportDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "desc"),
          Core.fieldTerm = (Phantoms.unTTerm desc)}]}))

exportDefDesc :: Phantoms.TTerm Syntax.ExportDef -> Phantoms.TTerm Syntax.ExportDesc
exportDefDesc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDef"),
        Core.projectionField = (Core.Name "desc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportDefName :: Phantoms.TTerm Syntax.ExportDef -> Phantoms.TTerm String
exportDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportDefWithDesc :: Phantoms.TTerm Syntax.ExportDef -> Phantoms.TTerm Syntax.ExportDesc -> Phantoms.TTerm Syntax.ExportDef
exportDefWithDesc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ExportDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "desc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

exportDefWithName :: Phantoms.TTerm Syntax.ExportDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ExportDef
exportDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ExportDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "desc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDef"),
              Core.projectionField = (Core.Name "desc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exportDescFunc :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ExportDesc
exportDescFunc x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "func"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportDescGlobal :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ExportDesc
exportDescGlobal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportDescMemory :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ExportDesc
exportDescMemory x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "memory"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportDescTable :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ExportDesc
exportDescTable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ExportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "table"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

func :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm [Syntax.FuncLocal] -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.Func
func name typeUse locals body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Func"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Phantoms.unTTerm typeUse)},
        Core.Field {
          Core.fieldName = (Core.Name "locals"),
          Core.fieldTerm = (Phantoms.unTTerm locals)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

funcBody :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm [Syntax.Instruction]
funcBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcLocal :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.FuncLocal
funcLocal name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.FuncLocal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

funcLocalName :: Phantoms.TTerm Syntax.FuncLocal -> Phantoms.TTerm (Maybe String)
funcLocalName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncLocal"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcLocalType :: Phantoms.TTerm Syntax.FuncLocal -> Phantoms.TTerm Syntax.ValType
funcLocalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncLocal"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcLocalWithName :: Phantoms.TTerm Syntax.FuncLocal -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FuncLocal
funcLocalWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.FuncLocal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncLocal"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

funcLocalWithType :: Phantoms.TTerm Syntax.FuncLocal -> Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.FuncLocal
funcLocalWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.FuncLocal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncLocal"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

funcLocals :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm [Syntax.FuncLocal]
funcLocals x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
        Core.projectionField = (Core.Name "locals")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcName :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm (Maybe String)
funcName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcType :: Phantoms.TTerm [Syntax.ValType] -> Phantoms.TTerm [Syntax.ValType] -> Phantoms.TTerm Syntax.FuncType
funcType params results =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.FuncType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "results"),
          Core.fieldTerm = (Phantoms.unTTerm results)}]}))

funcTypeParams :: Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm [Syntax.ValType]
funcTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncType"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcTypeRefIndex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.FuncTypeRef
funcTypeRefIndex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.FuncTypeRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

funcTypeRefInline :: Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm Syntax.FuncTypeRef
funcTypeRefInline x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.FuncTypeRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inline"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

funcTypeResults :: Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm [Syntax.ValType]
funcTypeResults x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncType"),
        Core.projectionField = (Core.Name "results")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcTypeUse :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm Syntax.TypeUse
funcTypeUse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
        Core.projectionField = (Core.Name "typeUse")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funcTypeWithParams :: Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm [Syntax.ValType] -> Phantoms.TTerm Syntax.FuncType
funcTypeWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.FuncType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "results"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncType"),
              Core.projectionField = (Core.Name "results")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

funcTypeWithResults :: Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm [Syntax.ValType] -> Phantoms.TTerm Syntax.FuncType
funcTypeWithResults original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.FuncType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.FuncType"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "results"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

funcWithBody :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.Func
funcWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Func"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "typeUse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "locals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "locals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

funcWithLocals :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm [Syntax.FuncLocal] -> Phantoms.TTerm Syntax.Func
funcWithLocals original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Func"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "typeUse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "locals"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

funcWithName :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Func
funcWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Func"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "typeUse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "locals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "locals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

funcWithTypeUse :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm Syntax.Func
funcWithTypeUse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Func"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "locals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "locals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Func"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

globalDef :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.GlobalDef
globalDef name type_ init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

globalDefInit :: Phantoms.TTerm Syntax.GlobalDef -> Phantoms.TTerm [Syntax.Instruction]
globalDefInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

globalDefName :: Phantoms.TTerm Syntax.GlobalDef -> Phantoms.TTerm (Maybe String)
globalDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

globalDefType :: Phantoms.TTerm Syntax.GlobalDef -> Phantoms.TTerm Syntax.GlobalType
globalDefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

globalDefWithInit :: Phantoms.TTerm Syntax.GlobalDef -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.GlobalDef
globalDefWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

globalDefWithName :: Phantoms.TTerm Syntax.GlobalDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.GlobalDef
globalDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

globalDefWithType :: Phantoms.TTerm Syntax.GlobalDef -> Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm Syntax.GlobalDef
globalDefWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalDef"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

globalType :: Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.GlobalType
globalType valType mutable =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.GlobalType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valType"),
          Core.fieldTerm = (Phantoms.unTTerm valType)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm mutable)}]}))

globalTypeMutable :: Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm Bool
globalTypeMutable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalType"),
        Core.projectionField = (Core.Name "mutable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

globalTypeValType :: Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm Syntax.ValType
globalTypeValType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalType"),
        Core.projectionField = (Core.Name "valType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

globalTypeWithMutable :: Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.GlobalType
globalTypeWithMutable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.GlobalType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalType"),
              Core.projectionField = (Core.Name "valType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

globalTypeWithValType :: Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.GlobalType
globalTypeWithValType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.GlobalType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mutable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.GlobalType"),
              Core.projectionField = (Core.Name "mutable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifInstruction :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.BlockType -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.IfInstruction
ifInstruction label blockType then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Phantoms.unTTerm blockType)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))

ifInstructionBlockType :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm Syntax.BlockType
ifInstructionBlockType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
        Core.projectionField = (Core.Name "blockType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifInstructionElse :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm [Syntax.Instruction]
ifInstructionElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
        Core.projectionField = (Core.Name "else")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifInstructionLabel :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm (Maybe String)
ifInstructionLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifInstructionThen :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm [Syntax.Instruction]
ifInstructionThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
        Core.projectionField = (Core.Name "then")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifInstructionWithBlockType :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm Syntax.BlockType -> Phantoms.TTerm Syntax.IfInstruction
ifInstructionWithBlockType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifInstructionWithElse :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.IfInstruction
ifInstructionWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "blockType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifInstructionWithLabel :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.IfInstruction
ifInstructionWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "blockType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifInstructionWithThen :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm [Syntax.Instruction] -> Phantoms.TTerm Syntax.IfInstruction
ifInstructionWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "blockType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "blockType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.IfInstruction"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importDef :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ImportDesc -> Phantoms.TTerm Syntax.ImportDef
importDef module_ name desc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "desc"),
          Core.fieldTerm = (Phantoms.unTTerm desc)}]}))

importDefDesc :: Phantoms.TTerm Syntax.ImportDef -> Phantoms.TTerm Syntax.ImportDesc
importDefDesc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
        Core.projectionField = (Core.Name "desc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDefModule :: Phantoms.TTerm Syntax.ImportDef -> Phantoms.TTerm String
importDefModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
        Core.projectionField = (Core.Name "module")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDefName :: Phantoms.TTerm Syntax.ImportDef -> Phantoms.TTerm String
importDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDefWithDesc :: Phantoms.TTerm Syntax.ImportDef -> Phantoms.TTerm Syntax.ImportDesc -> Phantoms.TTerm Syntax.ImportDef
importDefWithDesc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "desc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importDefWithModule :: Phantoms.TTerm Syntax.ImportDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ImportDef
importDefWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "desc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
              Core.projectionField = (Core.Name "desc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importDefWithName :: Phantoms.TTerm Syntax.ImportDef -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ImportDef
importDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "desc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDef"),
              Core.projectionField = (Core.Name "desc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importDescFunc :: Phantoms.TTerm Syntax.ImportFunc -> Phantoms.TTerm Syntax.ImportDesc
importDescFunc x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "func"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importDescGlobal :: Phantoms.TTerm Syntax.ImportGlobal -> Phantoms.TTerm Syntax.ImportDesc
importDescGlobal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importDescMemory :: Phantoms.TTerm Syntax.ImportMemory -> Phantoms.TTerm Syntax.ImportDesc
importDescMemory x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "memory"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importDescTable :: Phantoms.TTerm Syntax.ImportTable -> Phantoms.TTerm Syntax.ImportDesc
importDescTable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ImportDesc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "table"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importFunc :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm Syntax.ImportFunc
importFunc name typeUse =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportFunc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Phantoms.unTTerm typeUse)}]}))

importFuncName :: Phantoms.TTerm Syntax.ImportFunc -> Phantoms.TTerm (Maybe String)
importFuncName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportFunc"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importFuncTypeUse :: Phantoms.TTerm Syntax.ImportFunc -> Phantoms.TTerm Syntax.TypeUse
importFuncTypeUse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportFunc"),
        Core.projectionField = (Core.Name "typeUse")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importFuncWithName :: Phantoms.TTerm Syntax.ImportFunc -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ImportFunc
importFuncWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportFunc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportFunc"),
              Core.projectionField = (Core.Name "typeUse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importFuncWithTypeUse :: Phantoms.TTerm Syntax.ImportFunc -> Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm Syntax.ImportFunc
importFuncWithTypeUse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportFunc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportFunc"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeUse"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importGlobal :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm Syntax.ImportGlobal
importGlobal name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportGlobal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

importGlobalName :: Phantoms.TTerm Syntax.ImportGlobal -> Phantoms.TTerm (Maybe String)
importGlobalName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportGlobal"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importGlobalType :: Phantoms.TTerm Syntax.ImportGlobal -> Phantoms.TTerm Syntax.GlobalType
importGlobalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportGlobal"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importGlobalWithName :: Phantoms.TTerm Syntax.ImportGlobal -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ImportGlobal
importGlobalWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportGlobal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportGlobal"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importGlobalWithType :: Phantoms.TTerm Syntax.ImportGlobal -> Phantoms.TTerm Syntax.GlobalType -> Phantoms.TTerm Syntax.ImportGlobal
importGlobalWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportGlobal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportGlobal"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importMemory :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.ImportMemory
importMemory name limits =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportMemory"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm limits)}]}))

importMemoryLimits :: Phantoms.TTerm Syntax.ImportMemory -> Phantoms.TTerm Syntax.Limits
importMemoryLimits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportMemory"),
        Core.projectionField = (Core.Name "limits")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importMemoryName :: Phantoms.TTerm Syntax.ImportMemory -> Phantoms.TTerm (Maybe String)
importMemoryName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportMemory"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importMemoryWithLimits :: Phantoms.TTerm Syntax.ImportMemory -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.ImportMemory
importMemoryWithLimits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportMemory"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportMemory"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importMemoryWithName :: Phantoms.TTerm Syntax.ImportMemory -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ImportMemory
importMemoryWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportMemory"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportMemory"),
              Core.projectionField = (Core.Name "limits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importTable :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.ImportTable
importTable name refType limits =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Phantoms.unTTerm refType)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm limits)}]}))

importTableLimits :: Phantoms.TTerm Syntax.ImportTable -> Phantoms.TTerm Syntax.Limits
importTableLimits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
        Core.projectionField = (Core.Name "limits")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importTableName :: Phantoms.TTerm Syntax.ImportTable -> Phantoms.TTerm (Maybe String)
importTableName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importTableRefType :: Phantoms.TTerm Syntax.ImportTable -> Phantoms.TTerm Syntax.RefType
importTableRefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
        Core.projectionField = (Core.Name "refType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importTableWithLimits :: Phantoms.TTerm Syntax.ImportTable -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.ImportTable
importTableWithLimits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
              Core.projectionField = (Core.Name "refType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importTableWithName :: Phantoms.TTerm Syntax.ImportTable -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ImportTable
importTableWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
              Core.projectionField = (Core.Name "refType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
              Core.projectionField = (Core.Name "limits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importTableWithRefType :: Phantoms.TTerm Syntax.ImportTable -> Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.ImportTable
importTableWithRefType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.ImportTable"),
              Core.projectionField = (Core.Name "limits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

instructionBinop :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm Syntax.Instruction
instructionBinop x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binop"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionBlock :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm Syntax.Instruction
instructionBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionBr :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionBr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "br"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionBrIf :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionBrIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "brIf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionBrTable :: Phantoms.TTerm Syntax.BrTableArgs -> Phantoms.TTerm Syntax.Instruction
instructionBrTable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "brTable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionCall :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionCall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionCallIndirect :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm Syntax.Instruction
instructionCallIndirect x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "callIndirect"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionConst :: Phantoms.TTerm Syntax.ConstValue -> Phantoms.TTerm Syntax.Instruction
instructionConst x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionConvert :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionConvert x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "convert"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionDrop :: Phantoms.TTerm Syntax.Instruction
instructionDrop =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "drop"),
        Core.fieldTerm = Core.TermUnit}}))

instructionGlobalGet :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionGlobalGet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "globalGet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionGlobalSet :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionGlobalSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "globalSet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionIf :: Phantoms.TTerm Syntax.IfInstruction -> Phantoms.TTerm Syntax.Instruction
instructionIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionLoad :: Phantoms.TTerm Syntax.MemoryInstruction -> Phantoms.TTerm Syntax.Instruction
instructionLoad x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "load"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionLocalGet :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionLocalGet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localGet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionLocalSet :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionLocalSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localSet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionLocalTee :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionLocalTee x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localTee"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionLoop :: Phantoms.TTerm Syntax.BlockInstruction -> Phantoms.TTerm Syntax.Instruction
instructionLoop x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "loop"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionMemoryGrow :: Phantoms.TTerm Syntax.Instruction
instructionMemoryGrow =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "memoryGrow"),
        Core.fieldTerm = Core.TermUnit}}))

instructionMemorySize :: Phantoms.TTerm Syntax.Instruction
instructionMemorySize =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "memorySize"),
        Core.fieldTerm = Core.TermUnit}}))

instructionNop :: Phantoms.TTerm Syntax.Instruction
instructionNop =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nop"),
        Core.fieldTerm = Core.TermUnit}}))

instructionRaw :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Instruction
instructionRaw x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionRefIsNull :: Phantoms.TTerm Syntax.Instruction
instructionRefIsNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "refIsNull"),
        Core.fieldTerm = Core.TermUnit}}))

instructionRefNull :: Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.Instruction
instructionRefNull x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "refNull"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionRelop :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm Syntax.Instruction
instructionRelop x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "relop"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionReturn :: Phantoms.TTerm Syntax.Instruction
instructionReturn =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = Core.TermUnit}}))

instructionSelect :: Phantoms.TTerm Syntax.Instruction
instructionSelect =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = Core.TermUnit}}))

instructionStore :: Phantoms.TTerm Syntax.MemoryInstruction -> Phantoms.TTerm Syntax.Instruction
instructionStore x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "store"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionTestop :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm Syntax.Instruction
instructionTestop x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "testop"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionUnop :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm Syntax.Instruction
instructionUnop x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unop"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

instructionUnreachable :: Phantoms.TTerm Syntax.Instruction
instructionUnreachable =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.Instruction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unreachable"),
        Core.fieldTerm = Core.TermUnit}}))

limits :: Phantoms.TTerm Int -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Syntax.Limits
limits min max =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Limits"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm max)}]}))

limitsMax :: Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm (Maybe Int)
limitsMax x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Limits"),
        Core.projectionField = (Core.Name "max")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

limitsMin :: Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Int
limitsMin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Limits"),
        Core.projectionField = (Core.Name "min")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

limitsWithMax :: Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Syntax.Limits
limitsWithMax original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Limits"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Limits"),
              Core.projectionField = (Core.Name "min")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

limitsWithMin :: Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Limits
limitsWithMin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Limits"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Limits"),
              Core.projectionField = (Core.Name "max")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memArg :: Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Syntax.MemArg
memArg offset align =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Phantoms.unTTerm offset)},
        Core.Field {
          Core.fieldName = (Core.Name "align"),
          Core.fieldTerm = (Phantoms.unTTerm align)}]}))

memArgAlign :: Phantoms.TTerm Syntax.MemArg -> Phantoms.TTerm Int
memArgAlign x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemArg"),
        Core.projectionField = (Core.Name "align")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memArgOffset :: Phantoms.TTerm Syntax.MemArg -> Phantoms.TTerm Int
memArgOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemArg"),
        Core.projectionField = (Core.Name "offset")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memArgWithAlign :: Phantoms.TTerm Syntax.MemArg -> Phantoms.TTerm Int -> Phantoms.TTerm Syntax.MemArg
memArgWithAlign original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemArg"),
              Core.projectionField = (Core.Name "offset")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "align"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

memArgWithOffset :: Phantoms.TTerm Syntax.MemArg -> Phantoms.TTerm Int -> Phantoms.TTerm Syntax.MemArg
memArgWithOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "align"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemArg"),
              Core.projectionField = (Core.Name "align")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memoryDef :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.MemoryDef
memoryDef name limits =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemoryDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm limits)}]}))

memoryDefLimits :: Phantoms.TTerm Syntax.MemoryDef -> Phantoms.TTerm Syntax.Limits
memoryDefLimits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryDef"),
        Core.projectionField = (Core.Name "limits")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memoryDefName :: Phantoms.TTerm Syntax.MemoryDef -> Phantoms.TTerm (Maybe String)
memoryDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memoryDefWithLimits :: Phantoms.TTerm Syntax.MemoryDef -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.MemoryDef
memoryDefWithLimits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemoryDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

memoryDefWithName :: Phantoms.TTerm Syntax.MemoryDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.MemoryDef
memoryDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemoryDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryDef"),
              Core.projectionField = (Core.Name "limits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memoryInstruction :: Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.MemArg -> Phantoms.TTerm Syntax.MemoryInstruction
memoryInstruction type_ memArg =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemoryInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "memArg"),
          Core.fieldTerm = (Phantoms.unTTerm memArg)}]}))

memoryInstructionMemArg :: Phantoms.TTerm Syntax.MemoryInstruction -> Phantoms.TTerm Syntax.MemArg
memoryInstructionMemArg x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryInstruction"),
        Core.projectionField = (Core.Name "memArg")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memoryInstructionType :: Phantoms.TTerm Syntax.MemoryInstruction -> Phantoms.TTerm Syntax.ValType
memoryInstructionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryInstruction"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memoryInstructionWithMemArg :: Phantoms.TTerm Syntax.MemoryInstruction -> Phantoms.TTerm Syntax.MemArg -> Phantoms.TTerm Syntax.MemoryInstruction
memoryInstructionWithMemArg original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemoryInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryInstruction"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "memArg"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

memoryInstructionWithType :: Phantoms.TTerm Syntax.MemoryInstruction -> Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.MemoryInstruction
memoryInstructionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.MemoryInstruction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "memArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.MemoryInstruction"),
              Core.projectionField = (Core.Name "memArg")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

module_ :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm [Syntax.ModuleField] -> Phantoms.TTerm Syntax.Module
module_ name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

moduleFieldData :: Phantoms.TTerm Syntax.DataSegment -> Phantoms.TTerm Syntax.ModuleField
moduleFieldData x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldElem :: Phantoms.TTerm Syntax.ElemSegment -> Phantoms.TTerm Syntax.ModuleField
moduleFieldElem x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elem"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldExport :: Phantoms.TTerm Syntax.ExportDef -> Phantoms.TTerm Syntax.ModuleField
moduleFieldExport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldFunc :: Phantoms.TTerm Syntax.Func -> Phantoms.TTerm Syntax.ModuleField
moduleFieldFunc x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "func"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldGlobal :: Phantoms.TTerm Syntax.GlobalDef -> Phantoms.TTerm Syntax.ModuleField
moduleFieldGlobal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldImport :: Phantoms.TTerm Syntax.ImportDef -> Phantoms.TTerm Syntax.ModuleField
moduleFieldImport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldMemory :: Phantoms.TTerm Syntax.MemoryDef -> Phantoms.TTerm Syntax.ModuleField
moduleFieldMemory x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "memory"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldStart :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ModuleField
moduleFieldStart x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "start"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldTable :: Phantoms.TTerm Syntax.TableDef -> Phantoms.TTerm Syntax.ModuleField
moduleFieldTable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "table"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFieldType :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.ModuleField
moduleFieldType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ModuleField"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleFields :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.ModuleField]
moduleFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Module"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleName :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm (Maybe String)
moduleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Module"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleWithFields :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.ModuleField] -> Phantoms.TTerm Syntax.Module
moduleWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Module"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleWithName :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Module
moduleWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Module"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

numericOp :: Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.NumericOp
numericOp type_ name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.NumericOp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

numericOpName :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm String
numericOpName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.NumericOp"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericOpType :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm Syntax.ValType
numericOpType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.NumericOp"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericOpWithName :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.NumericOp
numericOpWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.NumericOp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.NumericOp"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

numericOpWithType :: Phantoms.TTerm Syntax.NumericOp -> Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.NumericOp
numericOpWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.NumericOp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.NumericOp"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

param :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.Param
param name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

paramName :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe String)
paramName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Param"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramType :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ValType
paramType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Param"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramWithName :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Param
paramWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Param"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paramWithType :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ValType -> Phantoms.TTerm Syntax.Param
paramWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

refTypeExternref :: Phantoms.TTerm Syntax.RefType
refTypeExternref =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "externref"),
        Core.fieldTerm = Core.TermUnit}}))

refTypeFuncref :: Phantoms.TTerm Syntax.RefType
refTypeFuncref =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "funcref"),
        Core.fieldTerm = Core.TermUnit}}))

tableDef :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.TableDef
tableDef name refType limits =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Phantoms.unTTerm refType)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm limits)}]}))

tableDefLimits :: Phantoms.TTerm Syntax.TableDef -> Phantoms.TTerm Syntax.Limits
tableDefLimits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
        Core.projectionField = (Core.Name "limits")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableDefName :: Phantoms.TTerm Syntax.TableDef -> Phantoms.TTerm (Maybe String)
tableDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableDefRefType :: Phantoms.TTerm Syntax.TableDef -> Phantoms.TTerm Syntax.RefType
tableDefRefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
        Core.projectionField = (Core.Name "refType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableDefWithLimits :: Phantoms.TTerm Syntax.TableDef -> Phantoms.TTerm Syntax.Limits -> Phantoms.TTerm Syntax.TableDef
tableDefWithLimits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
              Core.projectionField = (Core.Name "refType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tableDefWithName :: Phantoms.TTerm Syntax.TableDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TableDef
tableDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
              Core.projectionField = (Core.Name "refType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
              Core.projectionField = (Core.Name "limits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableDefWithRefType :: Phantoms.TTerm Syntax.TableDef -> Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.TableDef
tableDefWithRefType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "limits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TableDef"),
              Core.projectionField = (Core.Name "limits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDef :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm Syntax.TypeDef
typeDef name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeDefName :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm (Maybe String)
typeDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeDef"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefType :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.FuncType
typeDefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeDef"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefWithName :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeDef
typeDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeDef"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDefWithType :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm Syntax.TypeDef
typeDefWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeDef"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeUse :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm [Syntax.Param] -> Phantoms.TTerm [Syntax.ValType] -> Phantoms.TTerm Syntax.TypeUse
typeUse index params results =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm index)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "results"),
          Core.fieldTerm = (Phantoms.unTTerm results)}]}))

typeUseIndex :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm (Maybe String)
typeUseIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
        Core.projectionField = (Core.Name "index")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeUseParams :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm [Syntax.Param]
typeUseParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeUseResults :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm [Syntax.ValType]
typeUseResults x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
        Core.projectionField = (Core.Name "results")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeUseWithIndex :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeUse
typeUseWithIndex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "results"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
              Core.projectionField = (Core.Name "results")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeUseWithParams :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm [Syntax.Param] -> Phantoms.TTerm Syntax.TypeUse
typeUseWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
              Core.projectionField = (Core.Name "index")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "results"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
              Core.projectionField = (Core.Name "results")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeUseWithResults :: Phantoms.TTerm Syntax.TypeUse -> Phantoms.TTerm [Syntax.ValType] -> Phantoms.TTerm Syntax.TypeUse
typeUseWithResults original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
              Core.projectionField = (Core.Name "index")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.wasm.syntax.TypeUse"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "results"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

valTypeExternref :: Phantoms.TTerm Syntax.ValType
valTypeExternref =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ValType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "externref"),
        Core.fieldTerm = Core.TermUnit}}))

valTypeF32 :: Phantoms.TTerm Syntax.ValType
valTypeF32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ValType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "f32"),
        Core.fieldTerm = Core.TermUnit}}))

valTypeF64 :: Phantoms.TTerm Syntax.ValType
valTypeF64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ValType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "f64"),
        Core.fieldTerm = Core.TermUnit}}))

valTypeFuncref :: Phantoms.TTerm Syntax.ValType
valTypeFuncref =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ValType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "funcref"),
        Core.fieldTerm = Core.TermUnit}}))

valTypeI32 :: Phantoms.TTerm Syntax.ValType
valTypeI32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ValType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "i32"),
        Core.fieldTerm = Core.TermUnit}}))

valTypeI64 :: Phantoms.TTerm Syntax.ValType
valTypeI64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.wasm.syntax.ValType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "i64"),
        Core.fieldTerm = Core.TermUnit}}))
