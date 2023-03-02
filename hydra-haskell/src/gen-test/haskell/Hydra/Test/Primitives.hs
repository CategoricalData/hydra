-- | Test cases for primitive functions

module Hydra.Test.Primitives where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Data.List
import Data.Map
import Data.Set

primitiveFunctionTestCases :: Testing.TestGroup Compute.Kv
primitiveFunctionTestCases = Testing.TestGroup {
  Testing.testGroupName = "All primitives",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "hydra/lib/strings primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "cat",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.cat"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "one"),
                  Core.TermLiteral (Core.LiteralString "two"),
                  (Core.TermLiteral (Core.LiteralString "three"))])})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralString "onetwothree"))},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.cat"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString ""),
                  Core.TermLiteral (Core.LiteralString "one"),
                  Core.TermLiteral (Core.LiteralString ""),
                  (Core.TermLiteral (Core.LiteralString ""))])})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralString "one"))},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.cat"))),
                Core.applicationArgument = (Core.TermList [])})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralString ""))}]},
        Testing.TestGroup {
          Testing.testGroupName = "length",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.length"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.length"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.length"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "one"))})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}]},
        Testing.TestGroup {
          Testing.testGroupName = "splitOn",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ss"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Mississippi"))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "Mi"),
                Core.TermLiteral (Core.LiteralString "i"),
                (Core.TermLiteral (Core.LiteralString "ippi"))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Mississippi"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Mississippi"))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                (Core.TermLiteral (Core.LiteralString ""))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "one two three"))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString "two"),
                (Core.TermLiteral (Core.LiteralString "three"))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " one two three "))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString "two"),
                Core.TermLiteral (Core.LiteralString "three"),
                (Core.TermLiteral (Core.LiteralString ""))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "  one two three"))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString "two"),
                (Core.TermLiteral (Core.LiteralString "three"))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "  "))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "  one two three"))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                (Core.TermLiteral (Core.LiteralString "one two three"))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "aa"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "aaa"))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                (Core.TermLiteral (Core.LiteralString "a"))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "")])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Testing.testCaseOutput = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "")])}]},
        Testing.TestGroup {
          Testing.testGroupName = "toLower",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.toLower"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "One TWO threE"))})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralString "one two three"))},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.toLower"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Abc123"))})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralString "abc123"))}]},
        Testing.TestGroup {
          Testing.testGroupName = "toUpper",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.toUpper"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "One TWO threE"))})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralString "ONE TWO THREE"))},
            Testing.TestCase {
              Testing.testCaseDescription = Nothing,
              Testing.testCaseInput = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra/lib/strings.toUpper"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Abc123"))})),
              Testing.testCaseOutput = (Core.TermLiteral (Core.LiteralString "ABC123"))}]}],
      Testing.testGroupCases = []}],
  Testing.testGroupCases = []}