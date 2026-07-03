-- Note: this is an automatically generated file. Do not edit.

-- | Effectful test cases for hydra.lib.effects primitives

module Hydra.Test.Lib.Effects where

import qualified Hydra.Core as Core
import qualified Hydra.Overlay.Haskell.Lib.Effects as Effects
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Effectful test cases for hydra.lib.effects primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.effects primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "apply",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "apply applies an effectful function to an effectful argument",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.apply (Effects.pure (\s -> Strings.cat2 ">" s)) (Effects.pure "y")),
                Testing.effectfulTestCaseExpected = (\_ -> ">y")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "bind",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bind threads a pure value into the next effect",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Effects.pure "abc") (\s -> Effects.pure (Strings.cat2 s "!"))),
                Testing.effectfulTestCaseExpected = (\_ -> "abc!")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "compose",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "compose runs two Kleisli arrows in sequence",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.compose (\a -> Effects.pure (Strings.cat2 a "1")) (\b -> Effects.pure (Strings.cat2 b "2")) "n"),
                Testing.effectfulTestCaseExpected = (\_ -> "n12")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "foldl",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "foldl sequences an effect-returning step over a list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.foldl (\acc -> \x -> Effects.pure (Strings.cat2 acc x)) "" [
                  "a",
                  "b",
                  "c"]),
                Testing.effectfulTestCaseExpected = (\_ -> "abc")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "map",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map applies a pure function to an effect result",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\s -> Strings.cat2 s "-mapped") (Effects.pure "x")),
                Testing.effectfulTestCaseExpected = (\_ -> "x-mapped")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "mapList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mapList applies an effect-returning function across a list and collects results",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\xs -> Strings.cat xs) (Effects.mapList (\x -> Effects.pure (Strings.cat2 x ".")) [
                  "a",
                  "b"])),
                Testing.effectfulTestCaseExpected = (\_ -> "a.b.")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "mapOptional",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mapOptional over a present value applies the function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\m -> Optionals.fromOptional "<none>" m) (Effects.mapOptional (\x -> Effects.pure (Strings.cat2 x "!")) (Just "present"))),
                Testing.effectfulTestCaseExpected = (\_ -> "present!")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mapOptional over none yields none",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\m -> Optionals.fromOptional "<none>" m) (Effects.mapOptional (\x -> Effects.pure x) Nothing)),
                Testing.effectfulTestCaseExpected = (\_ -> "<none>")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "pure",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "pure of a string yields the string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.pure "hello"),
                Testing.effectfulTestCaseExpected = (\_ -> "hello")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
