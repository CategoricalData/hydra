-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.strings namespace.

module Hydra.Lib.Strings where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
cat :: Packaging.PrimitiveDefinition
cat =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.cat"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeString)}},
      Packaging.primitiveDefinitionDescription = "Concatenate a list of strings into a single string.",
      Packaging.primitiveDefinitionComments = (Just "cat(xs) returns the string formed by concatenating every string in xs in order. Total. Corresponds to Haskell's concat :: [String] -> String."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
cat2 :: Packaging.PrimitiveDefinition
cat2 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.cat2"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeString)}},
      Packaging.primitiveDefinitionDescription = "Concatenate two strings.",
      Packaging.primitiveDefinitionComments = (Just "cat2(s, t) returns the concatenation of s and t. Total. Corresponds to Haskell's (++) :: String -> String -> String."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
fromList :: Packaging.PrimitiveDefinition
fromList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.fromList"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeString)}},
      Packaging.primitiveDefinitionDescription = "Convert a list of Unicode code points to a string.",
      Packaging.primitiveDefinitionComments = (Just "fromList(cs) returns the string whose characters are the Unicode code points in cs, in order. Code points outside the valid Unicode range [0, 0x10FFFF] yield a host-defined result (typically substitution with U+FFFD or truncation of the bits). Total. The inverse of toList."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
intercalate :: Packaging.PrimitiveDefinition
intercalate =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.intercalate"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeString)}},
      Packaging.primitiveDefinitionDescription = "Join a list of strings with a separator between each element.",
      Packaging.primitiveDefinitionComments = (Just "intercalate(sep, xs) returns the strings in xs concatenated with sep inserted between each pair of adjacent strings; for the empty list the result is the empty string, and for a singleton list the result is the single string. Total. Corresponds to Haskell's Data.List.intercalate :: String -> [String] -> String."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
length :: Packaging.PrimitiveDefinition
length =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.length"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "Return the length of a string.",
      Packaging.primitiveDefinitionComments = (Just "length(s) returns the number of Unicode code points in s as an int32. Note: this is the code-point count, not the byte count or the grapheme-cluster count, so a four-byte UTF-8 character counts as one and an emoji built from multiple code points counts as the number of code points it uses. Total on strings shorter than 2^31-1 code points."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
lines :: Packaging.PrimitiveDefinition
lines =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.lines"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}},
      Packaging.primitiveDefinitionDescription = "Split a string into lines.",
      Packaging.primitiveDefinitionComments = (Just "lines(s) splits s into a list of lines, splitting on newline characters (U+000A). The trailing newline is consumed but does not produce an empty trailing element (matching Haskell's lines behavior). Total. Corresponds to Haskell's lines :: String -> [String]."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
maybeCharAt :: Packaging.PrimitiveDefinition
maybeCharAt =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.maybeCharAt"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds.",
      Packaging.primitiveDefinitionComments = (Just "maybeCharAt(i, s) returns Just(c) where c is the Unicode code point at position i in s, or Nothing if i is negative or i >= length(s). Total."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
null :: Packaging.PrimitiveDefinition
null =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.null"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionDescription = "Check whether a string is empty.",
      Packaging.primitiveDefinitionComments = (Just "null(s) returns true iff s is the empty string. Total. Corresponds to Haskell's null :: String -> Bool."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
splitOn :: Packaging.PrimitiveDefinition
splitOn =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.splitOn"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}},
      Packaging.primitiveDefinitionDescription = "Split a string on a delimiter string.",
      Packaging.primitiveDefinitionComments = (Just "splitOn(sep, s) returns the list of substrings of s obtained by splitting on every occurrence of the non-empty delimiter sep. Adjacent or boundary delimiters produce empty-string elements in the result. Behavior when sep is empty is host-defined and should not be relied upon. Total. Corresponds to Haskell's Data.List.Split.splitOn :: String -> String -> [String]."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
toList :: Packaging.PrimitiveDefinition
toList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.toList"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "Convert a string to a list of Unicode code points.",
      Packaging.primitiveDefinitionComments = (Just "toList(s) returns the list of Unicode code points making up s, in order. Each code point is represented as an int32. Total. The inverse of fromList."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
toLower :: Packaging.PrimitiveDefinition
toLower =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.toLower"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeString)}},
      Packaging.primitiveDefinitionDescription = "Convert a string to lowercase.",
      Packaging.primitiveDefinitionComments = (Just "toLower(s) returns s with each character replaced by its Unicode simple (one-to-one) lowercase mapping, or itself if it has no lowercase mapping. This is a code-point-by-code-point operation, so it does not handle the string-changing cases of full Unicode case folding (e.g. U+00DF \"\223\" does not lowercase to \"ss\"; it returns itself). For text intended for human-readable display in locales with non-trivial case mappings, prefer a host-specific full case-folding API. Total."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
toUpper :: Packaging.PrimitiveDefinition
toUpper =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.toUpper"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeString),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeString)}},
      Packaging.primitiveDefinitionDescription = "Convert a string to uppercase.",
      Packaging.primitiveDefinitionComments = (Just "toUpper(s) returns s with each character replaced by its Unicode simple (one-to-one) uppercase mapping, or itself if it has no uppercase mapping. This is a code-point-by-code-point operation, so it does not handle the string-changing cases of full Unicode case folding (e.g. U+00DF \"\223\" does not uppercase to \"SS\"; it returns itself). For text intended for human-readable display in locales with non-trivial case mappings, prefer a host-specific full case-folding API. Total."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
unlines :: Packaging.PrimitiveDefinition
unlines =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.strings.unlines"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeString)}},
      Packaging.primitiveDefinitionDescription = "Join a list of strings with newlines, appending a trailing newline.",
      Packaging.primitiveDefinitionComments = (Just "unlines(xs) returns the concatenation of every string in xs with a newline character (U+000A) appended after each, including the last. The inverse of lines for normalized input. Total. Corresponds to Haskell's unlines :: [String] -> String."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
