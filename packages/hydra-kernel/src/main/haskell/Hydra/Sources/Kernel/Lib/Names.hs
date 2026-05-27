-- | Namespace and primitive-name constants for the Hydra standard library.
--
-- This module is a Haskell-side derived index of the primitive names declared
-- in the per-namespace primitive modules (Hydra.Sources.Kernel.Lib.Chars,
-- Hydra.Sources.Kernel.Lib.Logic, ..., one module per hydra.lib.<sub>
-- namespace). The canonical source of truth for every primitive's name and
-- signature is its PrimitiveDefinition in those modules; this module merely
-- exposes per-primitive Name constants built from `qname namespace localName`.
--
-- This module is NOT emitted as a hydra.lib.names kernel module - it has no
-- `module_ :: Module` declaration. It exists purely so kernel/coder DSL
-- source files (and host-side primN registrations) can reference primitive
-- names without re-typing the qualified-string form. Each constant has type
-- `Name` (not `TTermDefinition Name`), matching the type expected by
-- `prim1` / `prim2` / `prim3` and `primitive1` / `primitive2` / `primitive3`.
--
-- The namespace constants (`chars`, `eithers`, ...) are declared here as
-- plain `ModuleName` literals rather than re-exports of `LibChars.ns` etc.,
-- to avoid a module-import cycle: `Lib/<Sub>.hs` modules import DSL wrappers
-- (`Hydra.Dsl.Meta.Lib.<Sub>`) which import `Hydra.Sources.Libraries` which
-- imports this module. If you change a namespace string here, change the
-- matching `ns = ModuleName "..."` in the corresponding `Lib/<Sub>.hs`.

module Hydra.Sources.Kernel.Lib.Names where

import Hydra.Kernel (Name, ModuleName(..))
import Hydra.Names (qname)


-- Namespace constants

chars, eithers, equality, lists, literals, logic, maps, math, maybes, pairs, regex, sets, strings, typeclass :: ModuleName
chars     = ModuleName "hydra.lib.chars"
eithers   = ModuleName "hydra.lib.eithers"
equality  = ModuleName "hydra.lib.equality"
lists     = ModuleName "hydra.lib.lists"
literals  = ModuleName "hydra.lib.literals"
logic     = ModuleName "hydra.lib.logic"
maps      = ModuleName "hydra.lib.maps"
math      = ModuleName "hydra.lib.math"
maybes    = ModuleName "hydra.lib.maybes"
pairs     = ModuleName "hydra.lib.pairs"
regex     = ModuleName "hydra.lib.regex"
sets      = ModuleName "hydra.lib.sets"
strings   = ModuleName "hydra.lib.strings"
-- The hydra.typeclass namespace is referenced by legacy aliases in
-- Hydra.Sources.Libraries (_typeclass_Eq, _typeclass_Ord); it has no
-- associated primitive registry module.
typeclass = ModuleName "hydra.typeclass"


-- Primitive Name constants


-- chars
charsIsAlphaNum                  :: Name
charsIsAlphaNum                  = qname chars "isAlphaNum"
charsIsLower                     :: Name
charsIsLower                     = qname chars "isLower"
charsIsSpace                     :: Name
charsIsSpace                     = qname chars "isSpace"
charsIsUpper                     :: Name
charsIsUpper                     = qname chars "isUpper"
charsToLower                     :: Name
charsToLower                     = qname chars "toLower"
charsToUpper                     :: Name
charsToUpper                     = qname chars "toUpper"

-- eithers
eithersBimap                     :: Name
eithersBimap                     = qname eithers "bimap"
eithersBind                      :: Name
eithersBind                      = qname eithers "bind"
eithersEither                    :: Name
eithersEither                    = qname eithers "either"
eithersFoldl                     :: Name
eithersFoldl                     = qname eithers "foldl"
eithersFromLeft                  :: Name
eithersFromLeft                  = qname eithers "fromLeft"
eithersFromRight                 :: Name
eithersFromRight                 = qname eithers "fromRight"
eithersIsLeft                    :: Name
eithersIsLeft                    = qname eithers "isLeft"
eithersIsRight                   :: Name
eithersIsRight                   = qname eithers "isRight"
eithersLefts                     :: Name
eithersLefts                     = qname eithers "lefts"
eithersMapList                   :: Name
eithersMapList                   = qname eithers "mapList"
eithersMapMaybe                  :: Name
eithersMapMaybe                  = qname eithers "mapMaybe"
eithersMapSet                    :: Name
eithersMapSet                    = qname eithers "mapSet"
eithersMap                       :: Name
eithersMap                       = qname eithers "map"
eithersPartitionEithers          :: Name
eithersPartitionEithers          = qname eithers "partitionEithers"
eithersRights                    :: Name
eithersRights                    = qname eithers "rights"

-- equality
equalityCompare                  :: Name
equalityCompare                  = qname equality "compare"
equalityEqual                    :: Name
equalityEqual                    = qname equality "equal"
equalityGte                      :: Name
equalityGte                      = qname equality "gte"
equalityGt                       :: Name
equalityGt                       = qname equality "gt"
equalityIdentity                 :: Name
equalityIdentity                 = qname equality "identity"
equalityLte                      :: Name
equalityLte                      = qname equality "lte"
equalityLt                       :: Name
equalityLt                       = qname equality "lt"
equalityMax                      :: Name
equalityMax                      = qname equality "max"
equalityMin                      :: Name
equalityMin                      = qname equality "min"

-- lists
listsApply                       :: Name
listsApply                       = qname lists "apply"
listsBind                        :: Name
listsBind                        = qname lists "bind"
listsConcat2                     :: Name
listsConcat2                     = qname lists "concat2"
listsConcat                      :: Name
listsConcat                      = qname lists "concat"
listsCons                        :: Name
listsCons                        = qname lists "cons"
listsDropWhile                   :: Name
listsDropWhile                   = qname lists "dropWhile"
listsDrop                        :: Name
listsDrop                        = qname lists "drop"
listsElem                        :: Name
listsElem                        = qname lists "elem"
listsFilter                      :: Name
listsFilter                      = qname lists "filter"
listsFind                        :: Name
listsFind                        = qname lists "find"
listsFoldl                       :: Name
listsFoldl                       = qname lists "foldl"
listsFoldr                       :: Name
listsFoldr                       = qname lists "foldr"
listsGroup                       :: Name
listsGroup                       = qname lists "group"
listsIntercalate                 :: Name
listsIntercalate                 = qname lists "intercalate"
listsIntersperse                 :: Name
listsIntersperse                 = qname lists "intersperse"
listsLength                      :: Name
listsLength                      = qname lists "length"
listsMap                         :: Name
listsMap                         = qname lists "map"
listsMaybeAt                     :: Name
listsMaybeAt                     = qname lists "maybeAt"
listsMaybeHead                   :: Name
listsMaybeHead                   = qname lists "maybeHead"
listsMaybeInit                   :: Name
listsMaybeInit                   = qname lists "maybeInit"
listsMaybeLast                   :: Name
listsMaybeLast                   = qname lists "maybeLast"
listsMaybeTail                   :: Name
listsMaybeTail                   = qname lists "maybeTail"
listsNub                         :: Name
listsNub                         = qname lists "nub"
listsNull                        :: Name
listsNull                        = qname lists "null"
listsPartition                   :: Name
listsPartition                   = qname lists "partition"
listsPure                        :: Name
listsPure                        = qname lists "pure"
listsReplicate                   :: Name
listsReplicate                   = qname lists "replicate"
listsReverse                     :: Name
listsReverse                     = qname lists "reverse"
listsSingleton                   :: Name
listsSingleton                   = qname lists "singleton"
listsSortOn                      :: Name
listsSortOn                      = qname lists "sortOn"
listsSort                        :: Name
listsSort                        = qname lists "sort"
listsSpan                        :: Name
listsSpan                        = qname lists "span"
listsTake                        :: Name
listsTake                        = qname lists "take"
listsTranspose                   :: Name
listsTranspose                   = qname lists "transpose"
listsUncons                      :: Name
listsUncons                      = qname lists "uncons"
listsZipWith                     :: Name
listsZipWith                     = qname lists "zipWith"
listsZip                         :: Name
listsZip                         = qname lists "zip"

-- literals
literalsBigintToDecimal          :: Name
literalsBigintToDecimal          = qname literals "bigintToDecimal"
literalsBigintToInt16            :: Name
literalsBigintToInt16            = qname literals "bigintToInt16"
literalsBigintToInt32            :: Name
literalsBigintToInt32            = qname literals "bigintToInt32"
literalsBigintToInt64            :: Name
literalsBigintToInt64            = qname literals "bigintToInt64"
literalsBigintToInt8             :: Name
literalsBigintToInt8             = qname literals "bigintToInt8"
literalsBigintToUint16           :: Name
literalsBigintToUint16           = qname literals "bigintToUint16"
literalsBigintToUint32           :: Name
literalsBigintToUint32           = qname literals "bigintToUint32"
literalsBigintToUint64           :: Name
literalsBigintToUint64           = qname literals "bigintToUint64"
literalsBigintToUint8            :: Name
literalsBigintToUint8            = qname literals "bigintToUint8"
literalsBinaryToBytes            :: Name
literalsBinaryToBytes            = qname literals "binaryToBytes"
literalsBinaryToString           :: Name
literalsBinaryToString           = qname literals "binaryToString"
literalsDecimalToBigint          :: Name
literalsDecimalToBigint          = qname literals "decimalToBigint"
literalsDecimalToFloat32         :: Name
literalsDecimalToFloat32         = qname literals "decimalToFloat32"
literalsDecimalToFloat64         :: Name
literalsDecimalToFloat64         = qname literals "decimalToFloat64"
literalsFloat32ToDecimal         :: Name
literalsFloat32ToDecimal         = qname literals "float32ToDecimal"
literalsFloat32ToFloat64         :: Name
literalsFloat32ToFloat64         = qname literals "float32ToFloat64"
literalsFloat64ToDecimal         :: Name
literalsFloat64ToDecimal         = qname literals "float64ToDecimal"
literalsFloat64ToFloat32         :: Name
literalsFloat64ToFloat32         = qname literals "float64ToFloat32"
literalsInt16ToBigint            :: Name
literalsInt16ToBigint            = qname literals "int16ToBigint"
literalsInt32ToBigint            :: Name
literalsInt32ToBigint            = qname literals "int32ToBigint"
literalsInt64ToBigint            :: Name
literalsInt64ToBigint            = qname literals "int64ToBigint"
literalsInt8ToBigint             :: Name
literalsInt8ToBigint             = qname literals "int8ToBigint"
literalsReadBigint               :: Name
literalsReadBigint               = qname literals "readBigint"
literalsReadBoolean              :: Name
literalsReadBoolean              = qname literals "readBoolean"
literalsReadDecimal              :: Name
literalsReadDecimal              = qname literals "readDecimal"
literalsReadFloat32              :: Name
literalsReadFloat32              = qname literals "readFloat32"
literalsReadFloat64              :: Name
literalsReadFloat64              = qname literals "readFloat64"
literalsReadInt16                :: Name
literalsReadInt16                = qname literals "readInt16"
literalsReadInt32                :: Name
literalsReadInt32                = qname literals "readInt32"
literalsReadInt64                :: Name
literalsReadInt64                = qname literals "readInt64"
literalsReadInt8                 :: Name
literalsReadInt8                 = qname literals "readInt8"
literalsReadString               :: Name
literalsReadString               = qname literals "readString"
literalsReadUint16               :: Name
literalsReadUint16               = qname literals "readUint16"
literalsReadUint32               :: Name
literalsReadUint32               = qname literals "readUint32"
literalsReadUint64               :: Name
literalsReadUint64               = qname literals "readUint64"
literalsReadUint8                :: Name
literalsReadUint8                = qname literals "readUint8"
literalsShowBigint               :: Name
literalsShowBigint               = qname literals "showBigint"
literalsShowBoolean              :: Name
literalsShowBoolean              = qname literals "showBoolean"
literalsShowDecimal              :: Name
literalsShowDecimal              = qname literals "showDecimal"
literalsShowFloat32              :: Name
literalsShowFloat32              = qname literals "showFloat32"
literalsShowFloat64              :: Name
literalsShowFloat64              = qname literals "showFloat64"
literalsShowInt16                :: Name
literalsShowInt16                = qname literals "showInt16"
literalsShowInt32                :: Name
literalsShowInt32                = qname literals "showInt32"
literalsShowInt64                :: Name
literalsShowInt64                = qname literals "showInt64"
literalsShowInt8                 :: Name
literalsShowInt8                 = qname literals "showInt8"
literalsShowString               :: Name
literalsShowString               = qname literals "showString"
literalsShowUint16               :: Name
literalsShowUint16               = qname literals "showUint16"
literalsShowUint32               :: Name
literalsShowUint32               = qname literals "showUint32"
literalsShowUint64               :: Name
literalsShowUint64               = qname literals "showUint64"
literalsShowUint8                :: Name
literalsShowUint8                = qname literals "showUint8"
literalsStringToBinary           :: Name
literalsStringToBinary           = qname literals "stringToBinary"
literalsUint16ToBigint           :: Name
literalsUint16ToBigint           = qname literals "uint16ToBigint"
literalsUint32ToBigint           :: Name
literalsUint32ToBigint           = qname literals "uint32ToBigint"
literalsUint64ToBigint           :: Name
literalsUint64ToBigint           = qname literals "uint64ToBigint"
literalsUint8ToBigint            :: Name
literalsUint8ToBigint            = qname literals "uint8ToBigint"

-- logic
logicAnd                         :: Name
logicAnd                         = qname logic "and"
logicIfElse                      :: Name
logicIfElse                      = qname logic "ifElse"
logicNot                         :: Name
logicNot                         = qname logic "not"
logicOr                          :: Name
logicOr                          = qname logic "or"

-- maps
mapsAlter                        :: Name
mapsAlter                        = qname maps "alter"
mapsBimap                        :: Name
mapsBimap                        = qname maps "bimap"
mapsDelete                       :: Name
mapsDelete                       = qname maps "delete"
mapsElems                        :: Name
mapsElems                        = qname maps "elems"
mapsEmpty                        :: Name
mapsEmpty                        = qname maps "empty"
mapsFilterWithKey                :: Name
mapsFilterWithKey                = qname maps "filterWithKey"
mapsFilter                       :: Name
mapsFilter                       = qname maps "filter"
mapsFindWithDefault              :: Name
mapsFindWithDefault              = qname maps "findWithDefault"
mapsFromList                     :: Name
mapsFromList                     = qname maps "fromList"
mapsInsert                       :: Name
mapsInsert                       = qname maps "insert"
mapsKeys                         :: Name
mapsKeys                         = qname maps "keys"
mapsLookup                       :: Name
mapsLookup                       = qname maps "lookup"
mapsMapKeys                      :: Name
mapsMapKeys                      = qname maps "mapKeys"
mapsMap                          :: Name
mapsMap                          = qname maps "map"
mapsMember                       :: Name
mapsMember                       = qname maps "member"
mapsNull                         :: Name
mapsNull                         = qname maps "null"
mapsSingleton                    :: Name
mapsSingleton                    = qname maps "singleton"
mapsSize                         :: Name
mapsSize                         = qname maps "size"
mapsToList                       :: Name
mapsToList                       = qname maps "toList"
mapsUnion                        :: Name
mapsUnion                        = qname maps "union"

-- math
mathAbs                          :: Name
mathAbs                          = qname math "abs"
mathAcosh                        :: Name
mathAcosh                        = qname math "acosh"
mathAcos                         :: Name
mathAcos                         = qname math "acos"
mathAddFloat64                   :: Name
mathAddFloat64                   = qname math "addFloat64"
mathAdd                          :: Name
mathAdd                          = qname math "add"
mathAsinh                        :: Name
mathAsinh                        = qname math "asinh"
mathAsin                         :: Name
mathAsin                         = qname math "asin"
mathAtan2                        :: Name
mathAtan2                        = qname math "atan2"
mathAtanh                        :: Name
mathAtanh                        = qname math "atanh"
mathAtan                         :: Name
mathAtan                         = qname math "atan"
mathCeiling                      :: Name
mathCeiling                      = qname math "ceiling"
mathCosh                         :: Name
mathCosh                         = qname math "cosh"
mathCos                          :: Name
mathCos                          = qname math "cos"
mathEven                         :: Name
mathEven                         = qname math "even"
mathExp                          :: Name
mathExp                          = qname math "exp"
mathE                            :: Name
mathE                            = qname math "e"
mathFloor                        :: Name
mathFloor                        = qname math "floor"
mathLogBase                      :: Name
mathLogBase                      = qname math "logBase"
mathLog                          :: Name
mathLog                          = qname math "log"
mathMax                          :: Name
mathMax                          = qname math "max"
mathMaybeDiv                     :: Name
mathMaybeDiv                     = qname math "maybeDiv"
mathMaybeMod                     :: Name
mathMaybeMod                     = qname math "maybeMod"
mathMaybePred                    :: Name
mathMaybePred                    = qname math "maybePred"
mathMaybeRem                     :: Name
mathMaybeRem                     = qname math "maybeRem"
mathMaybeSucc                    :: Name
mathMaybeSucc                    = qname math "maybeSucc"
mathMin                          :: Name
mathMin                          = qname math "min"
mathMulFloat64                   :: Name
mathMulFloat64                   = qname math "mulFloat64"
mathMul                          :: Name
mathMul                          = qname math "mul"
mathNegateFloat64                :: Name
mathNegateFloat64                = qname math "negateFloat64"
mathNegate                       :: Name
mathNegate                       = qname math "negate"
mathOdd                          :: Name
mathOdd                          = qname math "odd"
mathPi                           :: Name
mathPi                           = qname math "pi"
mathPow                          :: Name
mathPow                          = qname math "pow"
mathRange                        :: Name
mathRange                        = qname math "range"
mathRoundFloat32                 :: Name
mathRoundFloat32                 = qname math "roundFloat32"
mathRoundFloat64                 :: Name
mathRoundFloat64                 = qname math "roundFloat64"
mathRound                        :: Name
mathRound                        = qname math "round"
mathSignum                       :: Name
mathSignum                       = qname math "signum"
mathSinh                         :: Name
mathSinh                         = qname math "sinh"
mathSin                          :: Name
mathSin                          = qname math "sin"
mathSqrt                         :: Name
mathSqrt                         = qname math "sqrt"
mathSubFloat64                   :: Name
mathSubFloat64                   = qname math "subFloat64"
mathSub                          :: Name
mathSub                          = qname math "sub"
mathTanh                         :: Name
mathTanh                         = qname math "tanh"
mathTan                          :: Name
mathTan                          = qname math "tan"
mathTruncate                     :: Name
mathTruncate                     = qname math "truncate"

-- maybes
maybesApply                      :: Name
maybesApply                      = qname maybes "apply"
maybesBind                       :: Name
maybesBind                       = qname maybes "bind"
maybesCases                      :: Name
maybesCases                      = qname maybes "cases"
maybesCat                        :: Name
maybesCat                        = qname maybes "cat"
maybesCompose                    :: Name
maybesCompose                    = qname maybes "compose"
maybesFromMaybe                  :: Name
maybesFromMaybe                  = qname maybes "fromMaybe"
maybesIsJust                     :: Name
maybesIsJust                     = qname maybes "isJust"
maybesIsNothing                  :: Name
maybesIsNothing                  = qname maybes "isNothing"
maybesMapMaybe                   :: Name
maybesMapMaybe                   = qname maybes "mapMaybe"
maybesMap                        :: Name
maybesMap                        = qname maybes "map"
maybesMaybe                      :: Name
maybesMaybe                      = qname maybes "maybe"
maybesPure                       :: Name
maybesPure                       = qname maybes "pure"
maybesToList                     :: Name
maybesToList                     = qname maybes "toList"

-- pairs
pairsBimap                       :: Name
pairsBimap                       = qname pairs "bimap"
pairsFirst                       :: Name
pairsFirst                       = qname pairs "first"
pairsSecond                      :: Name
pairsSecond                      = qname pairs "second"

-- regex
regexFindAll                     :: Name
regexFindAll                     = qname regex "findAll"
regexFind                        :: Name
regexFind                        = qname regex "find"
regexMatches                     :: Name
regexMatches                     = qname regex "matches"
regexReplaceAll                  :: Name
regexReplaceAll                  = qname regex "replaceAll"
regexReplace                     :: Name
regexReplace                     = qname regex "replace"
regexSplit                       :: Name
regexSplit                       = qname regex "split"

-- sets
setsDelete                       :: Name
setsDelete                       = qname sets "delete"
setsDifference                   :: Name
setsDifference                   = qname sets "difference"
setsEmpty                        :: Name
setsEmpty                        = qname sets "empty"
setsFromList                     :: Name
setsFromList                     = qname sets "fromList"
setsInsert                       :: Name
setsInsert                       = qname sets "insert"
setsIntersection                 :: Name
setsIntersection                 = qname sets "intersection"
setsMap                          :: Name
setsMap                          = qname sets "map"
setsMember                       :: Name
setsMember                       = qname sets "member"
setsNull                         :: Name
setsNull                         = qname sets "null"
setsSingleton                    :: Name
setsSingleton                    = qname sets "singleton"
setsSize                         :: Name
setsSize                         = qname sets "size"
setsToList                       :: Name
setsToList                       = qname sets "toList"
setsUnions                       :: Name
setsUnions                       = qname sets "unions"
setsUnion                        :: Name
setsUnion                        = qname sets "union"

-- strings
stringsCat2                      :: Name
stringsCat2                      = qname strings "cat2"
stringsCat                       :: Name
stringsCat                       = qname strings "cat"
stringsFromList                  :: Name
stringsFromList                  = qname strings "fromList"
stringsIntercalate               :: Name
stringsIntercalate               = qname strings "intercalate"
stringsLength                    :: Name
stringsLength                    = qname strings "length"
stringsLines                     :: Name
stringsLines                     = qname strings "lines"
stringsMaybeCharAt               :: Name
stringsMaybeCharAt               = qname strings "maybeCharAt"
stringsNull                      :: Name
stringsNull                      = qname strings "null"
stringsSplitOn                   :: Name
stringsSplitOn                   = qname strings "splitOn"
stringsToList                    :: Name
stringsToList                    = qname strings "toList"
stringsToLower                   :: Name
stringsToLower                   = qname strings "toLower"
stringsToUpper                   :: Name
stringsToUpper                   = qname strings "toUpper"
stringsUnlines                   :: Name
stringsUnlines                   = qname strings "unlines"

-- typeclass
typeclassEq                      :: Name
typeclassEq                      = qname typeclass "Eq"
typeclassOrd                     :: Name
typeclassOrd                     = qname typeclass "Ord"
