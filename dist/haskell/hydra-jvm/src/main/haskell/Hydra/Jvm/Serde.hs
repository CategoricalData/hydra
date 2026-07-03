-- Note: this is an automatically generated file. Do not edit.
-- | Common JVM serialization helpers: Java/Scala string and character escaping

module Hydra.Jvm.Serde where
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Escape a single character for use in a Java/Scala string or character literal
escapeJavaChar :: Int -> String
escapeJavaChar c =
    Logic.ifElse (c == 34) "\\\""
    (Logic.ifElse (c == 92) "\\\\"
    (Logic.ifElse (c == 10) "\\n"
    (Logic.ifElse (c == 13) "\\r"
    (Logic.ifElse (c == 9)  "\\t"
    (Logic.ifElse (c == 8)  "\\b"
    (Logic.ifElse (c == 12) "\\f"
    (Logic.ifElse (c >= 32 && c < 127) (Strings.fromList [c])
    (javaUnicodeEscape c))))))))

-- | Escape a string for use in a Java/Scala string literal
escapeJavaString :: String -> String
escapeJavaString s = Strings.cat (Lists.map escapeJavaChar (Strings.toList s))

-- | Convert an integer in [0,15] to the ASCII code of the corresponding hex digit
hexDigit :: Int -> Int
hexDigit n = Logic.ifElse (n < 10) (n + 48) (n - 10 + 65)

-- | Produce a \\uXXXX Unicode escape for the given code point
javaUnicodeEscape :: Int -> String
javaUnicodeEscape n =
    Logic.ifElse (n > 65535)
      (let n' = n - 65536
           hi = 55296 + (n' `div` 1024)
           lo = 56320 + (n' `mod` 1024)
       in "\\u" ++ padHex4 hi ++ "\\u" ++ padHex4 lo)
      ("\\u" ++ padHex4 n)

-- | Pad an integer to 4 hex digits
padHex4 :: Int -> String
padHex4 n =
    let d3 = n `div` 4096
        r3 = n `mod` 4096
        d2 = r3 `div` 256
        r2 = r3 `mod` 256
        d1 = r2 `div` 16
        d0 = r2 `mod` 16
    in Strings.fromList [hexDigit d3, hexDigit d2, hexDigit d1, hexDigit d0]
