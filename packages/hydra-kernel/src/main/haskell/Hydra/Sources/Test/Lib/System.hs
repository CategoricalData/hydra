module Hydra.Sources.Test.Lib.System where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
-- Effectful test cases use HONESTLY-TYPED builders (Phantoms + Literals), NOT the reified-Term
-- builders in Hydra.Dsl.Meta.Terms: effectful cases compile directly to raw target effectful code,
-- so their terms must infer at their true types (effect<...>, string, binary). For #498.
import Hydra.Dsl.Meta.Phantoms hiding ((++))  -- (@@), primitive, lambda, var, wrap, just, nothing
import Hydra.Dsl.Meta.Literals               (string, int32, int64, uint32, boolean)
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.File as File
import qualified Hydra.System as System
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Time as Time
import qualified Hydra.Lib.Effects as DefEffects
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Equality as DefEquality
import qualified Hydra.Lib.Literals as DefLiterals
import qualified Hydra.Lib.Logic as DefLogic
import qualified Hydra.Lib.Maps as DefMaps
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Strings as DefStrings
import qualified Hydra.Lib.System as DefSystem
import qualified Hydra.Lib.Text as DefText


ns :: ModuleName
ns = ModuleName "hydra.test.lib.system"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core", ModuleName "hydra.system", ModuleName "hydra.error.system", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Effectful test cases for hydra.lib.system primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.system primitives. These do not touch the canonical temp directory; they
-- exercise process execution, the environment, the working directory, and the system clock. The cases
-- are written to be deterministic across hosts: they assert booleans or fixed strings rather than
-- host-specific values.

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Effectful test cases for hydra.lib.system primitives" $
    supergroup "hydra.lib.system primitives" [
      systemExecute,
      systemGetEnvironment,
      systemGetEnvironmentVariable,
      systemGetTime,
      systemGetWorkingDirectory]

-- A variable name guaranteed not to be present in any test environment.
absentVar :: String
absentVar = "HYDRA_DEFINITELY_NOT_SET_498"

-- Fold an effect<either<SystemError, T>> into an effect<string> via the eithers eliminator, with the
-- right branch passed through a (T -> string) function and the left branch rendered as "ERR".
foldEither :: TypedTerm a -> TypedTerm b -> TypedTerm c
foldEither showRight eff = retype $
  primitive DefEffects.map
    @@ (lambda "r" $ primitive DefEithers.either
         @@ (lambda "_e" $ string "ERR")
         @@ (retype showRight)
         @@ var "r")
    @@ retype eff
  where
    retype :: TypedTerm x -> TypedTerm y
    retype (TypedTerm t) = TypedTerm t

-- Decode a binary value to a string (assuming valid UTF-8), folding the decode either via fromRight.
decodeBytes :: TypedTerm a -> TypedTerm b
decodeBytes b = retype $
  primitive DefEithers.fromRight @@ string "<decode error>" @@ (primitive DefText.decodeUtf8 @@ retype b)
  where
    retype :: TypedTerm x -> TypedTerm y
    retype (TypedTerm t) = TypedTerm t

retypeT :: TypedTerm a -> TypedTerm b
retypeT (TypedTerm t) = TypedTerm t

-- Fold an effect<either<SystemError, T>> into an effect<string>, discriminating the LEFT branch by
-- SystemError variant (returning the variant's name) rather than collapsing it to "ERR". Used to assert
-- which error a failed launch produces.
foldErrorVariant :: TypedTerm a -> TypedTerm b -> TypedTerm c
foldErrorVariant showRight eff = retype $
  primitive DefEffects.map
    @@ (lambda "r" $ primitive DefEithers.either
         @@ (lambda "e" $ Phantoms.cases ErrorSystem._SystemError (var "e") Nothing [
               Phantoms.field ErrorSystem._SystemError_commandNotFound (lambda "_p" $ string "commandNotFound"),
               Phantoms.field ErrorSystem._SystemError_permissionDenied (lambda "_p" $ string "permissionDenied"),
               Phantoms.field ErrorSystem._SystemError_invalidWorkingDirectory (lambda "_p" $ string "invalidWorkingDirectory"),
               Phantoms.field ErrorSystem._SystemError_interrupted (lambda "_u" $ string "interrupted"),
               Phantoms.field ErrorSystem._SystemError_other (lambda "_m" $ string "other")])
         @@ (retype showRight)
         @@ var "r")
    @@ retype eff
  where
    retype :: TypedTerm x -> TypedTerm y
    retype (TypedTerm t) = TypedTerm t

-- Construct a Command with the given program and arguments, inheriting the environment/cwd.
command :: String -> [String] -> TypedTerm a
command program args = retypeT $ Phantoms.record System._Command [
  Phantoms.field System._Command_program (Phantoms.wrap File._FilePath (string program)),
  Phantoms.field System._Command_arguments (list (string <$> args)),
  Phantoms.field System._Command_workingDirectory nothing,
  Phantoms.field System._Command_environment nothing]

-- Construct a Command running /bin/echo with a single argument and inheriting the environment/cwd.
echoCommand :: String -> TypedTerm a
echoCommand arg = command "/bin/echo" [arg]

-- Construct a Command with an explicit working directory (given) and inherited environment.
commandInDir :: String -> [String] -> String -> TypedTerm a
commandInDir program args dir = retypeT $ Phantoms.record System._Command [
  Phantoms.field System._Command_program (Phantoms.wrap File._FilePath (string program)),
  Phantoms.field System._Command_arguments (list (string <$> args)),
  Phantoms.field System._Command_workingDirectory (just (Phantoms.wrap File._FilePath (string dir))),
  Phantoms.field System._Command_environment nothing]

-- Construct a Command with an explicit (replacement) environment of a single name=value binding.
commandWithEnv :: String -> [String] -> String -> String -> TypedTerm a
commandWithEnv program args name value = retypeT $ Phantoms.record System._Command [
  Phantoms.field System._Command_program (Phantoms.wrap File._FilePath (string program)),
  Phantoms.field System._Command_arguments (list (string <$> args)),
  Phantoms.field System._Command_workingDirectory nothing,
  Phantoms.field System._Command_environment (just (envMapTerm [(Phantoms.wrap System._EnvironmentVariable (string name), string value)]))]

-- Build a Hydra map term from key/value TypedTerms.
envMapTerm :: [(TypedTerm a, TypedTerm b)] -> TypedTerm c
envMapTerm kvs = retypeT $ Phantoms.map $ M.fromList
  [(retypeT k, retypeT v) | (k, v) <- kvs]

-- Render a ProcessResult's stdout (binary) as a decoded string.
resultStdout :: TypedTerm a -> TypedTerm b
resultStdout r = decodeBytes (project System._ProcessResult System._ProcessResult_stdout @@ retypeT r)

-- Render a ProcessResult's stderr (binary) as a decoded string.
resultStderr :: TypedTerm a -> TypedTerm b
resultStderr r = decodeBytes (project System._ProcessResult System._ProcessResult_stderr @@ retypeT r)

-- Unwrap a ProcessResult's exit code to an int32.
resultExitCode :: TypedTerm a -> TypedTerm b
resultExitCode r = retypeT $ Phantoms.unwrap System._StatusCode @@ (project System._ProcessResult System._ProcessResult_exitCode @@ retypeT r)

-- execute: process execution. Covers stdout/stderr capture, byte-exactness, exit codes (zero and
-- non-zero as right), the workingDirectory and environment fields, and the launch-failure error path.
systemExecute :: TypedTerm TestGroup
systemExecute = subgroup "execute" [
  -- stdout capture (the core happy path).
  effectfulCase "captures stdout"
    (foldEither (lambda "r" $ resultStdout (var "r"))
      (primitive DefSystem.execute @@ echoCommand "hydra"))
    (string "hydra\n"),
  -- the argument vector is passed through in order.
  effectfulCase "passes multiple arguments through to stdout"
    (foldEither (lambda "r" $ resultStdout (var "r"))
      (primitive DefSystem.execute @@ command "/bin/echo" ["a", "b", "c"]))
    (string "a b c\n"),
  -- output is byte-oriented: no trailing-newline added, no translation (printf emits exactly "abc").
  effectfulCase "captures stdout byte-exactly (no newline translation)"
    (foldEither (lambda "r" $ resultStdout (var "r"))
      (primitive DefSystem.execute @@ command "/usr/bin/printf" ["abc"]))
    (string "abc"),
  -- a successful program reports exit status 0.
  effectfulCase "reports exit status zero for a successful program"
    (foldEither (lambda "r" $ primitive DefLiterals.showInt32 @@ resultExitCode (var "r"))
      (primitive DefSystem.execute @@ command "/usr/bin/true" []))
    (string "0"),
  -- a program that runs and exits non-zero is right(result), NOT a SystemError; /usr/bin/false exits 1.
  effectfulCase "reports a non-zero exit status as right (not an error)"
    (foldEither (lambda "r" $ primitive DefLiterals.showBoolean
        @@ (primitive DefEquality.gt @@ resultExitCode (var "r") @@ int32 0))
      (primitive DefSystem.execute @@ command "/usr/bin/false" []))
    (string "true"),
  -- stderr is captured separately; ls of a missing path writes a diagnostic to stderr.
  effectfulCase "captures stderr separately from stdout"
    (foldEither (lambda "r" $ primitive DefLiterals.showBoolean
        @@ (primitive DefLogic.not @@ (primitive DefStrings.null @@ resultStderr (var "r"))))
      (primitive DefSystem.execute @@ command "/bin/ls" ["/hydra-nonexistent-498"]))
    (string "true"),
  -- and that same run leaves stdout empty (the diagnostic did not leak into stdout).
  effectfulCase "leaves stdout empty when a program writes only to stderr"
    (foldEither (lambda "r" $ resultStdout (var "r"))
      (primitive DefSystem.execute @@ command "/bin/ls" ["/hydra-nonexistent-498"]))
    (string ""),
  -- the workingDirectory field is honored: pwd run in the root directory prints "/". The root is used
  -- rather than e.g. /tmp because /tmp is a symlink to /private/tmp on macOS, which pwd resolves.
  effectfulCase "honors the workingDirectory field"
    (foldEither (lambda "r" $ resultStdout (var "r"))
      (primitive DefSystem.execute @@ commandInDir "/bin/pwd" [] "/"))
    (string "/\n"),
  -- the environment field replaces the child's environment: env prints exactly the one binding.
  effectfulCase "honors the environment field (replacing the child environment)"
    (foldEither (lambda "r" $ resultStdout (var "r"))
      (primitive DefSystem.execute @@ commandWithEnv "/usr/bin/env" [] "HYDRA_X" "hydra-value"))
    (string "HYDRA_X=hydra-value\n"),
  -- a program that does not exist fails to LAUNCH, returning left(commandNotFound).
  effectfulCase "yields left(commandNotFound) for a missing program"
    (foldErrorVariant (lambda "_r" $ string "<ran>")
      (primitive DefSystem.execute @@ command "/hydra/no/such/program-498" []))
    (string "commandNotFound")]

-- getEnvironment: the whole environment as a map. Asserted via a guaranteed-absent key (a present-key
-- value would not be deterministic across hosts).
systemGetEnvironment :: TypedTerm TestGroup
systemGetEnvironment = subgroup "getEnvironment" [
  effectfulCase "does not contain a guaranteed-absent variable"
    (primitive DefEffects.map
      @@ (lambda "m" $ primitive DefLiterals.showBoolean
            @@ (primitive DefMaps.member @@ Phantoms.wrap System._EnvironmentVariable (string absentVar) @@ var "m"))
      @@ (primitive DefSystem.getEnvironment))
    (string "false")]

-- getEnvironmentVariable: lookup by name. The deterministic, cross-host case is the absent variable
-- (a present variable's value varies by host and cannot be asserted portably).
systemGetEnvironmentVariable :: TypedTerm TestGroup
systemGetEnvironmentVariable = subgroup "getEnvironmentVariable" [
  effectfulCase "yields none for a guaranteed-absent variable"
    (primitive DefEffects.map
      @@ (lambda "m" $ primitive DefOptionals.fromOptional @@ string "absent" @@ var "m")
      @@ (primitive DefSystem.getEnvironmentVariable
            @@ Phantoms.wrap System._EnvironmentVariable (string absentVar)))
    (string "absent")]

-- getTime: the wall clock. Covers the seconds field (a sane positive epoch value) and the nanoseconds
-- field's documented range [0, 1000000000).
systemGetTime :: TypedTerm TestGroup
systemGetTime = subgroup "getTime" [
  effectfulCase "returns a positive number of seconds since the epoch"
    (primitive DefEffects.map
      @@ (lambda "t" $ primitive DefLiterals.showBoolean
            @@ (primitive DefEquality.gt
                  @@ (Phantoms.project Time._Timespec Time._Timespec_seconds @@ var "t")
                  @@ int64 0))
      @@ (primitive DefSystem.getTime))
    (string "true"),
  effectfulCase "returns nanoseconds within the range [0, 1000000000)"
    (primitive DefEffects.map
      @@ (lambda "t" $ primitive DefLiterals.showBoolean
            @@ (primitive DefLogic.and
                  @@ (primitive DefEquality.gte
                        @@ (Phantoms.project Time._Timespec Time._Timespec_nanoseconds @@ var "t") @@ uint32 0)
                  @@ (primitive DefEquality.lt
                        @@ (Phantoms.project Time._Timespec Time._Timespec_nanoseconds @@ var "t") @@ uint32 1000000000)))
      @@ (primitive DefSystem.getTime))
    (string "true")]

-- getWorkingDirectory: the current directory as a non-empty FilePath.
systemGetWorkingDirectory :: TypedTerm TestGroup
systemGetWorkingDirectory = subgroup "getWorkingDirectory" [
  effectfulCase "returns a non-empty path"
    (foldEither
      (lambda "p" $ primitive DefLiterals.showBoolean
        @@ (primitive DefLogic.not
              @@ (primitive DefStrings.null @@ (Phantoms.unwrap File._FilePath @@ var "p"))))
      (primitive DefSystem.getWorkingDirectory))
    (string "true")]
