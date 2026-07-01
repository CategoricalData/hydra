-- Note: this is an automatically generated file. Do not edit.

-- | Primitives in the hydra.lib.system module.

module Hydra.Lib.System where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

execute :: Packaging.PrimitiveDefinition
execute =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.system.execute"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Run a program to completion and capture its result."),
        Packaging.entityMetadataComments = [
          "execute(command) describes an effectful computation which runs the program described by command to completion and returns its result. This is the POSIX \"execute a command\" operation at the posix_spawn / system() altitude (https://pubs.opengroup.org/onlinepubs/9799919799/functions/posix_spawn.html, https://pubs.opengroup.org/onlinepubs/9799919799/functions/system.html): the child is spawned, Hydra waits for it (POSIX waitpid), and standard output and standard error are captured as raw bytes. Output is byte-oriented, with no character decoding or newline translation; decode it to text via hydra.lib.text.decodeUtf8. A child that runs and exits with a non-zero status (POSIX WEXITSTATUS) is returned as right(result) with that StatusCode, following the XCU section 2.8.2 exit-status convention. Only a failure to launch -- for example POSIX ENOENT, EACCES, or a bad working directory -- is returned as left(error). Unlike the shell system(), no intermediate shell is invoked; command.program is executed directly (as the execvp family does), so shell syntax in arguments is not interpreted."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.system.Command")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.system.SystemError")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.system.ProcessResult"))})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}

exit :: Packaging.PrimitiveDefinition
exit =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.system.exit"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Terminate the current process with a status code."),
        Packaging.entityMetadataComments = [
          "exit(code) describes an effectful computation which terminates the current process immediately with the given status, exactly as the POSIX exit() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/exit.html). The status is reported to the parent through wait(); by the XCU section 2.8.2 convention, 0 denotes success and non-zero denotes failure. This computation does not return, so subsequent effects in a sequence are not performed."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.system.StatusCode")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect Core.TypeUnit)}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}

getEnvironment :: Packaging.PrimitiveDefinition
getEnvironment =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.system.getEnvironment"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Get the full set of environment variables."),
        Packaging.entityMetadataComments = [
          "getEnvironment describes an effectful computation which returns the entire environment of the current process -- the POSIX environment list environ (XBD section 8, Environment Variables, https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html) -- as a map from variable name to value. POSIX represents each entry as a \"name=value\" string; this primitive splits each at the first '=' into an EnvironmentVariable key and a string value. This computation does not fail; an empty environment yields an empty map."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.system.EnvironmentVariable")),
            Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}

getEnvironmentVariable :: Packaging.PrimitiveDefinition
getEnvironmentVariable =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.system.getEnvironmentVariable"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Look up a single environment variable by name."),
        Packaging.entityMetadataComments = [
          "getEnvironmentVariable(name) describes an effectful computation which returns the value of the environment variable name, or none if it is not present -- the POSIX getenv() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/getenv.html). A variable that is present but set to the empty string is returned as given(\"\"), distinct from the none that POSIX getenv signals with a null pointer. This computation does not fail."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.system.EnvironmentVariable")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString)))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}

getTime :: Packaging.PrimitiveDefinition
getTime =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.system.getTime"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Get the current wall-clock time."),
        Packaging.entityMetadataComments = [
          "getTime describes an effectful computation which returns the current wall-clock time as a Timespec (seconds and nanoseconds since the Unix Epoch) -- the POSIX clock_gettime() function with the CLOCK_REALTIME clock (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/clock_gettime.html). The actual resolution is implementation- and platform-defined and may be coarser than nanoseconds. As with CLOCK_REALTIME, the clock is not monotonic: it can jump or move backwards when the host's wall clock is adjusted. This computation does not fail."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeVariable (Core.Name "hydra.time.Timespec")))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}

getWorkingDirectory :: Packaging.PrimitiveDefinition
getWorkingDirectory =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.system.getWorkingDirectory"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Get the current working directory."),
        Packaging.entityMetadataComments = [
          "getWorkingDirectory describes an effectful computation which returns the absolute pathname of the current working directory -- the POSIX getcwd() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/getcwd.html) -- as a FilePath. A recoverable failure (for example POSIX EACCES, or the working directory having been removed) is returned as left(error); success is returned as right(path)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.system.SystemError")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.file.FilePath"))})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
