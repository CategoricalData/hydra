-- | Haskell implementations of hydra.lib.system primitives

module Hydra.Overlay.Haskell.Lib.System where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Hydra.Error.System as SystemError
import qualified Hydra.File as File
import qualified Hydra.System as System
import qualified Hydra.Time as Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (ExitCode(..), exitWith)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.IO.Error as IOE
import qualified System.Process as P


-- | Run a program to completion and capture its result.
execute :: System.Command -> IO (Either SystemError.SystemError System.ProcessResult)
execute command =
  let program = File.unFilePath (System.commandProgram command)
      args = System.commandArguments command
      cwd = fmap File.unFilePath (System.commandWorkingDirectory command)
      env = fmap toEnvList (System.commandEnvironment command)
      proc0 = (P.proc program args) {
                P.cwd = cwd,
                P.env = env,
                P.std_out = P.CreatePipe,
                P.std_err = P.CreatePipe}
  in E.catch (runProcess proc0) (pure . Left . classify (System.commandProgram command))

-- | Terminate the current process with a status code.
exit :: System.StatusCode -> IO ()
exit code =
  let n = System.unStatusCode code
  in exitWith (if n == 0 then ExitSuccess else ExitFailure (fromIntegral n))

-- | Get the full set of environment variables.
getEnvironment :: IO (M.Map System.EnvironmentVariable String)
getEnvironment = do
  pairs <- Env.getEnvironment
  pure $ M.fromList [(System.EnvironmentVariable k, v) | (k, v) <- pairs]

-- | Look up a single environment variable by name.
getEnvironmentVariable :: System.EnvironmentVariable -> IO (Maybe String)
getEnvironmentVariable name =
  Env.lookupEnv (System.unEnvironmentVariable name)

-- | Get the current wall-clock time.
getTime :: IO Time.Timespec
getTime = do
  t <- getPOSIXTime
  let picos = floor (toRational t * 1000000000000) :: Integer
      (secs, subPicos) = picos `divMod` 1000000000000
      nanos = subPicos `div` 1000
  pure Time.Timespec {
    Time.timespecSeconds = fromInteger secs,
    Time.timespecNanoseconds = fromInteger nanos}

-- | Get the current working directory.
getWorkingDirectory :: IO (Either SystemError.SystemError File.FilePath)
getWorkingDirectory =
  E.catch (Right . File.FilePath <$> Dir.getCurrentDirectory)
    (\e -> pure $ Left $ SystemError.SystemErrorOther (IOE.ioeGetErrorString (e :: IOError)))

-- Helpers (not primitives)

-- | Convert the environment map to the association list System.Process expects.
toEnvList :: M.Map System.EnvironmentVariable String -> [(String, String)]
toEnvList m = [(System.unEnvironmentVariable k, v) | (k, v) <- M.toList m]

-- | Run a prepared process, capturing stdout/stderr as bytes and the exit code.
runProcess :: P.CreateProcess -> IO (Either SystemError.SystemError System.ProcessResult)
runProcess cp = do
  (_, mout, merr, ph) <- P.createProcess cp
  out <- Y.maybe (pure BS.empty) BS.hGetContents mout
  err <- Y.maybe (pure BS.empty) BS.hGetContents merr
  ec <- P.waitForProcess ph
  let code = case ec of
        ExitSuccess -> 0
        ExitFailure n -> fromIntegral n
  pure $ Right System.ProcessResult {
    System.processResultExitCode = System.StatusCode code,
    System.processResultStdout = out,
    System.processResultStderr = err}

-- | Classify a launch IOError into a SystemError.
classify :: File.FilePath -> IOError -> SystemError.SystemError
classify program e
  | IOE.isDoesNotExistError e = SystemError.SystemErrorCommandNotFound program
  | IOE.isPermissionError e = SystemError.SystemErrorPermissionDenied program
  | otherwise = SystemError.SystemErrorOther (IOE.ioeGetErrorString e)
