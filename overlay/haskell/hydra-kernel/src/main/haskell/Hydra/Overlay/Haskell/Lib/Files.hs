-- | Haskell implementations of hydra.lib.files primitives

module Hydra.Overlay.Haskell.Lib.Files where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Hydra.Error.File as FileError
import qualified Hydra.File as File
import qualified Hydra.Time as Time
import qualified System.Directory as Dir
import qualified System.IO.Error as IOE
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Prelude hiding (appendFile, readFile, writeFile)


appendFile :: File.FilePath -> BS.ByteString -> IO (Either FileError.FileError ())
appendFile path contents =
  withFileError path $ BS.appendFile (File.unFilePath path) contents

copy :: Bool -> File.FilePath -> File.FilePath -> IO (Either FileError.FileError ())
copy recursive source destination =
  withFileError source $
    if recursive
      then copyDirectoryRecursive (File.unFilePath source) (File.unFilePath destination)
      else Dir.copyFile (File.unFilePath source) (File.unFilePath destination)

createDirectory :: Bool -> File.FilePath -> IO (Either FileError.FileError ())
createDirectory recursive path =
  withFileError path $
    if recursive
      then Dir.createDirectoryIfMissing True (File.unFilePath path)
      else Dir.createDirectory (File.unFilePath path)

exists :: File.FilePath -> IO (Either FileError.FileError Bool)
exists path =
  withFileError path $ Dir.doesPathExist (File.unFilePath path)

listDirectory :: File.FilePath -> IO (Either FileError.FileError [File.FilePath])
listDirectory path =
  withFileError path $
    fmap (fmap File.FilePath) (Dir.listDirectory (File.unFilePath path))

readFile :: File.FilePath -> IO (Either FileError.FileError BS.ByteString)
readFile path =
  withFileError path $ BS.readFile (File.unFilePath path)

removeDirectory :: Bool -> File.FilePath -> IO (Either FileError.FileError ())
removeDirectory recursive path =
  withFileError path $
    if recursive
      then Dir.removeDirectoryRecursive (File.unFilePath path)
      else Dir.removeDirectory (File.unFilePath path)

removeFile :: File.FilePath -> IO (Either FileError.FileError ())
removeFile path =
  withFileError path $ Dir.removeFile (File.unFilePath path)

rename :: File.FilePath -> File.FilePath -> IO (Either FileError.FileError ())
rename source destination =
  withFileError source $ Dir.renamePath (File.unFilePath source) (File.unFilePath destination)

status :: File.FilePath -> IO (Either FileError.FileError File.FileStatus)
status path =
  withFileError path $ do
    let p = File.unFilePath path
    isDir <- Dir.doesDirectoryExist p
    size <- Dir.getFileSize p
    mtime <- Dir.getModificationTime p
    pure File.FileStatus {
      File.fileStatusFileType = if isDir then File.FileTypeDirectory else File.FileTypeRegular,
      File.fileStatusSize = fromInteger size,
      File.fileStatusModificationTime = utcToTimespec mtime,
      File.fileStatusAccessTime = Nothing,
      File.fileStatusStatusChangeTime = Nothing}

writeFile :: File.FilePath -> BS.ByteString -> IO (Either FileError.FileError ())
writeFile path contents =
  withFileError path $ BS.writeFile (File.unFilePath path) contents

-- Helpers (not primitives)

withFileError :: File.FilePath -> IO a -> IO (Either FileError.FileError a)
withFileError path action =
  E.catch (Right <$> action) $ \e ->
    pure $ Left $ classify path (e :: IOError)

classify :: File.FilePath -> IOError -> FileError.FileError
classify path e
  | IOE.isAlreadyExistsError e = FileError.FileErrorAlreadyExists path
  | IOE.isDoesNotExistError e = FileError.FileErrorNotFound path
  | IOE.isPermissionError e = FileError.FileErrorPermissionDenied path
  | IOE.isIllegalOperation e = FileError.FileErrorInvalidPath (IOE.ioeGetErrorString e)
  | otherwise = FileError.FileErrorOther (IOE.ioeGetErrorString e)

-- | Convert a UTCTime to a hydra.time.Timespec (seconds + nanoseconds since the Unix Epoch).
utcToTimespec :: UTCTime -> Time.Timespec
utcToTimespec t =
  let picos = floor (toRational (utcTimeToPOSIXSeconds t) * 1000000000000) :: Integer
      (secs, subPicos) = picos `divMod` 1000000000000
      nanos = subPicos `div` 1000
  in Time.Timespec {
       Time.timespecSeconds = fromInteger secs,
       Time.timespecNanoseconds = fromInteger nanos}

-- | Recursively copy a directory tree from source to destination.
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive source destination = do
  isDir <- Dir.doesDirectoryExist source
  if isDir
    then do
      Dir.createDirectoryIfMissing True destination
      entries <- Dir.listDirectory source
      mapM_ (\name -> copyDirectoryRecursive (source ++ "/" ++ name) (destination ++ "/" ++ name)) entries
    else Dir.copyFile source destination
