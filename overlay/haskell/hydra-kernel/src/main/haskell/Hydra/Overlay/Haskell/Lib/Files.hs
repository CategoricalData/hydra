-- | Haskell implementations of hydra.lib.files primitives

module Hydra.Overlay.Haskell.Lib.Files where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Hydra.Error.File as FileError
import qualified Hydra.File as File
import qualified Hydra.Time as Time
import qualified System.Directory as Dir
import qualified System.IO.Error as IOE
import qualified System.Posix.Files as Posix
import System.Posix.Types (EpochTime(..))
import Foreign.C.Types (CTime(..))
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

createSymlink :: File.FilePath -> File.FilePath -> IO (Either FileError.FileError ())
createSymlink target link =
  withFileError link $ Posix.createSymbolicLink (File.unFilePath target) (File.unFilePath link)

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

readSymlink :: File.FilePath -> IO (Either FileError.FileError File.FilePath)
readSymlink path =
  withFileError path $ do
    isLink <- Posix.isSymbolicLink <$> Posix.getSymbolicLinkStatus (File.unFilePath path)
    if isLink
      then File.FilePath <$> Posix.readSymbolicLink (File.unFilePath path)
      else E.throwIO (IOE.mkIOError IOE.illegalOperationErrorType "readSymlink" Nothing
             (Just (File.unFilePath path)))

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

status :: Bool -> File.FilePath -> IO (Either FileError.FileError File.FileStatus)
status followLinks path =
  withFileError path $ do
    let p = File.unFilePath path
        getStatus = if followLinks then Posix.getFileStatus else Posix.getSymbolicLinkStatus
    st <- getStatus p
    pure File.FileStatus {
      File.fileStatusFileType = posixFileType st,
      File.fileStatusSize = fromIntegral (Posix.fileSize st),
      File.fileStatusModificationTime = epochToTimespec (Posix.modificationTime st),
      File.fileStatusAccessTime = Just (epochToTimespec (Posix.accessTime st)),
      File.fileStatusStatusChangeTime = Just (epochToTimespec (Posix.statusChangeTime st))}

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

-- | Classify a POSIX file status's file type as a hydra.file.FileType.
posixFileType :: Posix.FileStatus -> File.FileType
posixFileType st
  | Posix.isSymbolicLink st  = File.FileTypeLink
  | Posix.isDirectory st     = File.FileTypeDirectory
  | Posix.isRegularFile st   = File.FileTypeRegular
  | Posix.isBlockDevice st   = File.FileTypeBlock
  | Posix.isCharacterDevice st = File.FileTypeCharacter
  | Posix.isNamedPipe st     = File.FileTypeFifo
  | Posix.isSocket st        = File.FileTypeSocket
  | otherwise                = File.FileTypeRegular

-- | Convert a POSIX EpochTime (whole seconds since the Unix Epoch; sub-second resolution is not
-- exposed by the portable "unix" API) to a hydra.time.Timespec.
epochToTimespec :: EpochTime -> Time.Timespec
epochToTimespec (CTime secs) =
  Time.Timespec {
    Time.timespecSeconds = fromIntegral secs,
    Time.timespecNanoseconds = 0}

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
