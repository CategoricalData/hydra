-- Note: this is an automatically generated file. Do not edit.

-- | File-system error types

module Hydra.Error.File where

import qualified Hydra.Core as Core
import qualified Hydra.File as File
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | A recoverable file-system error
data FileError =
  -- | A path already exists where one was required not to (POSIX EEXIST), e.g. creating a directory that is already present
  FileErrorAlreadyExists File.FilePath |
  -- | The path is syntactically invalid or cannot be represented by the host file system
  FileErrorInvalidPath String |
  -- | The requested path does not exist
  FileErrorNotFound File.FilePath |
  -- | Any other file-system error, represented as a host-provided message
  FileErrorOther String |
  -- | The host denied access to the requested path
  FileErrorPermissionDenied File.FilePath
  deriving (Eq, Ord, Read, Show)

_FileError = Core.Name "hydra.error.file.FileError"

_FileError_alreadyExists = Core.Name "alreadyExists"

_FileError_invalidPath = Core.Name "invalidPath"

_FileError_notFound = Core.Name "notFound"

_FileError_other = Core.Name "other"

_FileError_permissionDenied = Core.Name "permissionDenied"
