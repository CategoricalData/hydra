-- Note: this is an automatically generated file. Do not edit.

-- | A model for file-system paths and metadata. Several types here are drawn from the POSIX <sys/stat.h> specification; see https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/sys_stat.h.html

module Hydra.File where

import qualified Hydra.Core as Core
import qualified Hydra.Time as Time
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I

-- | A file extension (without the dot), e.g. "json" or "py"
newtype FileExtension =
  FileExtension {
    unFileExtension :: String}
  deriving (Eq, Ord, Read, Show)

_FileExtension = Core.Name "hydra.file.FileExtension"

-- | A host file-system path; a POSIX "pathname" (XBD section 3.254), i.e. a string used to identify a file. Named FilePath rather than Pathname so that the kernel type is self-describing and distinct from Hydra's other notions of paths
newtype FilePath =
  FilePath {
    unFilePath :: String}
  deriving (Eq, Ord, Read, Show)

_FilePath = Core.Name "hydra.file.FilePath"

-- | Metadata about a file, based on the POSIX struct stat. Only the portable, cross-platform subset of struct stat is modeled here; the following Unix-specific fields are omitted for now: st_dev (device ID), st_ino (inode number), st_mode permission bits (only the file type bits are kept, as fileType), st_nlink (hard link count), st_uid (owner user ID), st_gid (owner group ID), st_rdev (device ID for special files), st_blksize (preferred I/O block size), and st_blocks (number of blocks allocated). See <sys/stat.h> (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/sys_stat.h.html)
data FileStatus =
  FileStatus {
    -- | st_mode (file type bits) / the type of the file
    fileStatusFileType :: FileType,
    -- | st_size / the size of the file in bytes. Well-defined for regular files and symbolic links; implementation-defined for other file types
    fileStatusSize :: I.Int64,
    -- | st_mtim / the time of last data modification
    fileStatusModificationTime :: Time.Timespec,
    -- | st_atim / the time of last data access, if available. Optional, as it is unreliable or untracked on some filesystems and platforms (e.g. noatime mounts)
    fileStatusAccessTime :: (Maybe Time.Timespec),
    -- | st_ctim / the time of last file status change, if available. Optional, as this POSIX-specific notion does not map onto all platforms
    fileStatusStatusChangeTime :: (Maybe Time.Timespec)}
  deriving (Eq, Ord, Read, Show)

_FileStatus = Core.Name "hydra.file.FileStatus"

_FileStatus_fileType = Core.Name "fileType"

_FileStatus_size = Core.Name "size"

_FileStatus_modificationTime = Core.Name "modificationTime"

_FileStatus_accessTime = Core.Name "accessTime"

_FileStatus_statusChangeTime = Core.Name "statusChangeTime"

-- | An enumeration of POSIX file types, following the file type macros of <sys/stat.h>
data FileType =
  -- | S_IFBLK / block special: a block special file (e.g. a disk device)
  FileTypeBlock |
  -- | S_IFCHR / character special: a character special file (e.g. a terminal device)
  FileTypeCharacter |
  -- | S_IFDIR / directory: a directory
  FileTypeDirectory |
  -- | S_IFIFO / FIFO special: a named pipe (FIFO)
  FileTypeFifo |
  -- | S_IFREG / regular: a regular file
  FileTypeRegular |
  -- | S_IFLNK / symbolic link: a symbolic link
  FileTypeLink |
  -- | S_IFSOCK / socket: a socket
  FileTypeSocket
  deriving (Eq, Ord, Read, Show)

_FileType = Core.Name "hydra.file.FileType"

_FileType_block = Core.Name "block"

_FileType_character = Core.Name "character"

_FileType_directory = Core.Name "directory"

_FileType_fifo = Core.Name "fifo"

_FileType_regular = Core.Name "regular"

_FileType_link = Core.Name "link"

_FileType_socket = Core.Name "socket"
