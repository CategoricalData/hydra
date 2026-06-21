module Hydra.Sources.Kernel.Types.File where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Time as Time


ns :: ModuleName
ns = ModuleName "hydra.file"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, Time.ns],
            moduleMetadata = descriptionMetadata (Just
              ("A model for file-system paths and metadata. Several types here are drawn from the POSIX"
              ++ " <sys/stat.h> specification; see"
              ++ " https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/sys_stat.h.html"))}
  where
    definitions = [
      fileExtension,
      filePath,
      fileStatus,
      fileType]

fileExtension :: TypeDefinition
fileExtension = define "FileExtension" $
  doc "A file extension (without the dot), e.g. \"json\" or \"py\"" $
  T.wrap T.string

filePath :: TypeDefinition
filePath = define "FilePath" $
  doc ("A host file-system path; a POSIX \"pathname\" (XBD section 3.254), i.e. a string used to"
    ++ " identify a file. Named FilePath rather than Pathname so that the kernel type is"
    ++ " self-describing and distinct from Hydra's other notions of paths") $
  T.wrap T.string

fileStatus :: TypeDefinition
fileStatus = define "FileStatus" $
  doc ("Metadata about a file, based on the POSIX struct stat. Only the portable, cross-platform"
    ++ " subset of struct stat is modeled here; the following Unix-specific fields are omitted for"
    ++ " now: st_dev (device ID), st_ino (inode number), st_mode permission bits (only the file type"
    ++ " bits are kept, as fileType), st_nlink (hard link count), st_uid (owner user ID), st_gid"
    ++ " (owner group ID), st_rdev (device ID for special files), st_blksize (preferred I/O block"
    ++ " size), and st_blocks (number of blocks allocated). See <sys/stat.h>"
    ++ " (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/sys_stat.h.html)") $
  T.record [
    "fileType">:
      doc "st_mode (file type bits) / the type of the file"
      fileType,
    "size">:
      doc ("st_size / the size of the file in bytes. Well-defined for regular files and symbolic"
        ++ " links; implementation-defined for other file types")
      T.int64,
    "modificationTime">:
      doc "st_mtim / the time of last data modification"
      Time.timespec,
    "accessTime">:
      doc ("st_atim / the time of last data access, if available. Optional, as it is unreliable or"
        ++ " untracked on some filesystems and platforms (e.g. noatime mounts)")
      (T.optional Time.timespec),
    "statusChangeTime">:
      doc ("st_ctim / the time of last file status change, if available. Optional, as this"
        ++ " POSIX-specific notion does not map onto all platforms")
      (T.optional Time.timespec)]

fileType :: TypeDefinition
fileType = define "FileType" $
  doc "An enumeration of POSIX file types, following the file type macros of <sys/stat.h>" $
  T.union [
    "block">:
      doc "S_IFBLK / block special: a block special file (e.g. a disk device)"
      T.unit,
    "character">:
      doc "S_IFCHR / character special: a character special file (e.g. a terminal device)"
      T.unit,
    "directory">:
      doc "S_IFDIR / directory: a directory"
      T.unit,
    "fifo">:
      doc "S_IFIFO / FIFO special: a named pipe (FIFO)"
      T.unit,
    "regular">:
      doc "S_IFREG / regular: a regular file"
      T.unit,
    "link">:
      doc "S_IFLNK / symbolic link: a symbolic link"
      T.unit,
    "socket">:
      doc "S_IFSOCK / socket: a socket"
      T.unit]
