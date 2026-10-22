<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.file

<!-- Note: this is an automatically generated file. Do not edit. -->

## hydra.core.file.FileExtension

*(undocumented)*

`∀.wrap(string)@{description="A file extension (without the dot), e.g. \"json\" or \"py\""}`

## hydra.core.file.FilePath

*(undocumented)*

`∀.wrap(string)@{description="A host file-system path; a POSIX \"pathname\" (XBD section 3.254), i.e. a string used to identify a file. Named FilePath rather than Pathname so that the kernel type is self-describing and distinct from Hydra's other notions of paths"}`

## hydra.core.file.FileStatus

*(undocumented)*

`∀.record{fileType:hydra.core.file.FileType@{description="st_mode (file type bits) / the type of the file"}, size:int64@{description="st_size / the size of the file in bytes. Well-defined for regular files and symbolic links; implementation-defined for other file types"}, modificationTime:hydra.core.time.Timespec@{description="st_mtim / the time of last data modification"}, accessTime:optional<hydra.core.time.Timespec>@{description="st_atim / the time of last data access, if available. Optional, as it is unreliable or untracked on some filesystems and platforms (e.g. noatime mounts)"}, statusChangeTime:optional<hydra.core.time.Timespec>@{description="st_ctim / the time of last file status change, if available. Optional, as this POSIX-specific notion does not map onto all platforms"}}@{description="Metadata about a file, based on the POSIX struct stat. Only the portable, cross-platform subset of struct stat is modeled here; the following Unix-specific fields are omitted for now: st_dev (device ID), st_ino (inode number), st_mode permission bits (only the file type bits are kept, as fileType), st_nlink (hard link count), st_uid (owner user ID), st_gid (owner group ID), st_rdev (device ID for special files), st_blksize (preferred I/O block size), and st_blocks (number of blocks allocated). See <sys/stat.h> (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/sys_stat.h.html)"}`

## hydra.core.file.FileType

*(undocumented)*

`∀.union{block:unit@{description="S_IFBLK / block special: a block special file (e.g. a disk device)"}, character:unit@{description="S_IFCHR / character special: a character special file (e.g. a terminal device)"}, directory:unit@{description="S_IFDIR / directory: a directory"}, fifo:unit@{description="S_IFIFO / FIFO special: a named pipe (FIFO)"}, regular:unit@{description="S_IFREG / regular: a regular file"}, link:unit@{description="S_IFLNK / symbolic link: a symbolic link"}, socket:unit@{description="S_IFSOCK / socket: a socket"}}@{description="An enumeration of POSIX file types, following the file type macros of <sys/stat.h>"}`

# hydra.core.error.file

<!-- Note: this is an automatically generated file. Do not edit. -->

## hydra.core.error.file.FileError

*(undocumented)*

`∀.union{alreadyExists:hydra.core.file.FilePath@{description="A path already exists where one was required not to (POSIX EEXIST), e.g. creating a directory that is already present"}, invalidPath:string@{description="The path is syntactically invalid or cannot be represented by the host file system"}, notFound:hydra.core.file.FilePath@{description="The requested path does not exist"}, other:string@{description="Any other file-system error, represented as a host-provided message"}, permissionDenied:hydra.core.file.FilePath@{description="The host denied access to the requested path"}}@{description="A recoverable file-system error"}`
