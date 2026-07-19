<!-- NOTE: this page will be automatically generated from the kernel type definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/File.hs and
     Types/Error/File.hs (generator not yet built). Hand-authored draft under #417.
     The types/ pages document kernel types as needed by the primitive specifications;
     coverage is driven by those needs, not completeness. -->

# File types

Kernel types used by the [hydra.lib.files](../primitives/files.md) primitives.
These types live in the `hydra.file` and `hydra.error.file` namespaces.

## FilePath

A host file-system path: a POSIX "pathname" (XBD section 3.254), i.e. a string used to identify
a file.
Named `FilePath` rather than `Pathname` so that the kernel type is self-describing and distinct
from Hydra's other notions of paths.
A wrapped `string`.

## FileError

A recoverable file-system error, returned in the `left` channel of every `hydra.lib.files`
primitive.
A union with the following variants:

| Variant | Meaning |
|---|---|
| `alreadyExists` | a path already exists where one was required not to (POSIX `EEXIST`), e.g. creating a directory that is already present |
| `invalidPath` | the path is syntactically invalid or cannot be represented by the host file system |
| `notFound` | the requested path does not exist |
| `other` | any other file-system error, represented as a host-provided message |
| `permissionDenied` | the host denied access to the requested path |

Failures that cannot be surfaced as a `FileError` — hardware failure mid-operation,
termination of the executing process — are outside the specified domain, in the same category
as memory exhaustion: the specification makes no promise about behavior when the execution
substrate itself fails.

## FileStatus

Metadata about a file, based on the POSIX `struct stat`.
Only the portable, cross-platform subset of `struct stat` is modeled; Unix-specific fields
(`st_dev`, `st_ino`, permission bits, `st_nlink`, `st_uid`, `st_gid`, `st_rdev`, `st_blksize`,
`st_blocks`) are omitted.
A record with the following fields:

| Field | Type | Meaning |
|---|---|---|
| `fileType` | `FileType` | `st_mode` file type bits: the type of the file |
| `size` | `int64` | `st_size`: the size of the file in bytes; well-defined for regular files and symbolic links, implementation-defined for other file types |
| `modificationTime` | `Timespec` | `st_mtim`: the time of last data modification |
| `accessTime` | `optional<Timespec>` | `st_atim`: the time of last data access; optional, as it is unreliable or untracked on some filesystems and platforms (e.g. `noatime` mounts) |
| `statusChangeTime` | `optional<Timespec>` | `st_ctim`: the time of last file status change; optional, as this POSIX-specific notion does not map onto all platforms |

## FileType

An enumeration of POSIX file types, following the file type macros of `<sys/stat.h>`:
`blockSpecial` (`S_IFBLK`), `characterSpecial` (`S_IFCHR`), `directory` (`S_IFDIR`),
`fifo` (`S_IFIFO`), `regular` (`S_IFREG`), `symbolicLink` (`S_IFLNK`), `socket` (`S_IFSOCK`).

## FileExtension

A file extension (without the dot), e.g. `"json"` or `"py"`.
A wrapped `string`.
