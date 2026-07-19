<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Files.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.files

Filesystem input/output.
Every primitive in this module is effectful: it describes a computation against the file system,
and executing that computation is separate from describing it.
The types used here — [FilePath](../types/files.md#filepath),
[FileError](../types/files.md#fileerror), [FileStatus](../types/files.md#filestatus) — are
documented in [types/files.md](../types/files.md).

#### appendFile — **Draft**

`FilePath → binary → effect<either<FileError, unit>>`

Usage: `appendFile path contents`

Append bytes to the end of a file.
Describes an effectful computation which attempts to append the raw bytes `contents` to the end
of the file at `path`, creating the file if it does not exist.
Unlike `writeFile`, existing contents are preserved.
File I/O is byte-oriented, with no character encoding or newline translation; to append text,
encode it to bytes first (e.g. with `hydra.lib.text.encodeUtf8`).
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.

Effectful: writes to the file system.

Since: 0.17

#### copyFile — **Draft**

`FilePath → FilePath → effect<either<FileError, unit>>`

Usage: `copyFile source destination`

Copy a single file to a destination path.
Describes an effectful computation which attempts to copy the file at `source` to
`destination`; `source` must be a single file, not a directory.
Copying is performed by the host operating system rather than by reading and rewriting contents,
preserving binary data and avoiding a round trip through the language runtime.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.
To copy a directory tree, use `copyTree`.

Effectful: reads from and writes to the file system.

Since: 0.18 (split from `hydra.lib.files.copy`)

#### copyTree — **Draft**

`FilePath → FilePath → effect<either<FileError, unit>>`

Usage: `copyTree source destination`

Copy a file or directory tree, recursively, to a destination path.
Describes an effectful computation which attempts to copy `source` — a file, or a directory
whose entire tree is copied — to `destination`.
Copying is performed by the host operating system rather than by reading and rewriting contents,
preserving binary data and avoiding a round trip through the language runtime.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.

Effectful: reads from and writes to the file system.

Since: 0.18 (split from `hydra.lib.files.copy`)

#### createDirectory — **Draft**

`FilePath → effect<either<FileError, unit>>`

Usage: `createDirectory path`

Create a single directory.
Describes an effectful computation which attempts to create a directory at `path`,
corresponding to POSIX `mkdir`: the operation fails if `path` already exists
(`left alreadyExists`) or if a parent directory is missing.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.
To create missing parents as well, use `createDirectoryTree`.

Effectful: writes to the file system.

Since: 0.17 (0.18 drops the leading `recursive` flag; see `createDirectoryTree`)

#### createDirectoryTree — **Draft**

`FilePath → effect<either<FileError, unit>>`

Usage: `createDirectoryTree path`

Create a directory, creating missing parent directories as needed.
Describes an effectful computation which attempts to create a directory at `path` along with
any missing parent directories (as with `mkdir -p`); an already-existing directory is not an
error.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.

Effectful: writes to the file system.

Since: 0.18 (split from `hydra.lib.files.createDirectory`)

#### exists — **Draft**

`FilePath → effect<either<FileError, boolean>>`

Usage: `exists path`

Test whether a path exists.
Describes an effectful computation which reports whether anything exists at `path` — a file,
a directory, or any other type of entry.
A missing path is not an error: it is reported as `right false`.
Use `status` to obtain the type of an existing path.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror).

Effectful: reads from the file system.

Since: 0.17

#### listDirectory — **Draft**

`FilePath → effect<either<FileError, list<FilePath>>>`

Usage: `listDirectory path`

List the immediate entries of a directory.
Describes an effectful computation which returns the immediate entries of the directory at
`path`, excluding the special entries `.` and `..`.
The result is one level deep, and its order is unspecified; to traverse a tree recursively,
recurse using `status`.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right entries`.

Effectful: reads from the file system.

Since: 0.17

#### readFile — **Draft**

`FilePath → effect<either<FileError, binary>>`

Usage: `readFile path`

Read the complete contents of a file as raw bytes.
Describes an effectful computation which attempts to read the entire contents of the file at
`path` as raw bytes, with no character decoding or newline translation.
To interpret the result as text, decode it (e.g. with `hydra.lib.text.decodeUtf8`).
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right contents`.

Effectful: reads from the file system.

Since: 0.17

#### removeDirectory — **Draft**

`FilePath → effect<either<FileError, unit>>`

Usage: `removeDirectory path`

Remove an empty directory.
Describes an effectful computation which attempts to remove the directory at `path`,
corresponding to POSIX `rmdir`: the operation fails unless the directory is empty.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.
To remove a directory and its contents, use `removeDirectoryTree`.

Effectful: writes to the file system.

Since: 0.17 (0.18 drops the leading `recursive` flag; see `removeDirectoryTree`)

#### removeDirectoryTree — **Draft**

`FilePath → effect<either<FileError, unit>>`

Usage: `removeDirectoryTree path`

Remove a directory and its entire contents, recursively.
Describes an effectful computation which attempts to remove the directory at `path` together
with everything beneath it (as with `rm -r`).
Removal is performed by the host operating system rather than by walking and deleting entries
individually.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.

Effectful: writes to the file system.

Since: 0.18 (split from `hydra.lib.files.removeDirectory`)

#### removeFile — **Draft**

`FilePath → effect<either<FileError, unit>>`

Usage: `removeFile path`

Remove a file.
Describes an effectful computation which attempts to remove the file at `path` (POSIX `unlink`).
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.

Effectful: writes to the file system.

Since: 0.17

#### rename — **Draft**

`FilePath → FilePath → effect<either<FileError, unit>>`

Usage: `rename source destination`

Rename or move a file or directory.
Describes an effectful computation which attempts to rename the file or directory at `source`
to `destination`.
When `source` and `destination` are on the same file system, this is atomic (POSIX `rename`)
and cannot be reproduced by a copy followed by a delete.
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.

Effectful: writes to the file system.

Since: 0.17

#### status — **Draft**

`FilePath → effect<either<FileError, FileStatus>>`

Usage: `status path`

Retrieve metadata about a file or directory.
Describes an effectful computation which retrieves metadata about the entry at `path`
(POSIX `stat`), including its type, size, and modification time, as a
[FileStatus](../types/files.md#filestatus).
Symbolic links are followed.
A path which does not exist yields `left notFound`; other recoverable file-system failures are
also returned as `left` of a [FileError](../types/files.md#fileerror).
Use `exists` for a boolean presence check that does not treat absence as an error.

Effectful: reads from the file system.

Since: 0.17

#### writeFile — **Draft**

`FilePath → binary → effect<either<FileError, unit>>`

Usage: `writeFile path contents`

Write raw bytes as the complete contents of a file.
Describes an effectful computation which attempts to replace the file at `path` with the raw
bytes `contents`, with no character encoding or newline translation.
Unlike `appendFile`, existing contents are discarded.
To write text, encode it to bytes first (e.g. with `hydra.lib.text.encodeUtf8`).
A recoverable file-system failure is returned as `left` of a
[FileError](../types/files.md#fileerror); success is returned as `right unit`.

Effectful: writes to the file system.

Since: 0.17

#### copy — **Deprecated**

`boolean → FilePath → FilePath → effect<either<FileError, unit>>`

Deprecated since: 0.18. Use: `copyFile` or `copyTree`.
Split into flagless primitives; the leading boolean selected between single-file and
recursive copying.
