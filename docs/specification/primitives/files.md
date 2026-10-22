<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.files

<!-- Note: this is an automatically generated file. Do not edit. -->

Every primitive in this module is effectful: it describes a computation against the file system, and executing that computation is separate from describing it.

## hydra.core.lib.files.appendFile

Append bytes to the end of a file.

`∀.hydra.core.file.FilePath → binary → effect<either<hydra.core.error.file.FileError, unit>>`

*Since: 0.17.*

appendFile(path, contents) describes an effectful computation which attempts to append the raw bytes contents to the end of the file at path, creating the file if it does not exist. Unlike writeFile, existing contents are preserved. File I/O is byte-oriented; to append text, encode it to bytes first (e.g. via hydra.core.lib.text.encodeUtf8). A recoverable file-system failure is returned as left(error); success is returned as right(unit).

**[HYDRA-CORE-LIB-FILES-APPEND-FILE-PRESERVES-EXISTING-CONTENTS-AND-APPENDS] (requirement) **appendFile adds the given bytes to the end of a file's existing contents, preserving what was already there, rather than replacing it as writeFile does.

## hydra.core.lib.files.copy

Copy a file, or a directory tree, to a destination path.

`∀.boolean → hydra.core.file.FilePath → hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, unit>>`

*Deprecated since: 0.18.*

copy(recursive, source, destination) describes an effectful computation which attempts to copy source to destination. When recursive is false, source must be a single file. When recursive is true, source may be a directory, whose entire tree is copied. Copying is performed by the host operating system rather than by reading and rewriting contents, preserving binary data and avoiding a round trip through the language runtime. A recoverable file-system failure is returned as left(error); success is returned as right(unit).

Deprecated since 0.18. Use: copyFile.

**[HYDRA-CORE-LIB-FILES-COPY-COPIES-SINGLE-FILE] (requirement) **copy with recursive=false duplicates a single file's contents at the destination path.

**[HYDRA-CORE-LIB-FILES-COPY-COPIES-DIRECTORY-TREE-WHEN-RECURSIVE] (requirement) **copy with recursive=true duplicates an entire directory tree, including nested files, at the destination path, succeeding even when the destination directory already exists.

**[HYDRA-CORE-LIB-FILES-COPY-ERRORS-ON-NON-RECURSIVE-DIRECTORY] (requirement) **copy with recursive=false on a directory source yields an error, rather than silently copying only the directory entry or a subset of its contents.

## hydra.core.lib.files.createDirectory

Create a directory.

`∀.boolean → hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, unit>>`

*Since: 0.17.*

createDirectory(recursive, path) describes an effectful computation which attempts to create a directory at path. When recursive is false this corresponds to POSIX mkdir: it fails if path already exists or if a parent is missing. When recursive is true, any missing parent directories are created as well (as with mkdir -p) and an already-existing directory is not an error. A recoverable file-system failure is returned as left(error); success is returned as right(unit).

0.18 is planned to drop the leading recursive flag in favor of a flagless createDirectoryTree primitive, mirroring the copy/copyFile+copyTree split; not yet implemented.

**[HYDRA-CORE-LIB-FILES-CREATE-DIRECTORY-CREATES-DIRECTORY-THAT-THEN-EXISTS] (requirement) **createDirectory creates a directory at the given path such that a subsequent exists check on that path reports true.

## hydra.core.lib.files.createSymlink

Create a symbolic link.

`∀.hydra.core.file.FilePath → hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, unit>>`

*Since: 0.17.6.*

createSymlink(target, link) describes an effectful computation which attempts to create a symbolic link at link, pointing to target (POSIX symlink). target is stored verbatim; it may be a relative path, and it need not exist. There is no force flag: if link already exists (including as a dangling symbolic link), the computation fails with left(alreadyExists); a caller that wants replace-if-exists semantics should compose status(false, link), removeFile, and createSymlink. A missing parent directory yields left(notFound); a host or file system which refuses to create the link (e.g. an unprivileged process on Windows, or an unsupported file system) yields left(permissionDenied) or left(other) as appropriate. A recoverable file-system failure is returned as left(error); success is returned as right(unit).

**[HYDRA-CORE-LIB-FILES-CREATE-SYMLINK-CREATES-LINK-OBSERVABLE-AS-LINK-TYPE] (requirement) **createSymlink creates a symbolic link such that status(followLinks=false) on the link path reports fileType link; status(followLinks=true) on the link path reports the metadata of the link's target (not the link itself) when the target exists.

**[HYDRA-CORE-LIB-FILES-CREATE-SYMLINK-ALLOWS-DANGLING-TARGETS] (requirement) **createSymlink succeeds even when the target does not exist, creating a dangling link; the dangling link is itself reported as present by exists (observed via status, not treated as absent), and still reports fileType link under status(followLinks=false), but is reported as notFound when followed via status(followLinks=true).

**[HYDRA-CORE-LIB-FILES-CREATE-SYMLINK-PRESERVES-TARGET-VERBATIM] (requirement) **createSymlink stores the target path exactly as given, including a relative path, with no resolution or canonicalization; readSymlink returns that exact stored target.

**[HYDRA-CORE-LIB-FILES-CREATE-SYMLINK-ERRORS-ON-EXISTING-LINK-PATH] (requirement) **createSymlink fails (there is no force/replace flag) when the link path already exists, whether that existing entry is a regular file or another symbolic link (including a dangling one).

## hydra.core.lib.files.exists

Test whether a path exists.

`∀.hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, boolean>>`

*Since: 0.17.*

exists(path) describes an effectful computation which reports whether anything exists at path, whether a file, directory, or other type of file. It does not error when the path is absent; a missing path is reported as right(false). Use status to obtain the type of an existing path. A recoverable file-system failure is returned as left(error).

**[HYDRA-CORE-LIB-FILES-EXISTS-FALSE-FOR-MISSING-PATH] (requirement) **exists returns right(false) for a missing path, without erroring on absence.

**[HYDRA-CORE-LIB-FILES-EXISTS-TRUE-AFTER-CREATION] (requirement) **exists returns right(true) for a path immediately after a file or directory has been created there.

## hydra.core.lib.files.listDirectory

List the immediate entries of a directory.

`∀.hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, list<hydra.core.file.FilePath>>>`

*Since: 0.17.*

listDirectory(path) describes an effectful computation which returns the immediate entries of the directory at path, excluding the special entries "." and "..". The result is one level deep and is not ordered; to traverse a tree recursively, recurse using status. A recoverable file-system failure is returned as left(error).

**[HYDRA-CORE-LIB-FILES-LIST-DIRECTORY-LISTS-IMMEDIATE-ENTRIES] (requirement) **listDirectory returns exactly the immediate entries of the directory, one entry per file actually present, excluding the special "." and ".." entries.

## hydra.core.lib.files.readFile

Read the complete contents of a file as raw bytes.

`∀.hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, binary>>`

*Since: 0.17.*

readFile(path) describes an effectful computation which attempts to read the entire contents of the file at path as raw bytes, with no character decoding or newline translation. To interpret the result as text, decode it (e.g. via hydra.core.lib.text.decodeUtf8). A recoverable file-system failure is returned as left(error); success is returned as right(contents).

**[HYDRA-CORE-LIB-FILES-READ-FILE-ROUND-TRIPS-WITH-WRITE-FILE] (requirement) **readFile returns exactly the bytes most recently written to a file via writeFile (and any subsequent appendFile calls), byte-exactly, with no character decoding or translation.

## hydra.core.lib.files.readSymlink

Read the target of a symbolic link.

`∀.hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, hydra.core.file.FilePath>>`

*Since: 0.17.6.*

readSymlink(path) describes an effectful computation which attempts to read the target of the symbolic link at path (POSIX readlink). This is a single-hop, unresolved read: the target is returned exactly as stored, with no chain resolution or canonicalization; a relative target is returned relative, and a dangling target (one that does not exist) is returned as-is. To resolve a chain of links to a canonical path, compose repeated calls to readSymlink and status. A missing path yields left(notFound); a path which exists but is not a symbolic link yields left(invalidPath). A recoverable file-system failure is returned as left(error); success is returned as right(target).

**[HYDRA-CORE-LIB-FILES-READ-SYMLINK-RETURNS-STORED-TARGET-VERBATIM] (requirement) **readSymlink returns the target exactly as stored by createSymlink, with no chain resolution or canonicalization, including a relative target returned relative.

**[HYDRA-CORE-LIB-FILES-READ-SYMLINK-ERRORS-ON-NON-LINK-OR-MISSING-PATH] (requirement) **readSymlink yields left(invalidPath) when the path exists but is not a symbolic link, and left(notFound) when the path does not exist at all -- two distinct error conditions, neither of which is confused with a successful read.

## hydra.core.lib.files.removeDirectory

Remove a directory.

`∀.boolean → hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, unit>>`

*Since: 0.17.*

removeDirectory(recursive, path) describes an effectful computation which attempts to remove the directory at path. When recursive is false this corresponds to POSIX rmdir: it fails unless the directory is empty. When recursive is true, the directory and its entire contents are removed (as with rm -r). Removal is performed by the host operating system rather than by walking and deleting entries individually. A recoverable file-system failure is returned as left(error); success is returned as right(unit).

0.18 is planned to drop the leading recursive flag in favor of a flagless removeDirectoryTree primitive, mirroring the copy/copyFile+copyTree split; not yet implemented.

**[HYDRA-CORE-LIB-FILES-REMOVE-DIRECTORY-REMOVES-EMPTY-DIRECTORY-WHEN-NON-RECURSIVE] (requirement) **removeDirectory with recursive=false removes an empty directory, so that it no longer exists.

**[HYDRA-CORE-LIB-FILES-REMOVE-DIRECTORY-ERRORS-ON-NON-EMPTY-DIRECTORY-WHEN-NON-RECURSIVE] (requirement) **removeDirectory with recursive=false on a non-empty directory yields an error, rather than silently removing it or its contents.

**[HYDRA-CORE-LIB-FILES-REMOVE-DIRECTORY-REMOVES-POPULATED-DIRECTORY-WHEN-RECURSIVE] (requirement) **removeDirectory with recursive=true removes a directory and its entire contents, so that the directory no longer exists.

## hydra.core.lib.files.removeFile

Remove a file.

`∀.hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, unit>>`

*Since: 0.17.*

removeFile(path) describes an effectful computation which attempts to remove the file at path (POSIX unlink). A recoverable file-system failure is returned as left(error); success is returned as right(unit).

**[HYDRA-CORE-LIB-FILES-REMOVE-FILE-DELETES-FILE-SO-IT-NO-LONGER-EXISTS] (requirement) **removeFile deletes the file at the given path, so that a subsequent exists check reports false.

## hydra.core.lib.files.rename

Rename or move a file or directory.

`∀.hydra.core.file.FilePath → hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, unit>>`

*Since: 0.17.*

rename(source, destination) describes an effectful computation which attempts to rename the file or directory at source to destination. When source and destination are on the same file system this is atomic (POSIX rename) and cannot be reproduced by a copy followed by a delete. A recoverable file-system failure is returned as left(error); success is returned as right(unit).

**[HYDRA-CORE-LIB-FILES-RENAME-MOVES-TO-NEW-PATH-PRESERVING-CONTENTS] (requirement) **rename moves a file (or directory) from source to destination, such that the original contents are readable at the destination path afterward.

## hydra.core.lib.files.status

Retrieve metadata about a file or directory.

`∀.boolean → hydra.core.file.FilePath → effect<either<hydra.core.error.file.FileError, hydra.core.file.FileStatus>>`

*Since: 0.17.*

status(followLinks, path) describes an effectful computation which retrieves metadata about the file system entry at path, including its type, size, and modification time. When followLinks is true, this corresponds to POSIX stat: if path is a symbolic link, the metadata describes the link's target, and a dangling link (whose target does not exist) yields left(notFound). When followLinks is false, this corresponds to POSIX lstat: the metadata describes the link itself, with fileType link, a size equal to the length of the stored target path, and the link's own timestamps; a dangling link is not an error in this mode. status(false, path) is the only way to observe fileType link; status(true, path) never produces it, since a followed link's metadata is always that of its (non-link) target. For paths that are not symbolic links, both modes report identical metadata. A path which does not exist yields left(notFound); other recoverable file-system failures are also returned as left(error). Use exists for a boolean presence check that does not error on absence.

**[HYDRA-CORE-LIB-FILES-STATUS-REPORTS-CORRECT-TYPE-AND-SIZE-FOR-REGULAR-FILE-AND-DIRECTORY] (requirement) **status reports the correct fileType and size for a regular file (size equal to its byte count) and reports directory as the fileType for a directory.

**[HYDRA-CORE-LIB-FILES-STATUS-FOLLOW-LINKS-CONTROLS-LINK-RESOLUTION] (requirement) **status(followLinks=false) reports the metadata of a symbolic link itself (fileType link), while status(followLinks=true) on the same path reports the metadata of the link's target (its true fileType, not link) when the target exists -- the only way to observe fileType link is followLinks=false.

**[HYDRA-CORE-LIB-FILES-STATUS-NOT-FOUND-ON-MISSING-OR-DANGLING-TARGET] (requirement) **status yields left(notFound) for a path that does not exist, and also for a dangling symbolic link when followed via followLinks=true (even though the dangling link itself is not an error under followLinks=false).

## hydra.core.lib.files.writeFile

Write raw bytes as the complete contents of a file.

`∀.hydra.core.file.FilePath → binary → effect<either<hydra.core.error.file.FileError, unit>>`

*Since: 0.17.*

writeFile(path, contents) describes an effectful computation which attempts to replace the file at path with the raw bytes contents, with no character encoding or newline translation. To write text, encode it to bytes first (e.g. via hydra.core.lib.text.encodeUtf8). A recoverable file-system failure is returned as left(error); success is returned as right(unit).

**[HYDRA-CORE-LIB-FILES-WRITE-FILE-REPLACES-COMPLETE-FILE-CONTENTS] (requirement) **writeFile replaces the complete contents of the file at the given path with the given bytes, creating the file if it does not already exist.
