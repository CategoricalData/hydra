package hydra.overlay.scala.lib

import hydra.error.file.FileError

import java.io.{FileNotFoundException, IOException}
import java.nio.file.{AccessDeniedException, FileAlreadyExistsException, Files, InvalidPathException,
  NoSuchFileException, Paths, StandardOpenOption}

/**
 * Scala implementations of hydra.lib.files primitives (#494).
 *
 * The effect type is transparent in Scala (effect<t> = t), so these helpers perform real
 * java.nio file I/O eagerly and return scala.util.Either[FileError, T]. A recoverable
 * file-system failure becomes Left(error); success becomes Right(value). Mirrors the
 * withFileError/classify pattern of the Haskell and Java hosts.
 *
 * hydra.file.FilePath is a wrapped string, which the Scala coder renders as a plain
 * String alias, so a path argument is used directly. The Hydra `binary` type maps to a
 * base64-encoded String in the Scala host (see literals.binaryToBytes), so binary file
 * contents arrive/leave as base64 Strings and are base64-decoded/encoded around the raw
 * byte I/O. hydra.core.unit maps to Scala Unit, so mutating operations succeed with Right(()).
 */
object files:
  /** Append binary contents (base64-encoded) to the end of a file, creating it if absent. */
  def appendFile(path: String)(contents: String): Either[FileError, Unit] =
    withFileError(path) {
      Files.write(Paths.get(path), java.util.Base64.getDecoder.decode(contents),
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      ()
    }

  /** Copy source to destination; when recursive, source may be a directory whose tree is copied. */
  def copy(recursive: Boolean)(source: String)(destination: String): Either[FileError, Unit] =
    withFileError(source) {
      val sourcePath = Paths.get(source)
      val destinationPath = Paths.get(destination)
      if recursive && Files.isDirectory(sourcePath) then copyDirectoryRecursive(sourcePath, destinationPath)
      else {
        if Files.isDirectory(sourcePath) then
          throw new _root_.java.nio.file.FileSystemException(source, null, "is a directory, but recursive is false")
        Files.copy(sourcePath, destinationPath, _root_.java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }
      ()
    }

  /** Create a directory; when recursive, create missing parents (mkdir -p). */
  def createDirectory(recursive: Boolean)(path: String): Either[FileError, Unit] =
    withFileError(path) {
      val p = Paths.get(path)
      if recursive then Files.createDirectories(p) else Files.createDirectory(p)
      ()
    }

  /** Create a symbolic link at link, pointing to target (stored verbatim). No force flag: an
    * occupied link path (including a dangling symlink) fails with alreadyExists. */
  def createSymlink(target: String)(link: String): Either[FileError, Unit] =
    withFileError(link) {
      Files.createSymbolicLink(Paths.get(link), Paths.get(target))
      ()
    }

  /** Test whether a path exists (no error on absence). */
  def exists(path: String): Either[FileError, Boolean] =
    withFileError(path) { Files.exists(Paths.get(path)) }

  /** List the immediate entries of a directory as bare names (FilePath = String). */
  def listDirectory(path: String): Either[FileError, Seq[String]] =
    withFileError(path) {
      val stream = Files.list(Paths.get(path))
      try {
        val buf = _root_.scala.collection.mutable.ArrayBuffer.empty[String]
        stream.forEach { p =>
          val name = p.getFileName
          buf += (if name == null then p.toString else name.toString)
        }
        buf.toSeq
      } finally stream.close()
    }

  /** Read the entire contents of a file, returning the bytes as a base64-encoded String. */
  def readFile(path: String): Either[FileError, String] =
    withFileError(path) {
      java.util.Base64.getEncoder.encodeToString(Files.readAllBytes(Paths.get(path)))
    }

  /** Read the target of the symbolic link at path, verbatim and unresolved (relative stays
    * relative; a dangling target is returned as-is). Fails with invalidPath if path is not a
    * symbolic link. */
  def readSymlink(path: String): Either[FileError, String] =
    withFileError(path) {
      val p = Paths.get(path)
      if !Files.exists(p, _root_.java.nio.file.LinkOption.NOFOLLOW_LINKS) then
        throw new NoSuchFileException(path)
      if !Files.isSymbolicLink(p) then
        throw new InvalidPathException(path, "not a symbolic link")
      Files.readSymbolicLink(p).toString
    }

  /** Remove a directory; when recursive, remove its entire contents (rm -r). */
  def removeDirectory(recursive: Boolean)(path: String): Either[FileError, Unit] =
    withFileError(path) {
      if recursive then removeDirectoryRecursive(Paths.get(path)) else Files.delete(Paths.get(path))
      ()
    }

  /** Remove a file (POSIX unlink). */
  def removeFile(path: String): Either[FileError, Unit] =
    withFileError(path) {
      Files.delete(Paths.get(path))
      ()
    }

  /** Rename or move a file or directory. */
  def rename(source: String)(destination: String): Either[FileError, Unit] =
    withFileError(source) {
      Files.move(Paths.get(source), Paths.get(destination))
      ()
    }

  /** Retrieve metadata about the file at path. When followLinks is true (POSIX stat), a
    * symbolic link's metadata is that of its target, and a dangling link is not found. When
    * false (POSIX lstat), a symbolic link's own metadata is reported (fileType link), and a
    * dangling link is not an error. */
  def status(followLinks: Boolean)(path: String): Either[FileError, hydra.file.FileStatus] =
    withFileError(path) {
      import _root_.java.nio.file.LinkOption
      val options: Array[LinkOption] =
        if followLinks then Array.empty else Array(LinkOption.NOFOLLOW_LINKS)
      val attrs = Files.readAttributes(Paths.get(path), "unix:*", options*)
      val mode = attrs.get("mode").asInstanceOf[Integer].intValue
      val size = attrs.get("size").asInstanceOf[java.lang.Long].longValue
      val mtime = attrs.get("lastModifiedTime").asInstanceOf[_root_.java.nio.file.attribute.FileTime]
      val atime = attrs.get("lastAccessTime").asInstanceOf[_root_.java.nio.file.attribute.FileTime]
      val ctime = attrs.get("ctime").asInstanceOf[_root_.java.nio.file.attribute.FileTime]
      hydra.file.FileStatus(
        fileTypeFromMode(mode),
        size,
        timespec(mtime.toInstant),
        Some(timespec(atime.toInstant)),
        Some(timespec(ctime.toInstant)))
    }

  /** Write binary contents (base64-encoded) as the complete contents of a file. */
  def writeFile(path: String)(contents: String): Either[FileError, Unit] =
    withFileError(path) {
      Files.write(Paths.get(path), java.util.Base64.getDecoder.decode(contents))
      ()
    }

  // ---- Helpers (not primitives) ----

  // POSIX st_mode file-type bits (<sys/stat.h> S_IFMT and the individual S_IF* macros).
  private val S_IFMT   = 0170000
  private val S_IFSOCK = 0140000
  private val S_IFLNK  = 0120000
  private val S_IFREG  = 0100000
  private val S_IFBLK  = 0060000
  private val S_IFDIR  = 0040000
  private val S_IFCHR  = 0020000
  private val S_IFIFO  = 0010000

  private def fileTypeFromMode(mode: Int): hydra.file.FileType =
    (mode & S_IFMT) match {
      case S_IFDIR  => hydra.file.FileType.directory
      case S_IFLNK  => hydra.file.FileType.link
      case S_IFBLK  => hydra.file.FileType.block
      case S_IFCHR  => hydra.file.FileType.character
      case S_IFIFO  => hydra.file.FileType.fifo
      case S_IFSOCK => hydra.file.FileType.socket
      case _        => hydra.file.FileType.regular
    }

  private def timespec(instant: _root_.java.time.Instant): hydra.time.Timespec =
    hydra.time.Timespec(instant.getEpochSecond, instant.getNano.toLong)

  private def copyDirectoryRecursive(source: _root_.java.nio.file.Path, destination: _root_.java.nio.file.Path): Unit = {
    import _root_.java.nio.file.{FileVisitResult, SimpleFileVisitor}
    import _root_.java.nio.file.attribute.BasicFileAttributes
    Files.walkFileTree(source, new SimpleFileVisitor[_root_.java.nio.file.Path] {
      override def preVisitDirectory(dir: _root_.java.nio.file.Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.createDirectories(destination.resolve(source.relativize(dir)))
        FileVisitResult.CONTINUE
      }
      override def visitFile(file: _root_.java.nio.file.Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.copy(file, destination.resolve(source.relativize(file)), _root_.java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        FileVisitResult.CONTINUE
      }
    })
    ()
  }

  private def removeDirectoryRecursive(path: _root_.java.nio.file.Path): Unit = {
    import _root_.java.nio.file.{FileVisitResult, SimpleFileVisitor}
    import _root_.java.nio.file.attribute.BasicFileAttributes
    Files.walkFileTree(path, new SimpleFileVisitor[_root_.java.nio.file.Path] {
      override def visitFile(file: _root_.java.nio.file.Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: _root_.java.nio.file.Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
    ()
  }

  /** Run a file-system action, translating any IOException into the appropriate FileError. */
  private def withFileError[T](path: String)(action: => T): Either[FileError, T] =
    try Right(action)
    catch {
      case e: InvalidPathException => Left(FileError.invalidPath(message(e)))
      case e: IOException          => Left(classify(path, e))
      case e: Exception            => Left(FileError.other(message(e)))
    }

  /** Classify an IOException into a FileError, mirroring the Haskell/Java host's classify. */
  private def classify(path: String, e: IOException): FileError = e match {
    case _: FileAlreadyExistsException => FileError.alreadyExists(path)
    case _: NoSuchFileException        => FileError.notFound(path)
    case _: FileNotFoundException      => FileError.notFound(path)
    case _: AccessDeniedException      => FileError.permissionDenied(path)
    case _                             => FileError.other(message(e))
  }

  private def message(e: Exception): String = {
    val msg = e.getMessage
    if msg == null then e.getClass.getSimpleName else msg
  }
