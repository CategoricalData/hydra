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

  /** Create a directory; when recursive, create missing parents (mkdir -p). */
  def createDirectory(recursive: Boolean)(path: String): Either[FileError, Unit] =
    withFileError(path) {
      val p = Paths.get(path)
      if recursive then Files.createDirectories(p) else Files.createDirectory(p)
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

  /** Write binary contents (base64-encoded) as the complete contents of a file. */
  def writeFile(path: String)(contents: String): Either[FileError, Unit] =
    withFileError(path) {
      Files.write(Paths.get(path), java.util.Base64.getDecoder.decode(contents))
      ()
    }

  // ---- Helpers (not primitives) ----

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
