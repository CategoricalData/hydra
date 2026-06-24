package hydra.scala.lib

import hydra.error.system.SystemError
import hydra.system.{Command, ProcessResult}

import java.io.{File, IOException}

/**
 * Scala implementations of hydra.lib.system primitives (#498).
 *
 * The effect type is transparent in Scala (effect<t> = t), so these helpers perform their work
 * eagerly. Fallible primitives return scala.util.Either[SystemError, T]: a launch failure becomes
 * Left(error); success becomes Right(value). Infallible primitives return their value directly.
 * Mirrors the classify pattern of the Haskell and Java hosts.
 *
 * Wrapped string/int types (hydra.file.FilePath, hydra.system.EnvironmentVariable, hydra.system.StatusCode)
 * are rendered by the Scala coder as plain String / Int, so they are used directly. The Hydra `binary`
 * type maps to a base64-encoded String in the Scala host, so captured stdout/stderr are base64-encoded.
 */
object system:
  /**
   * Run a program to completion, capturing stdout, stderr (base64), and the exit code. A child that
   * runs and exits non-zero is Right(result); only a failure to launch is Left(error). No shell is used.
   */
  def execute(command: Command): Either[SystemError, ProcessResult] =
    val argv = new java.util.ArrayList[String]()
    argv.add(command.program)
    command.arguments.foreach(argv.add)
    val pb = new ProcessBuilder(argv)
    command.workingDirectory.foreach(d => pb.directory(new File(d)))
    command.environment.foreach { env =>
      val pe = pb.environment()
      pe.clear()
      env.foreach { case (k, v) => pe.put(k, v) }
    }
    try
      val process = pb.start()
      val out = readAll(process.getInputStream)
      val err = readAll(process.getErrorStream)
      val code = process.waitFor()
      Right(ProcessResult(
        code,
        java.util.Base64.getEncoder.encodeToString(out),
        java.util.Base64.getEncoder.encodeToString(err)))
    catch
      case e: IOException          => Left(classify(command.program, e))
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        Left(SystemError.interrupted)

  /** Terminate the current process immediately with the given status. Does not return. */
  def exit(code: Int): Unit =
    System.exit(code)

  /** Return the entire environment of the current process as a map from variable name to value. */
  def getEnvironment: Map[String, String] =
    import scala.jdk.CollectionConverters.*
    System.getenv().asScala.toMap

  /** Return the value of the named environment variable, or None if it is not set. */
  def getEnvironmentVariable(name: String): Option[String] =
    Option(System.getenv(name))

  /** Return the current wall-clock time as a Timespec (seconds and nanoseconds since the Unix epoch). */
  def getTime: hydra.time.Timespec =
    val now = java.time.Instant.now()
    hydra.time.Timespec(now.getEpochSecond, now.getNano.toLong)

  /** Return the current working directory as a FilePath (String). */
  def getWorkingDirectory: Either[SystemError, String] =
    try Right(System.getProperty("user.dir"))
    catch case e: Exception => Left(SystemError.other(message(e)))

  // ---- Helpers (not primitives) ----

  private def readAll(in: java.io.InputStream): Array[Byte] =
    val buffer = new java.io.ByteArrayOutputStream()
    val chunk = new Array[Byte](8192)
    var n = in.read(chunk)
    while n != -1 do
      buffer.write(chunk, 0, n)
      n = in.read(chunk)
    buffer.toByteArray

  /** Classify a process-launch IOException into a SystemError. */
  private def classify(program: String, e: IOException): SystemError =
    val msg = message(e)
    val lower = msg.toLowerCase
    if lower.contains("error=2") || lower.contains("no such file") then SystemError.commandNotFound(program)
    else if lower.contains("error=13") || lower.contains("permission denied") then SystemError.permissionDenied(program)
    else if lower.contains("error=20") || lower.contains("not a directory") then SystemError.invalidWorkingDirectory(program)
    else SystemError.other(msg)

  private def message(e: Exception): String =
    val msg = e.getMessage
    if msg == null then e.getClass.getSimpleName else msg
