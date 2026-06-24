package hydra.overlay.scala.lib

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}

/**
 * Scala implementations of hydra.lib.text primitives (#494).
 *
 * The Hydra `binary` type maps to a base64-encoded String in the Scala host
 * (see literals.binaryToBytes), so these helpers base64-decode/encode around the raw
 * bytes when bridging UTF-8 text and binary.
 */
object text:
  /**
   * Decode binary (a base64-encoded String) as UTF-8 text using a strict decoder.
   * Returns Right(text) on success, or Left(message) when the bytes are not valid UTF-8.
   * Mirrors the Java host's DecodeUtf8 (REPORT on malformed/unmappable input).
   */
  def decodeUtf8(binary: String): Either[String, String] =
    val bytes =
      try java.util.Base64.getDecoder.decode(binary)
      catch case e: IllegalArgumentException =>
        return Left(if e.getMessage == null then e.toString else e.getMessage)
    val decoder = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch
      case e: CharacterCodingException =>
        val message = e.getMessage
        Left(if message == null then e.toString else message)

  /**
   * Encode text as UTF-8 bytes, returned as a base64-encoded String (binary).
   * Total: every Hydra string is valid Unicode.
   */
  def encodeUtf8(string: String): String =
    java.util.Base64.getEncoder.encodeToString(string.getBytes(StandardCharsets.UTF_8))
