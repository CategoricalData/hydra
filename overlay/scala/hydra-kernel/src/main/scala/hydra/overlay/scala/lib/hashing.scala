package hydra.overlay.scala.lib

import java.security.MessageDigest

/**
 * Scala implementations of hydra.lib.hashing primitives (#524).
 *
 * The Hydra `binary` type maps to a base64-encoded String in the Scala host
 * (see literals.binaryToBytes), so these helpers base64-decode the input bytes,
 * hash them, and (for sha256) base64-encode the resulting digest. Pure and total.
 */
object hashing:
  private def digest(binary: String): Array[Byte] =
    val bytes = java.util.Base64.getDecoder.decode(binary)
    MessageDigest.getInstance("SHA-256").digest(bytes)

  /**
   * Compute the SHA-256 digest of binary (a base64-encoded String), returned as a
   * base64-encoded String (binary).
   */
  def sha256(binary: String): String =
    java.util.Base64.getEncoder.encodeToString(digest(binary))

  /**
   * Compute the SHA-256 digest of binary (a base64-encoded String) as a 64-character
   * lowercase hexadecimal string.
   */
  def sha256Hex(binary: String): String =
    digest(binary).map(b => f"${b & 0xff}%02x").mkString
