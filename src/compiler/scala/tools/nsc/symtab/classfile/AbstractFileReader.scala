/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package symtab
package classfile

import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import scala.tools.nsc.io.AbstractFile

/**
 * This class reads files byte per byte. Only used by ClassFileParser
 *
 * @author Philippe Altherr
 * @version 1.0, 23/03/2004
 */
trait AbstractFileReader {

  val file: AbstractFile

  /** the buffer containing the file
    */
  val buf: Array[Byte]

  /** the current input pointer
   */
  var bp: Int = 0

  /** read a byte
   */
  @throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte = {
    val b = buf(bp)
    bp += 1
    b
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = { // used in ide
    bp += len
    buf.slice(bp - len, bp)
  }

  /** read a character
   */
  def nextChar: Char =
    (((nextByte & 0xff) << 8) + (nextByte & 0xff)).toChar

  /** read an integer
   */
  def nextInt: Int =
    ((nextByte & 0xff) << 24) + ((nextByte & 0xff) << 16) +
    ((nextByte & 0xff) <<  8) +  (nextByte & 0xff)


  /** extract a character at position bp from buf
   */
  def getChar(mybp: Int): Char =
    (((buf(mybp) & 0xff) << 8) + (buf(mybp+1) & 0xff)).toChar

  /** extract an integer at position bp from buf
   */
  def getInt(mybp: Int): Int =
    ((buf(mybp  ) & 0xff) << 24) + ((buf(mybp+1) & 0xff) << 16) +
    ((buf(mybp+2) & 0xff) << 8) + (buf(mybp+3) & 0xff)

  /** extract a long integer at position bp from buf
   */
  def getLong(mybp: Int): Long =
    (getInt(mybp).toLong << 32) + (getInt(mybp + 4) & 0xffffffffL)

  /** extract a float at position bp from buf
   */
  def getFloat(mybp: Int): Float = intBitsToFloat(getInt(mybp))

  /** extract a double at position bp from buf
   */
  def getDouble(mybp: Int): Double = longBitsToDouble(getLong(mybp))

  /** skip next 'n' bytes
   */
  def skip(n: Int) { bp += n }

}

case class MemoryBackedFileReader private (
  override val file: AbstractFile,
  override val buf: Array[Byte]
) extends AbstractFileReader

object MemoryBackedFileReader {
  import java.io._
  import java.nio.file._
  import rsc.output._
  import rsc.report._

  private lazy val cache: InMemoryOutputCache = StaticCache.getCache

  private val logFile = new File("/Users/dmcclanahan/workspace/s3/log.txt").toPath

  private def logString(msg: String): Unit = if (false) {
    Files.write(logFile, s"$msg\n".getBytes, StandardOpenOption.APPEND)
  }

  def apply(file: AbstractFile): MemoryBackedFileReader = {
    val normPath = NormalizedPathForCaching(new File(file.path).toPath)
    val buf = cache.findPath(normPath) match {
      case Some(x) =>
        logString(s"scalasig bytes were found for path $normPath (${cache})!")
        x
      case None =>
        logString(s"scalasig bytes were NOT found for path $normPath (${cache})!")
        logString(cache.keysAsStr)
        file.toByteArray
    }
    new MemoryBackedFileReader(file, buf)
  }
}
