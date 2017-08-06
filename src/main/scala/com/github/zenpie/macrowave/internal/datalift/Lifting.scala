package com.github.zenpie.macrowave.internal.datalift

import java.io.{ByteArrayOutputStream, DataOutputStream}

import scala.collection.mutable
import scala.reflect.macros.whitebox

final class Lifting[DstTree](val c: whitebox.Context) {
  import c.universe._

  def lift[T](v: T)(implicit codecFactory: whitebox.Context => LiftCodec[T]): TreeRef[DstTree, T] = {
    val codec   = codecFactory(c)
    val treeRef = nextTreeRef[T]()
    liftData += ((treeRef, copyRawData(codec, v)))
    treeRef
  }

  def build(): LiftResult[DstTree] = {
    val maxDataSize   = 1 << 13
    val partArraySize = 1 << 13
    val maxPartCount  = maxDataSize / partArraySize

    val definitions = mutable.ArrayBuffer.empty[Tree]
    val treeRefs    = mutable.Map.empty[TreeRef[DstTree, _], DstTree]

    var objectName: TermName = null
    var fieldName : TermName = null
    val fieldDefs = mutable.ListBuffer.empty[Tree]
    var bufferCursor         = 0
    var buffer: Array[Byte]  = null

    def freshObjectName(): Unit = {
      objectName = TermName(c.freshName("LiftedData"))
    }
    freshObjectName()

    def freshFieldName(): Unit = {
      fieldName  = TermName(c.freshName("data"))
    }
    freshFieldName()

    def freshBuffer(): Unit = {
      bufferCursor = 0
      buffer       = new Array[Byte](partArraySize)
    }
    freshBuffer()

    def valDefFromBuffer(): Tree = {
      val array  = buffer take bufferCursor
      val valDef = q"val $fieldName: Array[Byte] = $array"

      // force val def to be a static member
      val flags  = new scala.reflect.internal.Flags
      val flagMeth = valDef.symbol.getClass.getMethod("setFlag", classOf[Long])
      flagMeth.setAccessible(true)
      flagMeth.invoke(valDef.symbol, java.lang.Long.valueOf(flags.STATIC))

      valDef
    }

    def freshObject(): Tree = {
      q"private object $objectName { ..$fieldDefs }"
    }

    def freshContainer(flush: Boolean): Unit = {
      if (bufferCursor >= partArraySize || (flush && bufferCursor > 0)) {
        // buffer is full, create a fresh one, but first:
        //   create val-def from current buffer, save in `fieldDefs`
        fieldDefs += valDefFromBuffer()
        freshFieldName()
        freshBuffer()

        // if now more than `maxPartCount` fieldDefs exist,
        //   create a new object,
        //   put collected fieldDefs there, clear collection
        if (fieldDefs.size >= maxPartCount || flush) {
          definitions += freshObject()
          fieldDefs.clear()
          freshObjectName()
        }
      } else {
        // buffer isn't full, do nothing
      }
    }

    for ((ref, RawLiftData(data, codec)) <- liftData) {
      val dataSize   = data.length
      var dataCursor = 0
      def toWrite    = dataSize - dataCursor
      val stores     = mutable.ListBuffer.empty[Store]

      while (toWrite > 0) {
        val write = Math.min(toWrite, partArraySize - bufferCursor)
        System.arraycopy(data, dataCursor, buffer, bufferCursor, write)
        stores += Store(objectName, fieldName, bufferCursor, write)
        bufferCursor += write
        dataCursor   += write
        freshContainer(flush = false)
      }

      treeRefs += ((ref, generateUnpackExpression(stores, codec).asInstanceOf[DstTree]))
    }
    freshContainer(flush = true)

    liftData.clear()
    LiftResult(
      definitions.asInstanceOf[Seq[DstTree]],
      treeRefs.toMap.asInstanceOf[Map[TreeRef[DstTree, Any], DstTree]])
  }

  private def generateUnpackExpression(stores: mutable.ListBuffer[Store], codec: LiftCodec[Any]): Tree = {

    val dataInput = TermName(c.freshName("dataInput"))

    val ByteArrayInputStream = tq"_root_.java.io.ByteArrayInputStream"
    val DataInputStream = tq"_root_.java.io.DataInputStream"
    val InputStream = tq"_root_.java.io.InputStream"

    def fromStream(stream: TermName): Tree = {
      q"""
        {
          val $dataInput = new $DataInputStream($stream)

          ${codec.decode(dataInput.asInstanceOf[codec.c.universe.TermName]).asInstanceOf[Tree]}
        }
      """
    }

    if (stores.size == 1) {

      val rawStream = TermName(c.freshName("rawStream"))
      val Store(objectNme, fieldNme, offset, length) = stores.head

      q"""
        {
          val $rawStream = new $ByteArrayInputStream($objectNme.$fieldNme, $offset, $length)

          ${fromStream(rawStream)}
        }
      """

    } else {

      val elements = TermName(c.freshName("elements"))
      val VectorOfStreams = tq"_root_.java.util.Vector[$InputStream]"
      val vector = TermName(c.freshName("vector"))

      val vectorAddStms = stores map {
        case Store(objectNme, fieldNme, offset, length) =>
          q"$vector.add(new $ByteArrayInputStream($objectNme.$fieldNme, $offset, $length))"
      }

      val EnumerationOfStreams = tq"_root_.java.util.Enumeration[$InputStream]"
      val concatStream = TermName(c.freshName("concatStream"))
      val SequenceInputStream = tq"_root_.java.io.SequenceInputStream"

      q"""
        {
          val $vector = new $VectorOfStreams(${stores.size})
          ..$vectorAddStms

          val $elements = $vector.elements().asInstanceOf[$EnumerationOfStreams]
          val $concatStream = new $SequenceInputStream($elements)

          ${fromStream(concatStream)}
        }
      """

    }
  }

  private case class RawLiftData(data: Array[Byte], codec: LiftCodec[Any])

  private val liftData = mutable.Map.empty[TreeRef[DstTree, _], RawLiftData]
  private var refIds   = 0

  private def nextTreeRef[T](): TreeRef[DstTree, T] = {
    val ref = TreeRef[DstTree, T](refIds)
    refIds += 1
    ref
  }

  private def copyRawData[T](codec: LiftCodec[T], v: T): RawLiftData = {
    val byteStream = new ByteArrayOutputStream()
    val dataOutput = new DataOutputStream(byteStream)
    codec.encode(dataOutput, v)
    RawLiftData(
      byteStream.toByteArray,
      codec.asInstanceOf[LiftCodec[Any]])
  }

  private case class Store(objectName: TermName, fieldName: TermName, offset: Int, length: Int)

}
