package com.github.zenpie.macrowave.internal.datalift

import java.io.DataOutput
import java.util

import scala.reflect.macros.whitebox
import scala.language.implicitConversions

abstract class LiftCodec[@specialized T](val c: whitebox.Context) {
  import c.universe._

  def encode(stream: DataOutput, value: T): Unit

  def decode(stream: TermName): Tree

}

object LiftCodec {

  implicit def stringCodecFactory: whitebox.Context => LiftCodec[String] =
    new LiftCodec[String](_) {
      import c.universe._

      val maxLenOfUTFString = 65535

      override def encode(stream: DataOutput, value: String): Unit = {
        val chunks = value.grouped(maxLenOfUTFString).toArray
        stream.writeInt(chunks.length)
        for (chunk <- chunks) {
          stream.writeUTF(chunk)
        }
      }

      override def decode(stream: TermName): Tree = {
        val chunkCount = TermName(c.freshName("chunkCount"))
        val sBuilder = TermName(c.freshName("builder"))
        val i = TermName(c.freshName("i"))

        q"""
          {
            val $chunkCount = $stream.readInt()
            val $sBuilder   = new _root_.java.lang.StringBuilder()
            var $i = 0

            while ($i < $chunkCount) {
              $sBuilder.append($stream.readUTF())
              $i += 1
            }
            $sBuilder.toString()
          }
        """
      }
  }

  implicit val intArrayCodecFactory: whitebox.Context => LiftCodec[Array[Int]] =
    { c =>
      import c.universe._
      primitiveArrayCodec(c)(_.writeInt(_), stream => q"$stream.readInt()")
    }

  implicit val longArrayCodecFactory: whitebox.Context => LiftCodec[Array[Long]] =
    { c =>
      import c.universe._
      primitiveArrayCodec(c)(_.writeLong(_), stream => q"$stream.readLong()")
    }

  implicit val bitsetCodecFactory: whitebox.Context => LiftCodec[util.BitSet] =
    { c =>
      val longArrayCodec = LiftCodec.longArrayCodecFactory(c)
      type ForeignTermName = longArrayCodec.c.universe.TermName

      new LiftCodec[util.BitSet](c) {
        import c.universe._

        override def encode(stream: DataOutput, value: util.BitSet): Unit = {
          longArrayCodec.encode(stream, value.toLongArray)
        }

        override def decode(stream: TermName): Tree = {
          val longs  = TermName(c.freshName("longs"))
          val BitSet = q"_root_.java.util.BitSet"

          q"""
            {
              val $longs = ${longArrayCodec.decode(stream.asInstanceOf[ForeignTermName]).asInstanceOf[Tree]}
              $BitSet.valueOf($longs)
            }
          """
        }
      }
    }

  private def primitiveArrayCodec[@specialized T <: AnyVal]
    (c0: whitebox.Context)
    (writeElement: (DataOutput, T) => Unit,
     readElement :  c0.universe.TermName => c0.universe.Tree)
    (implicit tpeTag: c0.universe.TypeTag[T]): LiftCodec[Array[T]] = {

      type ForeignTermName = c0.universe.TermName
      new LiftCodec[Array[T]](c0) {
        import c.universe._

        override def encode(stream: DataOutput, value: Array[T]): Unit = {
          val arrayLength = value.length
          stream.writeInt(arrayLength)
          var i = 0
          while (i < arrayLength) {
            writeElement(stream, value(i))
            i += 1
          }
        }

        override def decode(stream: TermName): Tree = {
          val arrayLength = TermName(c.freshName("arrayLength"))
          val value = TermName(c.freshName("value"))
          val i = TermName(c.freshName("i"))
          val tTpe = typeOf[T]

          q"""
            {
              val $arrayLength = $stream.readInt()
              val $value = new Array[$tTpe]($arrayLength)
              var $i = 0

              while ($i < $arrayLength) {
                $value($i) = ${readElement(stream.asInstanceOf[ForeignTermName]).asInstanceOf[Tree]}
                $i += 1
              }

              $value
            }
          """
        }
      }
    }

}
