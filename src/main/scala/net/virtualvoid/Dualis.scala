package net.virtualvoid.dualis

import scala.collection.mutable._

import java.nio.ByteBuffer

/**
 * A tool for analysis of binary representations
 */

trait ByteArray {
  def bytesAt(offset: PositionType, length: Int): ByteBuffer
  def size: Long
}

trait Type {
  def name: String

  def parse(buffer: ByteArray, pos: PositionType): Instance
  //def length: Option[Long]
  //def valueToString(
}

/**
 * A sequence of values read sequentially from the input stream
 */
trait Struct extends Type {
  def members: List[(String, Type)]
  override def parse(array: ByteArray, pos: PositionType): CompositeInstance
}

/**
 * A constant magic number expected at some position
 */
trait MagicNumber extends Type {
  def bytes: Array[Byte]
}

trait InstanceArray extends Type {
  def elementType: Type
  override def parse(array: ByteArray, pos: PositionType): InstanceCollection
}

/**
 * An array of constant size
 */
trait FixedArray extends InstanceArray {
  def length: Long
}

/**
 * An array of `elementType` which depends on a length field read before
 */
trait DynamicArray extends InstanceArray {
  def lengthTpe: Type
}

/**
 * A position inside a bytearray
 */
trait Position {
  
}

/**
 * A range inside a bytestream
 */
trait ByteRange {
  def data: ByteBuffer
  def positionAfter: PositionType
}

/*
 * An instance of a type at runtime
 */
trait Instance {
  def tpe: Type
  def range: ByteRange
}

trait InstanceCollection extends Instance {
  def members: List[Instance]
}

trait CompositeInstance extends InstanceCollection {
  def namedMembers: List[(String, Instance)]

  override def members = namedMembers map (_._2)
}

// List[ByteRange => Instance]
trait Covering


package impl {
  import net.virtualvoid.dualis

  case class ByteRange(val array: ByteArray, val offset: PositionType, val length: Int) extends dualis.ByteRange {
    def data = array.bytesAt(offset, length)
    def positionAfter = offset + length
  }

  case class SimpleInstance(val tpe: Type, val range: dualis.ByteRange) extends Instance

  abstract class PrimitiveType(val name: String, bytes: Int) extends Type {
    override def parse(buffer: ByteArray, pos: PositionType): Instance =
      SimpleInstance(this, ByteRange(buffer, pos, bytes))
  }

  object Primitives {
    case object Byte extends PrimitiveType("Byte", 1)
  }

  case class CompositeInstance(val tpe: Type, val range: dualis.ByteRange, val namedMembers: List[(String, Instance)])
       extends dualis.CompositeInstance

  trait StructImpl extends Struct {
    override def parse(buffer: ByteArray, pos: PositionType): CompositeInstance = {
      case class State(instances: List[(String, Instance)], pos: PositionType)
      
      val State(memberInstances, afterLast) = 
        members.foldLeft(State(Nil, pos)) { (state, member) =>
          (state, member) match {
            case (State(rest, pos), (name, tpe)) =>
              val next = tpe.parse(buffer, state.pos)
            State((name, next)::state.instances, next.range.positionAfter)
          }
        }
      
      CompositeInstance(this, ByteRange(buffer, pos, afterLast - pos), memberInstances)
    }
  }

  case class InstanceCollection(val tpe: Type, val range: dualis.ByteRange, val members: List[Instance])
    extends dualis.InstanceCollection

  trait FixedArrayImpl extends dualis.FixedArray {
    override def parse(buffer: ByteArray, pos: PositionType): dualis.InstanceCollection = {
      // Factor out with StructImpl
      case class State(instances: List[Instance], pos: PositionType)
      
      val State(memberInstances, afterLast) = 
        (0L until length).foldLeft(State(Nil, pos)) { (state, member) =>
          state match {
            case State(rest, pos) =>
              val next = elementType.parse(buffer, state.pos)
              State(next::state.instances, next.range.positionAfter)
          }
        }
      
      InstanceCollection(this, ByteRange(buffer, pos, afterLast - pos), memberInstances)
    }
  }

  abstract class MutableType(var name: String = "unnamed") extends Type
  class MutableStruct(name: String) extends MutableType(name) with StructImpl {
    private val _members = new LinkedHashMap[String, Type]
    def add(member: (String, Type)): Unit = _members += member
    def remove(name: String): Unit = _members remove name   
    
    def find(name: String): Option[Type] = _members get name
    override def members = _members.toList
  }
  class MutableFixedArray(var elementType: Type, var length: Long) 
    extends MutableType with dualis.FixedArray with impl.FixedArrayImpl

  import java.io._
  class FileByteArray(f: File) extends ByteArray {
    import java.nio._
    
    val raFile = new RandomAccessFile(f,"r")
    val channel = raFile.getChannel

    override def size = f.length
    override def bytesAt(pos: Int, length: Int) = {
      val buffer = ByteBuffer.allocate(length)
      channel.read(buffer, pos)
      buffer
    }
  }
}

object Dualis {
  val Exit = "exit|\u0004".r

  import java.io._
  val reader = new BufferedReader(new InputStreamReader(System.in))

  def readLine = reader.readLine

  def dispatch(cmd: String) = cmd match {
    case Exit() | null => exit(0)
    case _ => println("Command not found: '"+cmd+"'")
  }

  def repl(file: ByteArray) {
    val root = new impl.MutableStruct("unnamed")
    root.add("bytes" -> new impl.MutableFixedArray(impl.Primitives.Byte, file.size))

    var instance = root.parse(file, 0)

    while(true) {
      print("> ")
      val cmd = readLine

      dispatch(cmd)
    }
  }
  def main(args: Array[String]) {
    println("Hello")
    repl(new impl.FileByteArray(new File(args(0))))
  }
}

/*
 Sample session:

> load abc.class
Loaded 'abc.class', 125 Bytes

root = unnamed Sequential[125] {
 bytes[125] = [ca fe ba be 00 05 12 ...]
}
> name ClassFile

root = ClassFile Sequential[125] {
 bytes[125] = [ca fe ba be 00 05 12 ...]
}
> magic Magic 0xcafebabe

root = ClassFile Sequential[125] {
 Magic[ca fe ba be]
 bytes[121] = [00 05 12 a0 ...]
}
> seq Version version

Created new type Version
root.version =  


*/
