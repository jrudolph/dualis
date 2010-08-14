package net.virtualvoid.dualis

import scala.collection.mutable._

/**
 * A tool for analysis of binary representations
 */

trait Type {
  def name: String
  //def length: Option[Long]
  //def valueToString(
}

abstract class PrimitiveType(val name: String) extends Type

object Primitives {
  case object Byte extends PrimitiveType("Byte")
}

/**
 * A sequence of values read sequentially from the input stream
 */
trait Struct extends Type {
  def members: List[(String, Type)]
}

/**
 * A constant magic number expected at some position
 */
trait MagicNumber extends Type {
  def bytes: Array[Byte]
}

trait InstanceArray extends Type {
  def elementType: Type
}

/**
 * An array of constant size
 */
trait FixedArray extends InstanceArray {
  
}

/**
 * An array of `elementType` which depends on a length field read before
 */
trait DynamicArray extends InstanceArray {
  def length: Type
}

/**
 * A position inside a bytearray
 */
trait Position {
  
}

/**
 * A range inside a bytestream
 */
trait ByteRange

/*
 * An instance of a type at runtime
 */
trait Instance {
  def tpe: Type
  def range: ByteRange
}

// List[ByteRange => Instance]
trait Covering

trait ByteArray {
  def bytesAt(offset: Long, length: Long): Array[Byte]
  def size: Long
}

package impl {
  import net.virtualvoid.dualis

  abstract class MutableType(var name: String = "unnamed") extends Type
  class MutableStruct(name: String) extends MutableType(name) with dualis.Struct {
    private val _members = new LinkedHashMap[String, Type]
    def add(member: (String, Type)): Unit = _members += member
    def remove(name: String): Unit = _members remove name   
    
    def find(name: String): Option[Type] = _members get name
    def members = _members.toList
  }
  class FixedArray(var elementType: Type, var length: Long) extends MutableType with dualis.FixedArray
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
    //val root = new impl.MutableStruct("unnamed")
    //root.add("bytes" -> new impl.FixedArray(Primitives.Byte, file.size))

    while(true) {
      print("> ")
      val cmd = readLine

      dispatch(cmd)
    }
  }
  def main(args: Array[String]) {
    println("Hello")
    repl(null)
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
