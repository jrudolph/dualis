package net.virtualvoid.dualis

/**
 * A tool for analysis of binary representations
 */

trait Type {
  def name: String
  //def length: Option[Long]
  //def valueToString(
}

/**
 * A sequence of values read sequentially from the input stream
 */
trait SequentialType extends Type

/**
 * A constant magic number expected at some position
 */
trait MagicNumber extends Type

trait InstanceArray {
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
 * A position inside a bytestream
 */
trait Position

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


object Dualis {
  def main(args: Array[String]) {
    println("Hello")
  }
}
