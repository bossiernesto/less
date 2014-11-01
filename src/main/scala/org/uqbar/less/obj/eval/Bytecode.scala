package org.uqbar.less.obj.eval

sealed trait Bytecode
case class LOAD(argName: Symbol) extends Bytecode
case class STORE(argName: Symbol) extends Bytecode
case object EQ extends Bytecode
case class GET(slotName: Symbol) extends Bytecode
case class SET(slotName: Symbol) extends Bytecode
case class SEND(messageName: Symbol, argumentCount: Int) extends Bytecode
case class MKA(length: Int) extends Bytecode
case object LENGTH extends Bytecode
case object AT extends Bytecode
case object PUT extends Bytecode
case object ADD extends Bytecode
case object SUB extends Bytecode
case object MUL extends Bytecode
case object DIV extends Bytecode
case object AND extends Bytecode
case object OR extends Bytecode
case object NOT extends Bytecode
case object GRTHN extends Bytecode
case object LSTHN extends Bytecode
case object GREQ extends Bytecode
case object LSEQ extends Bytecode
case class PUSH(v: Int) extends Bytecode
case class IFNZ(jump: Int) extends Bytecode
case class GOTO(jump: Int) extends Bytecode