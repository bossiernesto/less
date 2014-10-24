package org.uqbar.less.obj

package object eval {
	type O = Map[Symbol, Any]
	type M = Seq[Bytecode]

	type Memory = Map[Symbol, Any]
	type Referenceable = Any

	type Stack[T] = List[T]
	case class Frame(locals: Map[Symbol, Referenceable], stack: Stack[Referenceable], pc: Int)

	case class State(locals: Map[Symbol, Referenceable], frameStack: Stack[Stack[Referenceable]], pc: Int, bytecode: Seq[Bytecode])
}