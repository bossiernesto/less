package org.uqbar.less.obj

import java.util.UUID
package object eval {
	type O = Map[Symbol, Any]
	type M = Seq[Bytecode]

	type Referenceable = Any
	type Stack[T] = List[T]

	case class Memory(value: Map[Symbol, Referenceable]) {
		def apply[T](key: Symbol) = value(key).asInstanceOf[T]

		def insert(elem: Referenceable) = {
			val key = Symbol(UUID.randomUUID.toString)
			updated(key, elem) -> key
		}

		def updated(key: Symbol, elem: Referenceable) = copy(value.updated(key, elem))
	}

	case class State(locals: Map[Symbol, Referenceable], executionStacks: Stack[Stack[Referenceable]], pc: Int, bytecode: Seq[Bytecode], memory: Memory)
}