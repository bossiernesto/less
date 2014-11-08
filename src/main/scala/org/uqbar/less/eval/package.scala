package org.uqbar.less

import org.uqbar.less.Bytecode._
import Math.max

package object eval {
	type MemKey = Int

	trait MemContent
	case class MI(value: Int) extends MemContent
	case class MM(bytecode: Seq[Bytecode]) extends MemContent
	case class MO(slots: Map[Symbol, MemKey]) extends MemContent {
		def apply(slotName: Symbol) = slots(slotName)
		def updated(slotName: Symbol, value: MemKey) = copy(slots.updated(slotName, value))
	}

	case class Memory(value: Map[MemKey, MemContent] = Map()) {

		def apply[T <: MemContent](key: MemKey) = get[T](key).get
		def get[T <: MemContent](key: MemKey) = value.get(key).asInstanceOf[Option[T]]

		def insert(elem: MemContent) = {
			val nextId = value.keys.fold(0)(max) + 1
			updated(nextId, elem) -> nextId
		}

		def updated(key: MemKey, elem: MemContent) = copy(value.updated(key, elem))
	}

	type Stack = List[MemKey]
	type Locals = Map[Symbol, MemKey]

	case class State(locals: Locals, stack: Stack, pc: Int, bytecode: Seq[Bytecode], memory: Memory)

}