package org.uqbar.less.obj

import org.uqbar.less.obj.eval.Bytecode

package object eval {

	type MemKey = Int

	trait MemContent
	case class I(value: Int) extends MemContent
	case class M(bytecode: Seq[Bytecode]) extends MemContent
	case class O(slots: Map[Symbol, MemKey]) extends MemContent {
		def apply(slotName: Symbol) = slots(slotName)
		def updated(slotName: Symbol, value: MemKey) = copy(slots.updated(slotName, value))
	}

	case class Memory(value: Map[MemKey, MemContent] = Map()) {
		protected var lastId = 0

		def apply[T <: MemContent](key: MemKey) = value(key).asInstanceOf[T]

		def insert(elem: MemContent) = {
			lastId += 1
			updated(lastId, elem) -> lastId
		}

		def updated(key: MemKey, elem: MemContent) = copy(value.updated(key, elem))
	}

	type Stack = List[MemKey]
	type Locals = Map[Symbol, MemKey]

	case class State(locals: Locals, stack: Stack, pc: Int, bytecode: Seq[Bytecode], memory: Memory)
}