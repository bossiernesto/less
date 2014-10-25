package org.uqbar.less.obj.eval

object Eval {

	protected def eval(state: State): State = {
		val State(locals, executionStacks @ stack :: previousStacks, pc, bytecode, memory) = state

		bytecode(pc) match {
			case ASIGN(argName) =>
				val value :: rest = stack
				state.copy(
					locals = locals.updated(argName, value),
					executionStacks = rest :: previousStacks,
					pc = pc + 1
				)
			case EQ =>
				val left :: right :: rest = stack
				state.copy(
					executionStacks = ((if (left == right) 1 else 0) :: rest) :: previousStacks,
					pc = pc + 1
				)

			case GET(slotName) =>
				val (target: Symbol) :: rest = stack
				state.copy(
					executionStacks = (memory[O](target)(slotName) :: rest) :: previousStacks,
					pc = pc + 1
				)

			case SET(slotName: Symbol) =>
				val (target: Symbol) :: value :: rest = stack
				state.copy(
					memory = memory.updated(target, memory[O](target).updated(slotName, value)),
					executionStacks = rest :: previousStacks,
					pc = pc + 1
				)

			case SEND(messageName, argumentCount) =>
				val (target: Symbol) :: restWithArgs = stack
				val (arguments, rest) = restWithArgs.splitAt(argumentCount)
				val nextLocals = (target :: arguments).zipWithIndex.map{ case (a, i) => (Symbol(s"$$$i"), a) }.toMap
				val State(_, (result :: _) :: _, _, _, _) = eval(State(nextLocals, Nil :: executionStacks, 0, memory[O](target)(messageName).asInstanceOf[M], memory))

				state.copy(
					executionStacks = (result :: rest) :: previousStacks,
					pc = pc + 1
				)

			case MKA(length) =>
				val newArray = (0 until length).map(n => Symbol(n.toString) -> null).toMap
				val (newMemory, newArrayId) = memory.insert(newArray)

				state.copy(
					executionStacks = (newArrayId :: stack) :: previousStacks,
					memory = newMemory,
					pc = pc + 1
				)

			case LENGTH =>
				val (target: Symbol) :: rest = stack
				val length = memory[O](target).keys.map(_.toString.tail.toInt).max + 1
				state.copy(
					executionStacks = (length :: rest) :: previousStacks,
					pc = pc + 1
				)

			case AT =>
				val (target: Symbol) :: (index: Int) :: rest = stack
				val elem = memory[O](target)(Symbol(index.toString))
				state.copy(
					executionStacks = (elem :: rest) :: previousStacks,
					pc = pc + 1
				)

			case PUT =>
				val (target: Symbol) :: value :: (index: Int) :: rest = stack
				state.copy(
					memory = memory.updated(target, memory[O](target).updated(Symbol(index.toString), value)),
					executionStacks = rest :: previousStacks,
					pc = pc + 1
				)

			case ADD =>
				val (left: Int) :: (right: Int) :: rest = stack
				state.copy(
					executionStacks = (left + right :: rest) :: previousStacks,
					pc = pc + 1
				)

			case MUL =>
				val (left: Int) :: (right: Int) :: rest = stack
				state.copy(
					executionStacks = (left * right :: rest) :: previousStacks,
					pc = pc + 1
				)

			case GRTHN =>
				val (left: Int) :: (right: Int) :: rest = stack
				state.copy(
					executionStacks = ((if (left > right) 1 else 0) :: rest) :: previousStacks,
					pc = pc + 1
				)

			case PUSHN(n) =>
				state.copy(
					executionStacks = (n :: stack) :: previousStacks,
					pc = pc + 1
				)

			case PUSHR(id) =>
				state.copy(
					executionStacks = (id :: stack) :: previousStacks,
					pc = pc + 1
				)

			case IFNZ(jump) =>
				val condition :: rest = stack
				state.copy(
					executionStacks = rest :: previousStacks,
					pc = pc + 1 + (if (condition == 0) jump else 0)
				)

			case GOTO(jump) =>
				state.copy(
					pc = pc + 1 + jump
				)
		}

	}
}