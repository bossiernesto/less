package org.uqbar.less.obj.eval

object Eval {

	def apply(locals: Map[Symbol, Referenceable] = Map(), stack: Stack[Referenceable] = Nil, memory: Memory = Memory())(bytecode: Bytecode*) =
		eval(State(locals, stack, 0, bytecode, memory))

	protected def eval(state: State): State = {
		val State(locals, stack, pc, bytecode, memory) = state

		val nextState = bytecode(pc) match {
			case STORE(argName) =>
				val value :: rest = stack
				state.copy(
					locals = locals.updated(argName, value),
					stack = rest,
					pc = pc + 1
				)
			case LOAD(argName) =>
				state.copy(
					stack = locals(argName) :: stack,
					pc = pc + 1
				)
			case EQ =>
				val left :: right :: rest = stack
				state.copy(
					stack = (if (left == right) 1 else 0) :: rest,
					pc = pc + 1
				)

			case GET(slotName) =>
				val (target: Symbol) :: rest = stack
				state.copy(
					stack = memory[O](target)(slotName) :: rest,
					pc = pc + 1
				)

			case SET(slotName: Symbol) =>
				val (target: Symbol) :: value :: rest = stack
				state.copy(
					memory = memory.updated(target, memory[O](target).updated(slotName, value)),
					stack = rest,
					pc = pc + 1
				)

			case SEND(messageName, argumentCount) =>
				val (target: Symbol) :: restWithArgs = stack
				val (arguments, rest) = restWithArgs.splitAt(argumentCount)
				val nextLocals = (target :: arguments).zipWithIndex.map{ case (a, i) => (Symbol(s"$$$i"), a) }.toMap
				val nextBytecode = memory[O](target)(messageName).asInstanceOf[M]
				val State(_, result :: _, _, _, _) = eval(State(nextLocals, Nil :: stack, 0, nextBytecode, memory))

				state.copy(
					stack = result :: rest,
					pc = pc + 1
				)

			case MKA(length) =>
				val newArray = (0 until length).map(n => Symbol(n.toString) -> null).toMap
				val (newMemory, newArrayId) = memory.insert(newArray)

				state.copy(
					stack = newArrayId :: stack,
					memory = newMemory,
					pc = pc + 1
				)

			case LENGTH =>
				val (target: Symbol) :: rest = stack
				val length = memory[O](target).keys.map(_.toString.tail.toInt).max + 1
				state.copy(
					stack = length :: rest,
					pc = pc + 1
				)

			case AT =>
				val (target: Symbol) :: (index: Int) :: rest = stack
				val elem = memory[O](target)(Symbol(index.toString))
				state.copy(
					stack = elem :: rest,
					pc = pc + 1
				)

			case PUT =>
				val (target: Symbol) :: value :: (index: Int) :: rest = stack
				state.copy(
					memory = memory.updated(target, memory[O](target).updated(Symbol(index.toString), value)),
					stack = rest,
					pc = pc + 1
				)

			case ADD =>
				val (left: Int) :: (right: Int) :: rest = stack
				state.copy(
					stack = left + right :: rest,
					pc = pc + 1
				)

			case MUL =>
				val (left: Int) :: (right: Int) :: rest = stack
				state.copy(
					stack = left * right :: rest,
					pc = pc + 1
				)

			case GRTHN =>
				val (left: Int) :: (right: Int) :: rest = stack
				state.copy(
					stack = (if (left > right) 1 else 0) :: rest,
					pc = pc + 1
				)

			case PUSHN(n) =>
				state.copy(
					stack = n :: stack,
					pc = pc + 1
				)

			case PUSHR(id) =>
				state.copy(
					stack = id :: stack,
					pc = pc + 1
				)

			case IFNZ(jump) =>
				val condition :: rest = stack
				state.copy(
					stack = rest,
					pc = pc + 1 + (if (condition != 0) jump else 0)
				)

			case GOTO(jump) =>
				state.copy(
					pc = pc + 1 + jump
				)
		}

		if (nextState.pc < nextState.bytecode.size) eval(nextState) else nextState
	}
}