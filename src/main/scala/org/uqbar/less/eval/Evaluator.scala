package org.uqbar.less.eval

import org.uqbar.less.Bytecode._

object Eval {

	def apply(locals: Locals = Map(), stack: Stack = Nil, memory: Memory = Memory())(bytecode: Bytecode*) =
		eval(State(locals, stack, 0, bytecode, memory))

	protected def eval(state: State): State = {
		val State(locals, stack, pc, bytecode, memory) = state

		if (bytecode.isEmpty) state else {
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
					val target :: rest = stack
					state.copy(
						stack = memory[MO](target)(slotName) :: rest,
						pc = pc + 1
					)

				case SET(slotName: Symbol) =>
					val target :: value :: rest = stack
					state.copy(
						memory = memory.updated(target, memory[MO](target).updated(slotName, value)),
						stack = rest,
						pc = pc + 1
					)

				case SEND(messageName, argumentCount) =>
					val target :: restWithArgs = stack
					val (arguments, rest) = restWithArgs.splitAt(argumentCount)
					val nextLocals = (target :: arguments).zipWithIndex.map{ case (a, i) => (Symbol(s"$$$i"), a) }.toMap
					val nextBytecode = memory[MM](memory[MO](target)(messageName)).bytecode
					val State(_, innerStack, _, _, _) = eval(State(nextLocals, Nil, 0, nextBytecode, memory))

					state.copy(
						stack = if (innerStack.isEmpty) rest else innerStack.head :: rest,
						pc = pc + 1
					)

				case DUP =>
					val top :: _ = stack
					state.copy(
						stack = top :: stack,
						pc = pc + 1
					)

				case POP =>
					val _ :: rest = stack
					state.copy(
						stack = rest,
						pc = pc + 1
					)

				case NEW =>
					val (newMemory, newId) = memory.insert(MO(Map()))
					state.copy(
						stack = newId :: stack,
						memory = newMemory,
						pc = pc + 1
					)

				case NEWM(id, body) =>
					val target :: rest = stack
					val newMethod = MM(body)
					val (memoryWithMethod, newMethodId) = memory.insert(newMethod)

					state.copy(
						stack = rest,
						memory = memoryWithMethod.updated(target, memoryWithMethod[MO](target).updated(id, newMethodId)),
						pc = pc + 1
					)

				case NEWA(length) =>
					val lengthMethod = MM(Seq(LOAD('$0), LENGTH))
					val (memoryWithMethod, lengthMethodId) = memory.insert(lengthMethod)
					val newArray = MO((('length -> lengthMethodId) +: (0 until length).map(n => Symbol(n.toString) -> -1)).toMap)
					val (newMemory, newArrayId) = memoryWithMethod.insert(newArray)

					state.copy(
						stack = newArrayId :: stack,
						memory = newMemory,
						pc = pc + 1
					)

				case LENGTH =>
					val target :: rest = stack
					val length = memory[MO](target).slots.keys.map(_.toString.tail.toInt).max + 1
					state.copy(
						stack = length :: rest,
						pc = pc + 1
					)

				case AT =>
					val target :: index :: rest = stack
					val elem = memory[MO](target)(Symbol(index.toString))
					state.copy(
						stack = elem :: rest,
						pc = pc + 1
					)

				case PUT =>
					val target :: value :: index :: rest = stack
					state.copy(
						memory = memory.updated(target, memory[MO](target).updated(Symbol(index.toString), value)),
						stack = rest,
						pc = pc + 1
					)

				case ADD =>
					val left :: right :: rest = stack
					state.copy(
						stack = left + right :: rest,
						pc = pc + 1
					)

				case SUB =>
					val left :: right :: rest = stack
					state.copy(
						stack = left - right :: rest,
						pc = pc + 1
					)

				case MUL =>
					val left :: right :: rest = stack
					state.copy(
						stack = left * right :: rest,
						pc = pc + 1
					)
				case DIV =>
					val left :: right :: rest = stack
					state.copy(
						stack = left / right :: rest,
						pc = pc + 1
					)

				case NOT =>
					val condition :: rest = stack
					state.copy(
						stack = (if (condition == 0) 1 else 0) :: rest,
						pc = pc + 1
					)

				case AND =>
					val left :: right :: rest = stack
					state.copy(
						stack = (if (left != 0 && right != 0) 1 else 0) :: rest,
						pc = pc + 1
					)

				case OR =>
					val left :: right :: rest = stack
					state.copy(
						stack = (if (left != 0 || right != 0) 1 else 0) :: rest,
						pc = pc + 1
					)

				case GRTHN =>
					val left :: right :: rest = stack
					state.copy(
						stack = (if (left > right) 1 else 0) :: rest,
						pc = pc + 1
					)

				case LSTHN =>
					val left :: right :: rest = stack
					state.copy(
						stack = (if (left < right) 1 else 0) :: rest,
						pc = pc + 1
					)
				case GREQ =>
					val left :: right :: rest = stack
					state.copy(
						stack = (if (left >= right) 1 else 0) :: rest,
						pc = pc + 1
					)

				case LSEQ =>
					val left :: right :: rest = stack
					state.copy(
						stack = (if (left <= right) 1 else 0) :: rest,
						pc = pc + 1
					)

				case PUSH(v) =>
					state.copy(
						stack = v :: stack,
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
}