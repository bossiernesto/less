package org.uqbar.less.obj.eval

object Eval {

	var memory: Map[Symbol, O] = Map()
	var lastId = 0

	def apply(bytecode: Bytecode*) = eval(List(Frame(Map(), Nil, 0)), bytecode)

	def eval(frameStack: List[Frame], bytecodeSeq: Seq[Bytecode]): List[Frame] = frameStack match {
		case (frame @ Frame(locals, stack, pc)) :: prevFrames =>
			val nextFrame = bytecodeSeq(pc) match {
				case ASIGN(argName) =>
					val value :: rest = stack
					Frame(locals.updated(argName, value), rest, pc)

				case EQ =>
					val left :: right :: rest = stack
					Frame(locals, (if (left == right) 1 else 0) :: rest, pc)

				case GET(slotName) =>
					val (target: Symbol) :: rest = stack
					Frame(locals, memory(target)(slotName) :: rest, pc)

				case SET(slotName: Symbol) =>
					val (target: Symbol) :: value :: rest = stack
					memory = memory.updated(target, memory(target).updated(slotName, value))
					Frame(locals, rest, pc)

				case SEND(messageName, argumentCount) =>
					val (target: Symbol) :: restWithArgs = stack
					val (arguments, rest) = restWithArgs.splitAt(argumentCount)
					val nextLocals = (target :: arguments).zipWithIndex.map{ case (a, i) => (Symbol(s"$$$i"), a) }.toMap
					val Frame(_, result :: _, pc) :: _ = eval(Frame(nextLocals, Nil, 0) :: frameStack, memory(target)(messageName).asInstanceOf[M])
					Frame(locals, result :: rest, pc)

				case MKA(length) =>
					val nextId = Symbol(s"#$lastId")
					lastId += 1
					val newArray = (0 until length).map(n => Symbol(n.toString) -> null).toMap
					memory = memory + (nextId -> newArray)
					Frame(locals, nextId :: stack, pc)

				case LENGTH =>
					val (target: Symbol) :: rest = stack
					val length = memory(target).keys.map(_.toString.tail.toInt).max + 1
					Frame(locals, length :: rest, pc)

				case AT =>
					val (target: Symbol) :: (index: Int) :: rest = stack
					val elem = memory(target)(Symbol(index.toString))
					Frame(locals, elem :: rest, pc)

				case PUT =>
					val (target: Symbol) :: value :: (index: Int) :: rest = stack
					memory = memory.updated(target, memory(target).updated(Symbol(index.toString), value))
					Frame(locals, rest, pc)

				case ADD =>
					val (left: Int) :: (right: Int) :: rest = stack
					Frame(locals, left + right :: rest, pc)

				case MUL =>
					val (left: Int) :: (right: Int) :: rest = stack
					Frame(locals, left * right :: rest, pc)

				case GRTHN =>
					val (left: Int) :: (right: Int) :: rest = stack
					Frame(locals, (if (left > right) 1 else 0) :: rest, pc)

				case PUSHN(n) => Frame(locals, n :: stack, pc)

				case PUSHR(id) => Frame(locals, id :: stack, pc)

				case IFNZ(jump) =>
					val condition :: rest = stack
					Frame(locals, rest, pc + 1 + (if (condition == 0) jump else 0))

				case GOTO(jump) => Frame(locals, stack, pc + 1 + jump)
			}

			nextFrame :: prevFrames
	}
}