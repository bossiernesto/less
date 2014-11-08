package org.uqbar.less.obj.eval

import org.uqbar.less.SemanticModel._
import org.uqbar.less.Bytecode._

object Compile {

	def apply(sentences: Seq[Sentence]): Seq[Bytecode] = compile(Seq(), sentences)

	protected def compile(previous: Seq[Bytecode], sentences: Seq[Sentence]): Seq[Bytecode] = (previous /: sentences)(compile)

	protected def compile(previous: Seq[Bytecode], sentence: Sentence): Seq[Bytecode] = sentence match {
		case R(ID(id)) => previous :+ LOAD(id)
		case O(ID(id), body) => NEW +: DUP +: STORE(id) +: compile(previous, body) :+ POP
		case M(ID(id), args, body) =>
			def replaceRefs(s: Bytecode): Bytecode = {
				def indexOf(argName: Symbol) = {
					val r = (ID('this) +: args).indexOf(ID(argName))
					if (r < 0) throw new RuntimeException(s"Undefined argument $argName")
					r
				}
				s match {
					case LOAD(argName) => LOAD(Symbol(s"$$${indexOf(argName)}"))
					case STORE(argName) => STORE(Symbol(s"$$${indexOf(argName)}"))
					case _ => s
				}
			}
			previous :+ DUP :+ NEWM(id, compile(Nil, body) map replaceRefs)

		case A(values) => (compile(previous, values.reverse) :+ NEWA(values.size)) ++ (0 until values.size flatMap { n => Seq(PUSH(n), PUT) })
		case N(n) => previous :+ PUSH(n)

		case Assign(ID(target), value) => compile(previous, value) :+ STORE(target)
		case Eq(left, right) => compile(compile(previous, right), left) :+ EQ
		case Get(targetR, ID(slotName)) => compile(previous, targetR) :+ GET(slotName)
		case Set(targetR, ID(slotName), value) => compile(compile(previous, value), targetR) :+ SET(slotName)
		case Send(targetR, ID(messageName), arguments) => compile(compile(previous, arguments), targetR) :+ SEND(messageName, arguments.size)
		case At(targetA, indexN) => compile(compile(previous, indexN), targetA) :+ AT
		case Put(targetA, indexN, value) => compile(compile(compile(previous, value), targetA), indexN) :+ PUT
		case Add(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ ADD
		case Sub(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ SUB
		case Mul(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ MUL
		case Div(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ DIV
		case And(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ AND
		case Or(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ OR
		case Greater(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ GRTHN
		case Lesser(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ LSTHN
		case GreaterOrEq(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ GREQ
		case LesserOrEq(leftN, rightN) => compile(compile(previous, rightN), leftN) :+ LSEQ
		case Not(condition) => compile(previous, condition) :+ NOT
		case If(conditionN, bodyTrue, bodyFalse) =>
			val bodyTrueBytecode = compile(Seq(), bodyTrue)
			compile((compile(previous, conditionN) :+ IFNZ(bodyTrueBytecode.size)) ++ bodyTrueBytecode, bodyFalse)
		case While(conditionN, body) =>
			val bodyBytecode = compile(Seq(), body)
			val conditionCheckBytecode = compile(previous, conditionN) :+ IFNZ(bodyBytecode.size)
			conditionCheckBytecode ++ bodyBytecode :+ GOTO(-bodyBytecode.size - conditionCheckBytecode.size)
	}
}