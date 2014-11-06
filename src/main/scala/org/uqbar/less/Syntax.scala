package org.uqbar.less

abstract class Syntax(defs: Map[Symbol, String]) {
	def apply(key: Symbol) = defs(key)
}
object Syntax extends Syntax(Map(
	'identifier -> """[a-z_A-Z][a-z_A-Z0-9]*""",
	'number -> """-?[0-9]+""",
	'sentenceSep -> """,""",
	'assignOp -> """=""",
	'lineSep -> """;""",
	'arrayOpen -> """#\[""",
	'arrayClose -> """\]""",
	'codeOpen -> """\{""",
	'codeClose -> """\}""",
	'parensOpen -> """\(""",
	'parensClose -> """\)""",
	'argOpen -> """\(""",
	'argClose -> """\)""",
	'atOpen -> """\[""",
	'atClose -> """\]""",
	'andOp -> """&&""",
	'orOp -> """\|\|""",
	'eqOp -> """==""",
	'neqOp -> """!=""",
	'geOp -> """>=""",
	'gtOp -> """>""",
	'ltOp -> """<""",
	'leOp -> """<=""",
	'addOp -> """\+""",
	'subOp -> """-""",
	'mulOp -> """\*""",
	'divOp -> """/""",
	'notOp -> """!""",
	'access -> """\.""",
	'ifKeyword -> """if""",
	'whileKeyword -> """while""",
	'elseKeyword -> """else""",
	'defKeyword -> """def""",
	'objectKeyword -> """object"""
))
