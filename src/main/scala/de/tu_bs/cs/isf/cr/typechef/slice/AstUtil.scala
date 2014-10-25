package de.tu_bs.cs.isf.cr.typechef.slice

import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{PrettyPrinter, AST, ASTNavigation}
import de.fosd.typechef.crewrite.UsedDefinedDeclaredVariables

object AstUtil extends ASTNavigation with UsedDefinedDeclaredVariables {
	def nodeInfo(node: AST): (Int, String) = (node.getPositionFrom.getLine, PrettyPrinter.print(node).replace('\n', ' ').replace('\r', ' '))//node.getClass.getSimpleName)
	def strip(expr: FeatureExpr): String = expr.toString().replaceAll("def[a-zE]*\\(([a-zA-Z0-9_]+)\\)", "$1")
}
