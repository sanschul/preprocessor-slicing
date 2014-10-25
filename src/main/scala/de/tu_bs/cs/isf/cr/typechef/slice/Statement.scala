package de.tu_bs.cs.isf.cr.typechef.slice

import de.fosd.typechef.parser.c.{PrettyPrinter, AST, CFGStmt, Id}
import de.fosd.typechef.error.Position
import scala.collection.mutable
import java.util
import scala.collection.JavaConversions.asScalaSet
import de.fosd.typechef.featureexpr.FeatureExpr
import scala.collection.mutable.ListBuffer

class Statement(val function: Function, private [slice] val astStatement: CFGStmt) {

	private val system = function.system

	private val range = {
		val posFrom = astStatement.getPositionFrom
		val posTo = astStatement.getPositionTo

		val (from, to) = system.lineIndex.trimRange((posFrom.getLine, posFrom.getColumn), (posTo.getLine, posTo.getColumn), true)

		(system.lineIndex.getOffset(from), system.lineIndex.getOffset(to))
	}

	def line = if(astStatement.hasPosition) astStatement.getPositionFrom.getLine else -1

	def unparse(): String = {
		PrettyPrinter.print(astStatement).replace('\n', ' ').replace('\r', ' ')
	}

	def fromOffset: Int = range._1
	def toOffset: Int = range._2

	override def toString = {
		astStatement.getClass.getSimpleName + "@" + astStatement.getPositionFrom.getLine + "," +
				astStatement.getPositionFrom.getColumn + "-" + astStatement.getPositionTo.getLine +
				"," + astStatement.getPositionTo.getColumn
	}

	def sliceForward(): List[(Statement, FeatureExpr)] = {
		function.dependenceGraph.sliceForward(this)
	}

	override def equals(other: Any): Boolean = other match {
		case that: Statement => astStatement eq that.astStatement
		case _ => false
	}

	override def hashCode(): Int = {
		31 * System.identityHashCode(astStatement)
	}
}
