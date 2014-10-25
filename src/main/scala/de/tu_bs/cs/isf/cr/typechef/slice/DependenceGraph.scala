package de.tu_bs.cs.isf.cr.typechef.slice

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.parser.c.{PrettyPrinter, CFGStmt}
import org.kiama.attribution.Attribution._
import java.util
import scala.collection.mutable

abstract sealed class DependenceType
object DataDependence extends DependenceType
object ControlDependence extends DependenceType

class DependenceGraph(dependences: List[Dependence]) {

	def sliceForward(criterion: Statement): List[(Statement, FeatureExpr)] = {
		val visited = new mutable.HashSet[Statement]()

		val queue = new mutable.Queue[Statement]()
		queue.enqueue(criterion)

		while(queue.nonEmpty) {
			val statement = queue.dequeue()
			visited.add(statement)

			for((_, succ, _, _) <- successors(statement)) {
				if(!visited.contains(succ)) {
					visited.add(succ)
					queue.enqueue(succ)
				}
			}
		}

		val fixedPoint = new FixedPoint(List(criterion.astStatement))

		visited.map(stmt => (stmt, fixedPoint.condition(stmt.astStatement)))
			.filter(_._2.isSatisfiable()).toList
	}

	private def successors(statement: Statement) = {
		dependences.filter({ case (node, _, _, _) => node == statement })
	}

	private def predecessors(cfgStmt: CFGStmt) = {
		dependences.filter({ case (_, node, _, _) => node.astStatement eq cfgStmt })
	}

	// TODO pc of criterion node should be its presence condition in the AST (or something similar) rather than True
	// (not really a problem, AST pc already contained in dep pcs.
	// however, the result pc of the criterion is currently always True, even for variable nodes.)
	private class FixedPoint(criterion: List[CFGStmt]) {
		private val criterionSet: util.Set[CFGStmt] =
			util.Collections.newSetFromMap(new util.IdentityHashMap())
		criterion.foreach(stmt => criterionSet.add(stmt))

		val condition: CFGStmt => FeatureExpr = circular(FeatureExprFactory.False) {
			case cfgStmt => {
				var result = FeatureExprFactory.False

				if(criterionSet.contains(cfgStmt)) {
					result = FeatureExprFactory.True
				}
				else {
					for ((pred, _, depExpr, _) <- predecessors(cfgStmt)) {
						result = result or (condition(pred.astStatement) and depExpr)
					}
				}

				result
			}
		}
	}

	def toDotString: String = {
		def tooltip(stmt: Statement) = {
			stmt.unparse().replace('"', '\'')
		}

		def stmtId(cfgStmt: CFGStmt) = System.identityHashCode(cfgStmt)

		def typeColor(depType: DependenceType) = {
			depType match {
				case ControlDependence => "blue"
				case DataDependence => "black"
			}
		}

		val nodes = dependences.flatMap({ case (node, dependent, _, _) => List(node, dependent) }).distinct
			.map(node => "\"" +stmtId(node.astStatement)+"\"[label=\"" + node.line +"\",tooltip=\""+tooltip(node)+"\"];")

		val edges = dependences.map {
			case (node, dependent, depExpr, depType) =>

				"\"" +stmtId(node.astStatement) + "\"->\"" + stmtId(dependent.astStatement) + "\"" +
					"[color="+typeColor(depType)+",label=\" "+(if(!depExpr.isTautology()) AstUtil.strip(depExpr) else "")+" \",fontcolor=red];"

		}.distinct

		"digraph \"PDG\" { node [shape=record];\n "+ nodes.foldLeft("")(_ + "\n" + _) + edges.foldLeft("")(_ + "\n" + _) + "\n}"
	}
}
