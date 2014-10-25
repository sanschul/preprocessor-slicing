package de.tu_bs.cs.isf.cr.typechef.slice

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.parser.c._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class ControlFlowGraph(functionDef: FunctionDef, astEnv: ASTEnv, val reversed: Boolean, cfgHelper: CFGHelper = new CFGHelper {}) {

	private def accept(node: AST): Boolean = {
		node.isInstanceOf[CFGStmt] && !node.isInstanceOf[FunctionDef]
	}

	def successors(node: CFGStmt): List[(CFGStmt, FeatureExpr)] = {
		neighbors(node, reversed)
	}

	def predecessors(node: CFGStmt): List[(CFGStmt, FeatureExpr)] = {
		neighbors(node, !reversed)
	}

	def reverse: ControlFlowGraph = {
		new ControlFlowGraph(functionDef, astEnv, !reversed, cfgHelper)
	}

	private def neighbors(node: CFGStmt, reversed: Boolean) = {
		(if(reversed) cfgHelper.pred(node, astEnv) else cfgHelper.succ(node, astEnv))
			.filter(next => accept(next.entry))
			.map({ case Opt(expr, next: CFGStmt) => (next, expr) })
	}

	val entryPoints: List[(CFGStmt, FeatureExpr)] = {
		// join possible duplicates
		val nodes = new util.IdentityHashMap[CFGStmt, FeatureExpr]()

		for((next, expr) <- successors(functionDef)) {
			var nextVal = nodes.get(next);
			if(nextVal == null)
				nextVal = FeatureExprFactory.False

			nodes.put(next, expr.or(nextVal))
		}

		nodes.toList
	}

	val edges: List[(CFGStmt, CFGStmt, FeatureExpr)] = {
		val flowAll = if(reversed) cfgHelper.getAllPred(functionDef, astEnv) else
			cfgHelper.getAllSucc(functionDef, astEnv)

		val controlNeighbors: List[(AST, CFG)] =
			flowAll.filter({ case (from, _) => accept(from) })
					.map({ case(from, nexts) => (from, nexts.filter(next => accept(next.entry)))})

		controlNeighbors.flatMap(p => p._2.map(n => (p._1, n)))
				.map({ case(from: CFGStmt, Opt(expr, to: CFGStmt)) => (from, to, expr) }).reverse
	}

	val nodes: List[(CFGStmt, FeatureExpr)] = {
		val nodeSet: util.Set[CFGStmt] = util.Collections.newSetFromMap(new util.IdentityHashMap())
		for((from, to, _) <- edges) {
			nodeSet.add(from)
			nodeSet.add(to)
		}

		val result = new ListBuffer[(CFGStmt, FeatureExpr)]

		for(node <- nodeSet) {
			result += ((node, astEnv.featureExpr(node)))
		}

		result.toList
	}

	// TODO maybe refactor this (shares code with DG class)
	def toDotString: String = {
		val nodeFormat = "\"%d\"[label=\"%s\",tooltip=\"%s\",fillcolor=white,color=\"%s\"];"
		val edgeFormat = "\"%d\"->\"%d\"[label=\"%s\",fontcolor=red];"

		def tooltip(stmt: CFGStmt) = {
			PrettyPrinter.print(stmt).replace('"', '\'').replace('\r',' ').replace('\n', ' ')
		}

		def stmtId(cfgStmt: CFGStmt) = System.identityHashCode(cfgStmt)

		val nodeStrs = nodes.map {
			case (node, expr) =>
				val entryInfo = entryPoints.find { case(entryNode, _) => node eq entryNode } match {
					case Some((entryNode, entryExpr)) => "|" + AstUtil.strip(entryExpr)
					case None => ""
				}

				nodeFormat.format(stmtId(node),
					(if(node.hasPosition) node.getPositionFrom.getLine else -1) + entryInfo,
					tooltip(node),
					if(entryInfo.isEmpty) "black" else "blue")
		}

		val edgeStrs = edges.map { case (from, to, expr) => edgeFormat.format(stmtId(from), stmtId(to), if(!expr.isTautology()) AstUtil.strip(expr) else "") }

		"digraph \"CFG\" { rankdir=BT; node [shape=record];\n "+ nodeStrs.foldLeft("")(_ + "\n" + _) + edgeStrs.foldLeft("")(_ + "\n" + _) + "\n}"
	}
}
