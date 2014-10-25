package de.tu_bs.cs.isf.cr.typechef.slice

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{AST, Id, CFGStmt, FunctionDef}
import de.tu_bs.cs.isf.cr.typechef
import de.tu_bs.cs.isf.cr.typechef.slice
import scala.collection.JavaConversions._
import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import de.fosd.typechef.crewrite.CFGHelper

class Function(private [slice] val system: System, private[slice] val definition: FunctionDef) {

	lazy val reverseCfg = new ControlFlowGraph(definition, system.astEnv, true)

	private lazy val statements = reverseCfg.nodes.map(_._1)

	lazy val dependenceGraph: DependenceGraph = createDependenceGraph()

	private def createStatement(cfgStmt: CFGStmt): Statement = {
		new Statement(this, cfgStmt)
	}

	private def createDependenceGraph(): DependenceGraph = {
		val reachableUses = new ReachableUsesAnalysis(reverseCfg, system.astEnv, system.declUseMap,
				system.useDeclMap)
		val controlDependence = new ControlDependenceAnalysis(reverseCfg, system.astEnv)

		val dependences = new ListBuffer[Dependence]

		for(cfgStmt <- statements) {

			for ((dependentStmt, expr) <- controlDependence.getDependentNodes(cfgStmt)) {
				dependences += ((createStatement(cfgStmt), createStatement(dependentStmt), expr, ControlDependence))
			}

			for ((id, expr) <- reachableUses.getUses(cfgStmt)) {
				getEnclosingStatement(id, reachableUses) match {
					case Some(dependentStmt) => {
						dependences += ((createStatement(cfgStmt), createStatement(dependentStmt), expr, DataDependence))
					}

					case None => println("Warning: enclosing statement not found")
				}
			}
		}

		new DependenceGraph(dependences.toList)
	}

	private def getEnclosingStatement(id: Id, analysis: MonotoneFramework[_]): Option[CFGStmt] = {
		// retrieve the smallest valid enclosing statement

		var node: AST = id

		while(node != null && !analysis.isValid(node)) {
			node = AstUtil.parentAST(node, system.astEnv)
		}

		if(node == null)
			None
		else
			Some(node.asInstanceOf[CFGStmt])
	}


	def getStatements(line: Int): List[Statement] = {
		// TODO warn on missing position information
		statements.filter(stmt => stmt.hasPosition && (stmt.getPositionFrom.getLine == line))
			.map(new Statement(this, _))
	}

	def name = definition.getName

	def getStatements(): List[Statement] = {
		statements.map(new Statement(this, _))
	}

	override def toString = definition.toString
}
