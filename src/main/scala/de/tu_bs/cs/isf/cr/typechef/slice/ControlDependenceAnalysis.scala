package de.tu_bs.cs.isf.cr.typechef.slice

import de.fosd.typechef.parser.c._
import scala.collection.mutable.ListBuffer
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.conditional.Opt

class ControlDependenceAnalysis(reverseCfg: ControlFlowGraph, astEnv: ASTEnv) extends MonotoneFramework[(Unique[CFGStmt], Boolean)](reverseCfg, true) {

	override protected def gen(node: AST): List[((Unique[CFGStmt], Boolean), FeatureExpr)] = {
		node match {
			case stmt: CFGStmt => List(((unique(stmt), true), astEnv.featureExpr(stmt)))
			case _ => List()
		}
	}

	override val initialFacts = reverseCfg.nodes.map({ case(node, astExpr) => ((unique(node), false), astExpr) })

	override def bottom = List()

	override protected def kill(node: AST): List[((Unique[CFGStmt], Boolean), FeatureExpr)] = {
		node match {
			case stmt: CFGStmt => List(((unique(stmt), false), astEnv.featureExpr(node)))
			case _ => List()
		}
	}

	def getDependentNodes(node: CFGStmt): List[(CFGStmt, FeatureExpr)] = {
		// http://infolab.stanford.edu/~ullman/dragon/w06/lectures/cs243-lec08-wei.ppt
		// Y control-dependent on X iff.
		// - Y does not postdominate X
		// - ex. (X -> Z) such that Y postdominates Z
		// the latter implies that there is a direct dependence (i.e. non-transitive) of Y on X

		// CFG is reversed, therefore preds are actually succs
		val succs = reverseCfg.predecessors(node)

		val result = new ListBuffer[(CFGStmt, FeatureExpr)]

		for((canStmt, canExpr) <- getNonPostDominators(node)) {
			var joinedExpr = FeatureExprFactory.False

			// TODO presence condition of "postdominates" must be incorporated
			for((succ, expr) <- succs if postdominates(canStmt, succ)) {
				joinedExpr = joinedExpr or (expr and canExpr)
			}

			if(joinedExpr.isSatisfiable())
				result += ((canStmt, joinedExpr))
		}

		result.filter(_._2.isSatisfiable()).toList
	}

	private def getNonPostDominators(node: CFGStmt): List[(CFGStmt, FeatureExpr)] = {
		val factMap: Map[(Unique[CFGStmt], Boolean), FeatureExpr] = in(node).toMap
		val result = new ListBuffer[(CFGStmt, FeatureExpr)]()

		for (((uniqueStmt, reached), expr) <- factMap) {
			if (reached) {
				factMap.get((uniqueStmt, false)) match {
					case Some(initExpr) => result += ((uniqueStmt.value, expr.and(initExpr)))
					case _ =>
				}
			}
		}

		result.toList
	}

	private def postdominates(postdominator: CFGStmt, node: CFGStmt): Boolean = {
		val facts = in(node)
		val uniquePostdom = unique(postdominator)

		facts.exists({ case ((stmt, reachable), _) => stmt == uniquePostdom && reachable}) &&
			!facts.exists({case ((stmt, reachable), _) => stmt == uniquePostdom && !reachable})
	}

	solve()
}
