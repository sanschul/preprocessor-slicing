package de.tu_bs.cs.isf.cr.typechef.slice

import scala.collection.mutable
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crewrite.{CFG, CFGHelper}
import de.fosd.typechef.parser.c._
import java.util
import scala.collection.JavaConversions._
import de.fosd.typechef.conditional.Opt

// TODO feature model
abstract class MonotoneFramework[T](cfg: ControlFlowGraph, may: Boolean) {
	if(!may)
		throw new IllegalArgumentException("'must' analysis not supported yet")

	type Fact = (T, FeatureExpr)

	private type AnnotatedFactSet = Map[T, FeatureExpr]
	private val facts = new util.IdentityHashMap[AST, AnnotatedFactSet]()
	private val operation: Operation = if(may) Union else Intersection

	private val uniqueObjects = new util.IdentityHashMap[AnyRef, Unique[_]]()

	protected val initialFacts: List[Fact] = List()
	protected def gen(node: AST): List[Fact]
	protected def kill(node: AST): List[Fact]
	protected def bottom: List[Fact]

	def in(node: AST): List[Fact] = {
		checkValid(node)
		if(!cfg.reversed)
			circle(node)
		else
			point(node)
	}

	def out(node: AST): List[Fact] = {
		checkValid(node)
		if(!cfg.reversed)
			point(node)
		else
			circle(node)
	}

	private def checkValid(node: AST) {
		if(!isValid(node))
			throw new IllegalArgumentException("invalid node specified: " + AstUtil.nodeInfo(node))
	}

	/**
	 * Checks if node is a valid control flow statement in this analysis.
	 * @param node
	 */
	def isValid(node: AST): Boolean = {
		facts.containsKey(node)
	}

	final def unique[S](value: S): Unique[S] = {
		val uniqueObj = uniqueObjects.get(value)
		if(uniqueObj == null) {
			val newObj = new Unique(value)
			uniqueObjects.put(value.asInstanceOf[AnyRef], newObj)
			newObj.asInstanceOf[Unique[S]]
		}
		else {
			uniqueObj.asInstanceOf[Unique[S]]
		}
	}

	private def circle(node: AST): List[Fact] = {
		filterSatisfiable(facts.get(node)).toList
	}

	private def point(node: AST): List[Fact] = {
		filterSatisfiable(transfer(node, facts.get(node))).toList
	}

	private def filterSatisfiable(set: AnnotatedFactSet): AnnotatedFactSet = {
		set.filter(_._2.isSatisfiable())
	}

	private sealed abstract class Operation {
		def combine(set1: AnnotatedFactSet, set2: AnnotatedFactSet): AnnotatedFactSet
		def compare(set1: AnnotatedFactSet, set2: AnnotatedFactSet): Boolean
	}

	// TODO not working
	private object Intersection extends Operation {
		def combine(set1: AnnotatedFactSet, set2: AnnotatedFactSet): AnnotatedFactSet = {
			var result: AnnotatedFactSet = Map()

			for((fact, expr) <- set1) {
				set2.get(fact) match {
					case None =>
					case Some(otherExpr) => result = result + ((fact, expr.and(otherExpr)))
				}
			}

			result
		}

		def compare(set1: AnnotatedFactSet, set2: AnnotatedFactSet): Boolean = {
			set1.size == set2.size
		}
	}

	private object Union extends Operation {
		def combine(set1: AnnotatedFactSet, set2: AnnotatedFactSet): AnnotatedFactSet = {
			var result = set1

			for(annotated@(fact, expr) <- set2) {
				set1.get(fact) match {
					case None => result = result + annotated
					case Some(otherExpr) => result = result + ((fact, expr.or(otherExpr)))
				}
			}

			result
		}

		def compare(set1: AnnotatedFactSet, set2: AnnotatedFactSet): Boolean = {
			set1.size == set2.size && set1.forall(fact => (set2.contains(fact._1) && set2.get(fact._1).get.equiv(fact._2).isTautology()))
		}
	}

	private def difference(set1: AnnotatedFactSet, set2: AnnotatedFactSet): AnnotatedFactSet = {
		var result = set1

		for((fact, expr) <- set2) {
			set1.get(fact) match {
				case None =>
				case Some(initialExpr) => result = result + ((fact, initialExpr.and(expr.not)))
			}
		}

		result
	}

	private def transfer(node: AST, facts: AnnotatedFactSet): AnnotatedFactSet = {
		Union.combine(difference(facts, kill(node).toMap), gen(node).toMap)
	}

	protected def solve() {
		// algorithm adapted from Nielson et al., Principles of Program Analysis

		val queue = new mutable.Queue[(CFGStmt, CFGStmt, FeatureExpr)]()
		queue ++= cfg.edges

		for((node, expr) <- cfg.entryPoints) {
			facts.put(node, initialFacts.map({ case (fact, factExpr) => (fact, factExpr.and(expr))}).toMap)
		}

		for((node, expr) <- cfg.nodes if !facts.containsKey(node)) {
			facts.put(node, bottom.map({ case (fact, factExpr) => ((fact, factExpr.and(expr))) }).toMap)
		}

		while(queue.nonEmpty) {
			val (node, next, edgeExpr) = queue.dequeue()

			val nodeFacts = facts.get(node)
			val nextFacts = facts.get(next)

			val updatedFacts = operation.combine(transfer(node, nodeFacts)
					.map({ case (fact, expr) => (fact, expr.and(edgeExpr))}), nextFacts)

			// change detection must involve fact presence conditions (no feature model though)
			// TODO evaluate performance
			if(!operation.compare(nextFacts, updatedFacts)) {
				facts.put(next, updatedFacts)

				for((nextnext, nextnextExpr) <- cfg.successors(next)) {
					queue.enqueue((next, nextnext, nextnextExpr))
				}
			}
		}
	}
}

// wrapper to hide any custom equals/hashcode implementation (of case classes, for instance)
class Unique[T](val value: T) {
	override def toString = "Unique("+value+","+System.identityHashCode(value)+")"
}