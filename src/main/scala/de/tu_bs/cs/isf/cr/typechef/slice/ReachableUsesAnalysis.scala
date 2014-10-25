package de.tu_bs.cs.isf.cr.typechef.slice

import de.fosd.typechef.parser.c.{Id, ASTEnv, AST}
import de.fosd.typechef.typesystem._
import scala.collection.mutable.ListBuffer
import de.fosd.typechef.featureexpr.FeatureExpr

class ReachableUsesAnalysis(reverseCfg: ControlFlowGraph, astEnv: ASTEnv, declUseMap: DeclUseMap, useDeclMap: UseDeclMap) extends MonotoneFramework[Unique[Id]](reverseCfg, true) {

	protected def bottom: List[Fact] = List()

	override def gen(a: AST) = {
		AstUtil.uses(a).map(id => (unique(id), astEnv.featureExpr(id)))
	}

	// TODO feature expression using scope (cf. paper/wiki)
	override def kill(a: AST) = {
		getOccurrences(a).map(id => (unique(id), astEnv.featureExpr(id)))
	}

	// TODO feature expressions (scope problem etc.)?
	// (see new paper version)
	def getUses(a: AST): List[(Id, FeatureExpr)] = {
		val declarations = AstUtil.defines(a).flatMap(useDeclMap.get)

		val reachableIds = out(a).map(unique => (unique._1.value, unique._2))

		val result = new ListBuffer[(Id, FeatureExpr)]
		for ((id, expr) <- reachableIds) {
			val idDeclarations = useDeclMap.get(id)
			if(idDeclarations != null && idDeclarations.exists(declarations.contains)) {
				result.append((id, expr))
			}
		}

		result.toList
	}

	private def getOccurrences(a: AST): List[Id] = {
		val ids = new ListBuffer[Id]

		for (definition <- AstUtil.defines(a) if useDeclMap.containsKey(definition)) {
			for(declaration <- useDeclMap.get(definition) if declUseMap.containsKey(declaration)) {
				for (use <- declUseMap.get(declaration)) {
					ids.append(use)
				}
			}
		}
		ids.toList
	}

	solve()
}
