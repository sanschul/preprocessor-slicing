package de.tu_bs.cs.isf.cr.typechef.slice

import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{DeclUseMap, UseDeclMap, CDeclUse, CTypeSystemFrontend}
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.crewrite.CFGHelper
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.parser.c.FunctionDef
import scala.collection.JavaConversions._

class System(astRoot: TranslationUnit, typeSystem: CTypeSystemFrontend with CDeclUse,
		private[slice] val featureModel: FeatureModel, private [slice] val lineIndex: LineIndex) {

	private [slice] val astEnv = CASTEnv.createASTEnv(astRoot)

	private [slice] val cfgHelper = new CFGHelper {}

	private [slice] val useDeclMap = new DeclUseMap()
	private [slice] val declUseMap = new UseDeclMap()

	private def makeSymbolsReflexive() {
		val tcDeclUseMap = typeSystem.getDeclUseMap
		val tcUseDeclMap = typeSystem.getUseDeclMap

		useDeclMap.putAll(tcUseDeclMap)

		for(entry <- tcDeclUseMap.entrySet()) {
			val declId = entry.getKey
			declUseMap.put(declId, entry.getValue :+ declId)

			val decls = useDeclMap.get(declId)
			if(decls == null)
				useDeclMap.put(declId, List(declId))
			else
				useDeclMap.put(declId, decls :+ declId)
		}
	}

	makeSymbolsReflexive()

	private val functions: List[Function] = AstUtil.filterASTElems[FunctionDef](astRoot)
		.map(new Function(this, _))

	def getFunction(name: String): List[Function] = {
 		functions.filter(_.name == name)
	}

	def getFunctions(): List[Function] = functions

	def resolve(fileOffset: Int): List[Statement] = {
		lineIndex.getPosition(fileOffset) match {
			case Some((line, column)) => {
				functions.flatMap(_.getStatements())
					.filter(stmt => positionContained(line, column, stmt.astStatement))
			}

			case None => List()
		}
	}

	// TODO filter filename (includes)
	def findStatements(line: Int): List[Statement] = {
		functions.flatMap(_.getStatements(line))
	}

	private def positionContained(line: Int, column: Int, node: AST): Boolean = {
		val posFrom = node.getPositionFrom
		val posTo = node.getPositionTo

		// TypeChef's to-position is exclusive (i.e. beginning of next AST node),
		// convert to inclusive to-position and remove any surrounding whitespace
		val ((fromLine, fromCol),(toLine, toCol)) =
			lineIndex.trimRange((posFrom.getLine, posFrom.getColumn), (posTo.getLine, posTo.getColumn), true)

		(fromLine < line || (fromLine == line && fromCol <= column)) &&
				(line < toLine || (line == toLine && column <= toCol))
	}

}
