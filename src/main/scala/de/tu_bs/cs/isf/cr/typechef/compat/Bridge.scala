package de.tu_bs.cs.isf.cr.typechef.compat

import de.fosd.typechef.options.FrontendOptions
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c.CParser
import de.fosd.typechef.parser.c.CToken
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.parser.c.EnforceTreeHelper
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.ParserMain
import de.fosd.typechef.parser.c.SilentParserOptions
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.typesystem.CDeclUse
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import java.util

class Bridge(options: FrontendOptions, tokenReader: TokenReader[CToken,CTypeContext]) extends EnforceTreeHelper {
	
	lazy val ast: TranslationUnit = {
		val cParser = new CParser(options.getLexerFeatureModel, false)
		val parserMain = new ParserMain(cParser)
		val ast = parserMain.parserMain(tokenReader, SilentParserOptions).asInstanceOf[TranslationUnit]
		if(ast == null)
			throw new TypeChefException("parsing failed");
		prepareAST(ast)
	}
	
	lazy val typeSystem = {
		val frontend = new CTypeSystemFrontend(ast, completeFeatureModel, options) /* with CTypeCache */ with CDeclUse
		frontend.checkASTSilent
		if(!frontend.errors.isEmpty)
			throw new TypeChefException("typechecking failed");
		frontend
	}

	lazy val completeFeatureModel = options.getTypeSystemFeatureModel
			.and(options.getLocalFeatureModel).and(options.getFilePresenceCondition)
	
	def declarations: util.Map[Id, util.Set[Id]] = typeSystem.getUntouchedDeclUseMap
	
	def typeErrors = !typeSystem.errors.isEmpty
	
}

class TypeChefException(message: String = null, cause: Throwable = null) 
		extends RuntimeException(message, cause) {
}
