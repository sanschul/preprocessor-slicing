package de.tu_bs.cs.isf.cr.typechef.slice

import de.fosd.typechef.options.FrontendOptionsWithConfigFiles
import de.fosd.typechef.lexer.Main
import de.fosd.typechef.parser.c.{PrettyPrinter, CFGStmt, CLexer}
import de.tu_bs.cs.isf.cr.typechef.compat.Bridge
import java.io.PrintWriter


object SliceMain {
	def main (_args: Array[String]) {
		val defaultArgs = System.getenv("CR_SLICE_DEFAULT")

		val args = if(_args.length == 0 && defaultArgs != null) defaultArgs.split(",") else _args

		if(args.length < 2) {
			println("usage: [--dg] [--cfg] <filename> <line>")
			return
		}

		var offset = 0

		val dg = args.contains("--dg")
		val cfg = args.contains("--cfg")

		if(dg)
			offset += 1
		if(cfg)
			offset += 1

		val line = args(offset + 1).toInt
		val filename = args(offset)

		val options = new FrontendOptionsWithConfigFiles()
		options.parseOptions(Array(filename))
		options.setPrintToStdOutput(false)

		// TODO move this to bridge
		val lexer = new Main()
		val tokens = CLexer.prepareTokens(lexer.run(options, true))
		val bridge = new Bridge(options, tokens)
		val system = new System(bridge.ast, bridge.typeSystem, bridge.completeFeatureModel, LineIndex.fromFile(filename))

		val statements = system.findStatements(line)

		if(statements.isEmpty) {
			println("No statement found in line " + line)
			return
		}

		val statement = statements.head
		println("criterion: " + statement.unparse())

		if(cfg)
			writeFlowGraph(statement.function.reverseCfg, filename + ".cfg.dot")
		if(dg)
			writeDependenceGraph(statement.function.dependenceGraph, filename + ".pdg.dot")
		println()

		val slice = statement.sliceForward()
		println("Slice:")
		for((statement, cond) <- slice.sortWith(_._1.line < _._1.line)) {
			println("%5d".format(statement.line) + " ["+ AstUtil.strip(cond)+ "] -- " + statement.unparse())
		}
	}

	private def writeDependenceGraph(pdg: DependenceGraph, filename: String) {
		val pw = new PrintWriter(filename)
		pw.print(pdg.toDotString)
		pw.close()
		println("dependence graph written to " + filename)
	}

	private def writeFlowGraph(cfg: ControlFlowGraph, filename: String) {
		val pw = new PrintWriter(filename)
		pw.print(cfg.toDotString)
		pw.close()
		println("control flow graph written to " + filename)
	}
}