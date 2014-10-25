package de.tu_bs.cs.isf.cr.typechef.slice

import scala.collection.immutable.TreeMap
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

// TODO possible shift due to charset conversions, maybe use byte[]
class LineIndex(code: String) {

	// offsets of line beginnings to lines
	private val offsetMap: TreeMap[Int, Int] = indexLines()
	// lines to offsets
	private val lineMap: TreeMap[Int, Int] = offsetMap.map(_.swap)

	private def indexLines() = {
		var line = 1
		var offset = -1
		var lineStart = 0
		var result = TreeMap[Int, Int]()

		for (char <- code) {
			offset += 1

			if(char == '\n') {
				result = result + ((lineStart, line))
				lineStart = offset + 1
				line += 1
			}
		}

		if(offset >= lineStart)
			result = result + ((lineStart, line))

		result
	}

	def getPosition(offset: Int): Option[(Int,Int)] = {
		if(offset < 0 || offset >= code.length)
			return None

		val (lineOffset, line) = offsetMap.to(offset).last

		Some((line, offset - lineOffset))
	}

	def getOffset(line: Int, column: Int): Int = {
		lineMap.get(line) match {
			case Some(lineOffset) => lineOffset + column
			case None => -1
		}
	}

	def getOffset(position: (Int, Int)): Int = getOffset(position._1, position._2)

	def trimRange(from: (Int, Int), to: (Int, Int), toExclusive: Boolean = false): ((Int, Int), (Int, Int)) = {
		var fromOffset = getOffset(from)
		var toOffset = getOffset(to)

		if(toExclusive)
			toOffset -= 1

		while(fromOffset < code.length&& Character.isWhitespace(code(fromOffset)))
			fromOffset += 1

		while(toOffset >= 0 && Character.isWhitespace(code(toOffset)))
			toOffset -= 1

		(getPosition(fromOffset).get, getPosition(toOffset).get)
	}
}

object LineIndex {
	def fromFile(filename: String): LineIndex = {
		val bytes = Files.readAllBytes(Paths.get(filename))
		new LineIndex(new String(bytes, StandardCharsets.UTF_8))
	}
}
