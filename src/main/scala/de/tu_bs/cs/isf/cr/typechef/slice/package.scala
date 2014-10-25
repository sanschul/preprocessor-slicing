package de.tu_bs.cs.isf.cr.typechef

import de.fosd.typechef.parser.c.CFGStmt
import de.fosd.typechef.featureexpr.FeatureExpr

package object slice {
	// (X, Y, P, _) = Y is dependent on X wrt. condition P
	type Dependence = (Statement, Statement, FeatureExpr, DependenceType)
}
