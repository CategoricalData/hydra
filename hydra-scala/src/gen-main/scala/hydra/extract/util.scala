package hydra.extract.util

import hydra.core.*

import hydra.errors.*

import hydra.util.*

def comparison[T0](cx: T0)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error, hydra.util.Comparison] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Name, hydra.util.Comparison](hydra.extract.core.unitVariant("hydra.util.Comparison")(graph)(term))((fname: hydra.core.Name) =>
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.util.Comparison]](hydra.lib.equality.equal[scala.Predef.String](fname)("equalTo"))(Right(hydra.util.Comparison.equalTo))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.util.Comparison]](hydra.lib.equality.equal[scala.Predef.String](fname)("lessThan"))(Right(hydra.util.Comparison.lessThan))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.util.Comparison]](hydra.lib.equality.equal[scala.Predef.String](fname)("greaterThan"))(Right(hydra.util.Comparison.greaterThan))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("comparison",
     fname))))))))
