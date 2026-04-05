package hydra.extract.util

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.util.*

def comparison(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.util.Comparison] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Name, hydra.util.Comparison](hydra.extract.core.unitVariant(cx)("hydra.util.Comparison")(graph)(term))((fname: hydra.core.Name) =>
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.util.Comparison]](hydra.lib.equality.equal[scala.Predef.String](fname)("equalTo"))(Right(hydra.util.Comparison.equalTo))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error],
     hydra.util.Comparison]](hydra.lib.equality.equal[scala.Predef.String](fname)("lessThan"))(Right(hydra.util.Comparison.lessThan))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error],
     hydra.util.Comparison]](hydra.lib.equality.equal[scala.Predef.String](fname)("greaterThan"))(Right(hydra.util.Comparison.greaterThan))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("expected comparison but found ")(fname)),
     cx))))))
