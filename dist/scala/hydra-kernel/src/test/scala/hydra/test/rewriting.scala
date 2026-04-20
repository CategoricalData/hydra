package hydra.test.rewriting

import hydra.coders.*

import hydra.core.*

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("rewriting",
   None, Seq(hydra.testing.TestGroup("foldOverTerm", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("collect labels from single node - pre-order",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Literal,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Term]((lit: hydra.core.Literal) => hydra.core.Term.literal(lit))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Seq[hydra.core.Literal]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[hydra.core.Literal](Seq(acc, term match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p) match
    case hydra.core.Term.literal(v_Term_literal_lit) => Seq(v_Term_literal_lit)
    case _ => Seq()
  case _ => Seq())))(Seq())(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("a")),
     hydra.core.Term.list(Seq()))))))), hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("a"))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()), hydra.testing.TestCaseWithMetadata("collect labels from tree - pre-order",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Literal,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((lit: hydra.core.Literal) => hydra.core.Term.literal(lit))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Seq[hydra.core.Literal]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[hydra.core.Literal](Seq(acc, term match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p) match
    case hydra.core.Term.literal(v_Term_literal_lit) => Seq(v_Term_literal_lit)
    case _ => Seq()
  case _ => Seq())))(Seq())(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("a")),
     hydra.core.Term.list(Seq(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("b")),
     hydra.core.Term.list(Seq()))), hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("c")),
     hydra.core.Term.list(Seq(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("d")),
     hydra.core.Term.list(Seq()))))))))))))))), hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("a")),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("b")), hydra.core.Term.literal(hydra.core.Literal.string("c")),
     hydra.core.Term.literal(hydra.core.Literal.string("d"))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("collect labels from single node - post-order",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Literal,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((lit: hydra.core.Literal) => hydra.core.Term.literal(lit))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.post)((acc: Seq[hydra.core.Literal]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[hydra.core.Literal](Seq(acc, term match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p) match
    case hydra.core.Term.literal(v_Term_literal_lit) => Seq(v_Term_literal_lit)
    case _ => Seq()
  case _ => Seq())))(Seq())(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("a")),
     hydra.core.Term.list(Seq()))))))), hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("a"))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()), hydra.testing.TestCaseWithMetadata("collect labels from tree - post-order",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Literal,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((lit: hydra.core.Literal) => hydra.core.Term.literal(lit))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.post)((acc: Seq[hydra.core.Literal]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[hydra.core.Literal](Seq(acc, term match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p) match
    case hydra.core.Term.literal(v_Term_literal_lit) => Seq(v_Term_literal_lit)
    case _ => Seq()
  case _ => Seq())))(Seq())(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("a")),
     hydra.core.Term.list(Seq(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("b")),
     hydra.core.Term.list(Seq()))), hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("c")),
     hydra.core.Term.list(Seq(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("d")),
     hydra.core.Term.list(Seq()))))))))))))))), hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("b")),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("d")), hydra.core.Term.literal(hydra.core.Literal.string("c")),
     hydra.core.Term.literal(hydra.core.Literal.string("a"))))))), None, Seq()), hydra.testing.TestCaseWithMetadata("sum int32 literals",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.variable("x"))), hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(10)))))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(52)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("collect list lengths - pre-order",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((n: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(n))))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Seq[Int]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[Int](Seq(acc, term match
  case hydra.core.Term.list(v_Term_list_elems) => Seq(hydra.lib.lists.length[hydra.core.Term](v_Term_list_elems))
  case _ => Seq())))(Seq())(hydra.core.Term.list(Seq(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("foo")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("bar")))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.lambda(hydra.core.Lambda("x",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, hydra.core.Term.variable("x"))), hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("quux")))))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("collect list lengths - post-order",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((n: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(n))))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.post)((acc: Seq[Int]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[Int](Seq(acc, term match
  case hydra.core.Term.list(v_Term_list_elems) => Seq(hydra.lib.lists.length[hydra.core.Term](v_Term_list_elems))
  case _ => Seq())))(Seq())(hydra.core.Term.list(Seq(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("foo")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("bar")))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.lambda(hydra.core.Lambda("x",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, hydra.core.Term.variable("x"))), hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("quux")))))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))))),
     None, Seq()))), hydra.testing.TestGroup("rewriteType", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("String type in left side of either is replaced",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.string),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
     hydra.show.core.`type`(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String type in right side of either is replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.string))))), hydra.show.core.`type`(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String types in both sides of either are replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.string),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.string))))), hydra.show.core.`type`(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String type in nested either (left of left) is replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.string),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))),
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)))))),
     hydra.show.core.`type`(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))),
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String type in nested either (right of right) is replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.string))))))), hydra.show.core.`type`(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String types in complex nested either are all replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.string),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.string))), hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.string),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)))))))),
     hydra.show.core.`type`(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))),
     hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64))))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String in list type is replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.string)))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.`type`(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()), hydra.testing.TestCaseWithMetadata("String in function domain is replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.string),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)))))),
     hydra.show.core.`type`(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String in function codomain is replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.string))))), hydra.show.core.`type`(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("String in optional type is replaced",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.`type`(hydra.rewriting.rewriteType((recurse: (hydra.core.Type => hydra.core.Type)) =>
  (typ: hydra.core.Type) =>
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Type](typ)(hydra.core.Type.literal(hydra.core.LiteralType.string)))(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))(recurse(typ)))(hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.string)))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.`type`(hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()))), hydra.testing.TestGroup("rewriteTerm", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("string literal foo replaced with bar",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.literal(hydra.core.Literal.string("foo")))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.string("bar"))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in variable not changed",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.variable("x"))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(hydra.core.Term.variable("x")))), None, Seq()), hydra.testing.TestCaseWithMetadata("string in list",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("foo")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("baz")))))), hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("bar")),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("baz"))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("multiple strings in list", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("foo")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")), hydra.core.Term.literal(hydra.core.Literal.string("baz")))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.string("bar")),
     hydra.core.Term.literal(hydra.core.Literal.string("bar")), hydra.core.Term.literal(hydra.core.Literal.string("baz"))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in optional (just)",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.string("foo")))))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in function application",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("print"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")))))), hydra.show.core.term(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("print"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in lambda body", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.lambda(hydra.core.Lambda("x",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     None, hydra.core.Term.literal(hydra.core.Literal.string("foo")))))), hydra.show.core.term(hydra.core.Term.lambda(hydra.core.Lambda("x",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in nested applications", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("f"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("g"),
     hydra.core.Term.literal(hydra.core.Literal.string("foo")))))))), hydra.show.core.term(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("f"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("g"),
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in record field", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.record(hydra.core.Record("Person",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     Seq(hydra.core.Field("name", hydra.core.Term.literal(hydra.core.Literal.string("foo")))))))),
     hydra.show.core.term(hydra.core.Term.record(hydra.core.Record("Person", Seq(hydra.core.Field("name",
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("strings in multiple record fields", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.record(hydra.core.Record("Data",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     Seq(hydra.core.Field("a", hydra.core.Term.literal(hydra.core.Literal.string("foo"))),
     hydra.core.Field("b", hydra.core.Term.literal(hydra.core.Literal.string("baz"))),
     hydra.core.Field("c", hydra.core.Term.literal(hydra.core.Literal.string("foo")))))))),
     hydra.show.core.term(hydra.core.Term.record(hydra.core.Record("Data", Seq(hydra.core.Field("a",
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))), hydra.core.Field("b",
     hydra.core.Term.literal(hydra.core.Literal.string("baz"))), hydra.core.Field("c",
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in pair", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("foo")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))))),
     hydra.show.core.term(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.string("bar")),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in let binding value",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")), None)), hydra.core.Term.variable("x"))))),
     hydra.show.core.term(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
     hydra.core.Term.literal(hydra.core.Literal.string("bar")), None)), hydra.core.Term.variable("x")))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in let body", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))),
     None)), hydra.core.Term.literal(hydra.core.Literal.string("foo")))))), hydra.show.core.term(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))),
     None)), hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))), None,
     Seq()), hydra.testing.TestCaseWithMetadata("string in first case branch", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.cases(hydra.core.CaseStatement("Result",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     None, Seq(hydra.core.Field("success", hydra.core.Term.literal(hydra.core.Literal.string("foo"))),
     hydra.core.Field("error", hydra.core.Term.literal(hydra.core.Literal.string("baz")))))))),
     hydra.show.core.term(hydra.core.Term.cases(hydra.core.CaseStatement("Result",
     None, Seq(hydra.core.Field("success", hydra.core.Term.literal(hydra.core.Literal.string("bar"))),
     hydra.core.Field("error", hydra.core.Term.literal(hydra.core.Literal.string("baz"))))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in second case branch",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.cases(hydra.core.CaseStatement("Result",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     None, Seq(hydra.core.Field("success", hydra.core.Term.literal(hydra.core.Literal.string("baz"))),
     hydra.core.Field("error", hydra.core.Term.literal(hydra.core.Literal.string("foo")))))))),
     hydra.show.core.term(hydra.core.Term.cases(hydra.core.CaseStatement("Result",
     None, Seq(hydra.core.Field("success", hydra.core.Term.literal(hydra.core.Literal.string("baz"))),
     hydra.core.Field("error", hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in default branch",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.cases(hydra.core.CaseStatement("Result",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     Some(hydra.core.Term.literal(hydra.core.Literal.string("foo"))), Seq(hydra.core.Field("success",
     hydra.core.Term.literal(hydra.core.Literal.string("baz"))), hydra.core.Field("error",
     hydra.core.Term.literal(hydra.core.Literal.string("baz")))))))), hydra.show.core.term(hydra.core.Term.cases(hydra.core.CaseStatement("Result",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Some(hydra.core.Term.literal(hydra.core.Literal.string("bar"))), Seq(hydra.core.Field("success",
     hydra.core.Term.literal(hydra.core.Literal.string("baz"))), hydra.core.Field("error",
     hydra.core.Term.literal(hydra.core.Literal.string("baz"))))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string deeply nested in record in list in application",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("process"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.list(Seq(hydra.core.Term.record(hydra.core.Record("Item", Seq(hydra.core.Field("value",
     hydra.core.Term.literal(hydra.core.Literal.string("foo")))))))))))), hydra.show.core.term(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("process"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.list(Seq(hydra.core.Term.record(hydra.core.Record("Item", Seq(hydra.core.Field("value",
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in union inject value", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.inject(hydra.core.Injection("Result",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("success", hydra.core.Term.literal(hydra.core.Literal.string("foo"))))))),
     hydra.show.core.term(hydra.core.Term.inject(hydra.core.Injection("Result", hydra.core.Field("success",
     hydra.core.Term.literal(hydra.core.Literal.string("bar")))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in wrapped term", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.wrap(hydra.core.WrappedTerm("Email",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")))))), hydra.show.core.term(hydra.core.Term.wrap(hydra.core.WrappedTerm("Email",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in annotated term body", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("foo")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     Map())))), hydra.show.core.term(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("bar")),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Map()))))), None, Seq()), hydra.testing.TestCaseWithMetadata("string in first of multiple let bindings",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")), None), hydra.core.Binding("y",
     hydra.core.Term.literal(hydra.core.Literal.string("baz")), None)), hydra.core.Term.variable("x"))))),
     hydra.show.core.term(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
     hydra.core.Term.literal(hydra.core.Literal.string("bar")), None), hydra.core.Binding("y",
     hydra.core.Term.literal(hydra.core.Literal.string("baz")), None)), hydra.core.Term.variable("x")))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in second of multiple let bindings",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("baz")), None), hydra.core.Binding("y",
     hydra.core.Term.literal(hydra.core.Literal.string("foo")), None)), hydra.core.Term.variable("y"))))),
     hydra.show.core.term(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
     hydra.core.Term.literal(hydra.core.Literal.string("baz")), None), hydra.core.Binding("y",
     hydra.core.Term.literal(hydra.core.Literal.string("bar")), None)), hydra.core.Term.variable("y")))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in all let bindings and body",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")), None), hydra.core.Binding("y",
     hydra.core.Term.literal(hydra.core.Literal.string("foo")), None)), hydra.core.Term.literal(hydra.core.Literal.string("foo")))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
     hydra.core.Term.literal(hydra.core.Literal.string("bar")), None), hydra.core.Binding("y",
     hydra.core.Term.literal(hydra.core.Literal.string("bar")), None)), hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in set", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.set(scala.collection.immutable.Set(hydra.core.Term.literal(hydra.core.Literal.string("baz")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")))))), hydra.show.core.term(hydra.core.Term.set(scala.collection.immutable.Set(hydra.core.Term.literal(hydra.core.Literal.string("bar")),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("baz"))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in type lambda body", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.typeLambda(hydra.core.TypeLambda("a",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.string("foo")))))), hydra.show.core.term(hydra.core.Term.typeLambda(hydra.core.TypeLambda("a",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in type application body", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.core.Term.literal(hydra.core.Literal.string("foo")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.string))))), hydra.show.core.term(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.core.Term.literal(hydra.core.Literal.string("bar")),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Type.literal(hydra.core.LiteralType.string)))))), None, Seq()), hydra.testing.TestCaseWithMetadata("string in nested type lambdas",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.typeLambda(hydra.core.TypeLambda("a",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.typeLambda(hydra.core.TypeLambda("b", hydra.core.Term.literal(hydra.core.Literal.string("foo")))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.typeLambda(hydra.core.TypeLambda("a", hydra.core.Term.typeLambda(hydra.core.TypeLambda("b",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))))))))), None, Seq()),
     hydra.testing.TestCaseWithMetadata("string in case branch within let binding",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("handler",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.cases(hydra.core.CaseStatement("Result", None, Seq(hydra.core.Field("ok",
     hydra.core.Term.literal(hydra.core.Literal.string("foo"))), hydra.core.Field("err",
     hydra.core.Term.literal(hydra.core.Literal.string("baz")))))), None)), hydra.core.Term.variable("handler"))))),
     hydra.show.core.term(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("handler",
     hydra.core.Term.cases(hydra.core.CaseStatement("Result", None, Seq(hydra.core.Field("ok",
     hydra.core.Term.literal(hydra.core.Literal.string("bar"))), hydra.core.Field("err",
     hydra.core.Term.literal(hydra.core.Literal.string("baz")))))), None)), hydra.core.Term.variable("handler")))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("string in annotated wrapped record field",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.rewriting.rewriteTerm((recurse: (hydra.core.Term => hydra.core.Term)) =>
  (term: hydra.core.Term) =>
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Term](term)(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))(recurse(term)))(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.wrap(hydra.core.WrappedTerm("User",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.record(hydra.core.Record("UserData", Seq(hydra.core.Field("name",
     hydra.core.Term.literal(hydra.core.Literal.string("foo")))))))), Map())))), hydra.show.core.term(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.wrap(hydra.core.WrappedTerm("User",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.record(hydra.core.Record("UserData", Seq(hydra.core.Field("name",
     hydra.core.Term.literal(hydra.core.Literal.string("bar")))))))), Map()))))),
     None, Seq()))), hydra.testing.TestGroup("rewriteAndFoldTermWithPath", None, Seq(),
     Seq(hydra.testing.TestCaseWithMetadata("path tracking through application - sum literals",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.variable("x"))), hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through nested applications",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.lambda(hydra.core.Lambda("x",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     None, hydra.core.Term.lambda(hydra.core.Lambda("y", None, hydra.core.Term.list(Seq(hydra.core.Term.variable("x"),
     hydra.core.Term.variable("y"))))))), hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))))))),
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(3)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through let bindings",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("x",
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(10))),
     None)), hydra.core.Term.list(Seq(hydra.core.Term.variable("x"), hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(32)))))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through record fields",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.record(hydra.core.Record("Point", Seq(hydra.core.Field("x",
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(10)))),
     hydra.core.Field("y", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(20)))))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(30)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through case branches",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.cases(hydra.core.CaseStatement("Result", None,
     Seq(hydra.core.Field("ok", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))),
     hydra.core.Field("err", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(3)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through pair",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(5))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(7)))))))))),
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(12)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through optional",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.maybe(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))))))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through wrapped term",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.wrap(hydra.core.WrappedTerm("Age", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(25)))))))))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(25)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through type lambda",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.typeLambda(hydra.core.TypeLambda("a", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(100)))))))))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(100)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through type application",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(50))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type.literal(hydra.core.LiteralType.string)))))))), hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(50)))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     None, Seq()), hydra.testing.TestCaseWithMetadata("path tracking through set elements",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.set(scala.collection.immutable.Set(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(3)))))))))),
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(6)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("deep nesting - application in lambda in let",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Int) =>
  (term: hydra.core.Term) =>
  hydra.lib.math.add(acc)(term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_intVal) => v_Literal_integer_intVal match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_n) => v_IntegerValue_int32_n
      case _ => 0
    case _ => 0
  case _ => 0))(0)(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("f",
     hydra.core.Term.lambda(hydra.core.Lambda("x", None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("x"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(5))))))),
     None)), hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(10)))))))))),
     hydra.show.core.term(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(15)))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("collect list lengths in nested structure",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((n: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(n))))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Seq[Int]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[Int](Seq(acc, term match
  case hydra.core.Term.list(v_Term_list_elems) => Seq(hydra.lib.lists.length[hydra.core.Term](v_Term_list_elems))
  case _ => Seq())))(Seq())(hydra.core.Term.list(Seq(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))))),
     hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(3))))))))))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))))),
     None, Seq()), hydra.testing.TestCaseWithMetadata("collect list lengths in let body",
     hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.show.core.term(hydra.core.Term.list(hydra.lib.lists.map[Int,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((n: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(n))))(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((acc: Seq[Int]) =>
  (term: hydra.core.Term) =>
  hydra.lib.lists.concat[Int](Seq(acc, term match
  case hydra.core.Term.list(v_Term_list_elems) => Seq(hydra.lib.lists.length[hydra.core.Term](v_Term_list_elems))
  case _ => Seq())))(Seq())(hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("xs",
     hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))))),
     None)), hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(3))))))))))),
     hydra.show.core.term(hydra.core.Term.list(Seq(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))))),
     None, Seq())))), Seq())
