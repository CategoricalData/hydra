// Note: this is an automatically generated file. Do not edit.

package hydra.rdf;

/**
 * Serialization functions for converting RDF graphs to N-Triples format expressions
 */
public interface Serde {
  static String escapeIriChar(Integer c) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        hydra.lib.equality.Gte.apply(
          c,
          128),
        hydra.lib.logic.Or.apply(
          hydra.lib.equality.Lte.apply(
            c,
            32),
          hydra.lib.logic.Or.apply(
            hydra.lib.equality.Equal.apply(
              c,
              60),
            hydra.lib.logic.Or.apply(
              hydra.lib.equality.Equal.apply(
                c,
                62),
              hydra.lib.logic.Or.apply(
                hydra.lib.equality.Equal.apply(
                  c,
                  34),
                hydra.lib.logic.Or.apply(
                  hydra.lib.equality.Equal.apply(
                    c,
                    123),
                  hydra.lib.logic.Or.apply(
                    hydra.lib.equality.Equal.apply(
                      c,
                      125),
                    hydra.lib.logic.Or.apply(
                      hydra.lib.equality.Equal.apply(
                        c,
                        124),
                      hydra.lib.logic.Or.apply(
                        hydra.lib.equality.Equal.apply(
                          c,
                          94),
                        hydra.lib.logic.Or.apply(
                          hydra.lib.equality.Equal.apply(
                            c,
                            96),
                          hydra.lib.equality.Equal.apply(
                            c,
                            92))))))))))),
      () -> "?",
      () -> hydra.lib.strings.FromList.apply(java.util.Arrays.asList(c)));
  }

  static String escapeIriStr(String s) {
    return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.rdf.Serde::escapeIriChar,
      hydra.lib.strings.ToList.apply(s)));
  }

  static String escapeLiteralChar(Integer c) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gte.apply(
        c,
        128),
      () -> "?",
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          c,
          34),
        () -> "\\\"",
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            c,
            92),
          () -> "\\\\",
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              c,
              10),
            () -> "\\n",
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                c,
                13),
              () -> "\\r",
              () -> hydra.lib.strings.FromList.apply(java.util.Arrays.asList(c)))))));
  }

  static String escapeLiteralString(String s) {
    return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.rdf.Serde::escapeLiteralChar,
      hydra.lib.strings.ToList.apply(s)));
  }

  static String rdfGraphToNtriples(hydra.rdf.syntax.Graph g) {
    return hydra.Serialization.printExpr(hydra.rdf.Serde.writeGraph(g));
  }

  static hydra.ast.Expr writeBlankNode(hydra.rdf.syntax.BlankNode bnode) {
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.Serialization.cst("_:"),
      hydra.Serialization.cst((bnode).value)));
  }

  static hydra.ast.Expr writeGraph(hydra.rdf.syntax.Graph g) {
    return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      hydra.rdf.Serde::writeTriple,
      hydra.lib.sets.ToList.apply((g).value)));
  }

  static hydra.ast.Expr writeIri(hydra.rdf.syntax.Iri iri) {
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.Serialization.cst("<"),
      hydra.Serialization.cst(hydra.rdf.Serde.escapeIriStr((iri).value)),
      hydra.Serialization.cst(">")));
  }

  static hydra.ast.Expr writeLanguageTag(hydra.rdf.syntax.LanguageTag lang) {
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.Serialization.cst("@"),
      hydra.Serialization.cst((lang).value)));
  }

  static hydra.ast.Expr writeLiteral(hydra.rdf.syntax.Literal lit) {
    hydra.rdf.syntax.Iri dt = (lit).datatypeIri;
    hydra.util.Maybe<hydra.rdf.syntax.LanguageTag> lang = (lit).languageTag;
    String lex = (lit).lexicalForm;
    hydra.ast.Expr lexExpr = hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "\"",
      hydra.rdf.Serde.escapeLiteralString(lex),
      "\"")));
    hydra.util.Lazy<hydra.ast.Expr> suffix = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.Serialization.cst("^^"),
        hydra.rdf.Serde.writeIri(dt))),
      hydra.rdf.Serde::writeLanguageTag,
      lang));
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      lexExpr,
      suffix.get()));
  }

  static hydra.ast.Expr writeNode(hydra.rdf.syntax.Node n) {
    return (n).accept(new hydra.rdf.syntax.Node.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.rdf.syntax.Node.Iri iri) {
        return hydra.rdf.Serde.writeIri((iri).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.rdf.syntax.Node.Bnode bnode) {
        return hydra.rdf.Serde.writeBlankNode((bnode).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.rdf.syntax.Node.Literal lit) {
        return hydra.rdf.Serde.writeLiteral((lit).value);
      }
    });
  }

  static hydra.ast.Expr writeResource(hydra.rdf.syntax.Resource r) {
    return (r).accept(new hydra.rdf.syntax.Resource.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.rdf.syntax.Resource.Iri iri) {
        return hydra.rdf.Serde.writeIri((iri).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.rdf.syntax.Resource.Bnode bnode) {
        return hydra.rdf.Serde.writeBlankNode((bnode).value);
      }
    });
  }

  static hydra.ast.Expr writeTriple(hydra.rdf.syntax.Triple t) {
    hydra.rdf.syntax.Node obj = (t).object;
    hydra.rdf.syntax.Iri pred = (t).predicate;
    hydra.rdf.syntax.Resource subj = (t).subject;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.rdf.Serde.writeResource(subj),
      hydra.rdf.Serde.writeIri(pred),
      hydra.rdf.Serde.writeNode(obj),
      hydra.Serialization.cst(".")));
  }
}
