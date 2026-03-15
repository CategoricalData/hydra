// Note: this is an automatically generated file. Do not edit.

package hydra.ext.rdf.serde;

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
      () -> hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(c)));
  }
  
  static String escapeIriStr(String s) {
    return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.ext.rdf.serde.Serde::escapeIriChar,
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
              () -> hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(c)))))));
  }
  
  static String escapeLiteralString(String s) {
    return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.ext.rdf.serde.Serde::escapeLiteralChar,
      hydra.lib.strings.ToList.apply(s)));
  }
  
  static String rdfGraphToNtriples(hydra.ext.org.w3.rdf.syntax.Graph g) {
    return hydra.serialization.Serialization.printExpr(hydra.ext.rdf.serde.Serde.writeGraph(g));
  }
  
  static hydra.ast.Expr writeBlankNode(hydra.ext.org.w3.rdf.syntax.BlankNode bnode) {
    return hydra.serialization.Serialization.noSep(hydra.util.ConsList.of(
      hydra.serialization.Serialization.cst("_:"),
      hydra.serialization.Serialization.cst((bnode).value)));
  }
  
  static hydra.ast.Expr writeGraph(hydra.ext.org.w3.rdf.syntax.Graph g) {
    return hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      hydra.ext.rdf.serde.Serde::writeTriple,
      hydra.lib.sets.ToList.apply((g).value)));
  }
  
  static hydra.ast.Expr writeIri(hydra.ext.org.w3.rdf.syntax.Iri iri) {
    return hydra.serialization.Serialization.noSep(hydra.util.ConsList.of(
      hydra.serialization.Serialization.cst("<"),
      hydra.serialization.Serialization.cst(hydra.ext.rdf.serde.Serde.escapeIriStr((iri).value)),
      hydra.serialization.Serialization.cst(">")));
  }
  
  static hydra.ast.Expr writeLanguageTag(hydra.ext.org.w3.rdf.syntax.LanguageTag lang) {
    return hydra.serialization.Serialization.noSep(hydra.util.ConsList.of(
      hydra.serialization.Serialization.cst("@"),
      hydra.serialization.Serialization.cst((lang).value)));
  }
  
  static hydra.ast.Expr writeLiteral(hydra.ext.org.w3.rdf.syntax.Literal lit) {
    hydra.ext.org.w3.rdf.syntax.Iri dt = (lit).datatypeIri;
    hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag> lang = (lit).languageTag;
    String lex = (lit).lexicalForm;
    hydra.ast.Expr lexExpr = hydra.serialization.Serialization.cst(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "\"",
      hydra.ext.rdf.serde.Serde.escapeLiteralString(lex),
      "\"")));
    hydra.util.Lazy<hydra.ast.Expr> suffix = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.serialization.Serialization.noSep(hydra.util.ConsList.of(
        hydra.serialization.Serialization.cst("^^"),
        hydra.ext.rdf.serde.Serde.writeIri(dt))),
      hydra.ext.rdf.serde.Serde::writeLanguageTag,
      lang));
    return hydra.serialization.Serialization.noSep(hydra.util.ConsList.of(
      lexExpr,
      suffix.get()));
  }
  
  static hydra.ast.Expr writeNode(hydra.ext.org.w3.rdf.syntax.Node n) {
    return (n).accept(new hydra.ext.org.w3.rdf.syntax.Node.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.org.w3.rdf.syntax.Node.Iri iri) {
        return hydra.ext.rdf.serde.Serde.writeIri((iri).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.org.w3.rdf.syntax.Node.Bnode bnode) {
        return hydra.ext.rdf.serde.Serde.writeBlankNode((bnode).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.org.w3.rdf.syntax.Node.Literal lit) {
        return hydra.ext.rdf.serde.Serde.writeLiteral((lit).value);
      }
    });
  }
  
  static hydra.ast.Expr writeResource(hydra.ext.org.w3.rdf.syntax.Resource r) {
    return (r).accept(new hydra.ext.org.w3.rdf.syntax.Resource.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.org.w3.rdf.syntax.Resource.Iri iri) {
        return hydra.ext.rdf.serde.Serde.writeIri((iri).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.org.w3.rdf.syntax.Resource.Bnode bnode) {
        return hydra.ext.rdf.serde.Serde.writeBlankNode((bnode).value);
      }
    });
  }
  
  static hydra.ast.Expr writeTriple(hydra.ext.org.w3.rdf.syntax.Triple t) {
    hydra.ext.org.w3.rdf.syntax.Node obj = (t).object;
    hydra.ext.org.w3.rdf.syntax.Iri pred = (t).predicate;
    hydra.ext.org.w3.rdf.syntax.Resource subj = (t).subject;
    return hydra.serialization.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.ext.rdf.serde.Serde.writeResource(subj),
      hydra.ext.rdf.serde.Serde.writeIri(pred),
      hydra.ext.rdf.serde.Serde.writeNode(obj),
      hydra.serialization.Serialization.cst(".")));
  }
}
