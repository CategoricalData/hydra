// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz;

/**
 * Serialization functions for converting Graphviz DOT AST to abstract expressions
 */
public interface Serde {
  static hydra.ast.Expr writeAttrList(hydra.graphviz.dot.AttrList al) {
    return hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.graphviz.dot.EqualityPair>, hydra.ast.Expr>) (alist -> hydra.Serialization.brackets(
        hydra.Serialization.squareBrackets(),
        hydra.Serialization.inlineStyle(),
        hydra.Serialization.commaSep(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.graphviz.Serde::writeEqualityPair,
            alist)))),
      (al).value));
  }

  static hydra.ast.Expr writeAttrStmt(hydra.graphviz.dot.AttrStmt as) {
    hydra.graphviz.dot.AttrList attr = (as).attributes;
    hydra.graphviz.dot.AttrType t = (as).type;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.graphviz.Serde.writeAttrType(t),
      hydra.graphviz.Serde.writeAttrList(attr)));
  }

  static hydra.ast.Expr writeAttrType(hydra.graphviz.dot.AttrType t) {
    return (t).accept(new hydra.graphviz.dot.AttrType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.AttrType.Graph ignored) {
        return hydra.Serialization.cst("graph");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.AttrType.Node ignored) {
        return hydra.Serialization.cst("node");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.AttrType.Edge ignored) {
        return hydra.Serialization.cst("edge");
      }
    });
  }

  static hydra.ast.Expr writeCompassPt(hydra.graphviz.dot.CompassPt p) {
    return (p).accept(new hydra.graphviz.dot.CompassPt.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.N ignored) {
        return hydra.Serialization.cst("n");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.Ne ignored) {
        return hydra.Serialization.cst("ne");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.E ignored) {
        return hydra.Serialization.cst("e");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.Se ignored) {
        return hydra.Serialization.cst("se");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.S ignored) {
        return hydra.Serialization.cst("s");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.Sw ignored) {
        return hydra.Serialization.cst("sw");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.W ignored) {
        return hydra.Serialization.cst("w");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.Nw ignored) {
        return hydra.Serialization.cst("nw");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.C ignored) {
        return hydra.Serialization.cst("c");
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.CompassPt.None ignored) {
        return hydra.Serialization.cst("none");
      }
    });
  }

  static hydra.ast.Expr writeEdgeStmt(Boolean directed, hydra.graphviz.dot.EdgeStmt es) {
    hydra.util.Lazy<String> arrow = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      directed,
      () -> "->",
      () -> "--"));
    hydra.util.Maybe<hydra.graphviz.dot.AttrList> attr = (es).attributes;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> attrParts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.graphviz.dot.AttrList, java.util.List<hydra.ast.Expr>>) (a -> java.util.Arrays.asList(hydra.graphviz.Serde.writeAttrList(a))),
      attr));
    hydra.graphviz.dot.NodeOrSubgraph l = (es).left;
    java.util.List<hydra.graphviz.dot.NodeOrSubgraph> r = (es).right;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> rhsParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.graphviz.dot.NodeOrSubgraph, java.util.List<hydra.ast.Expr>>) (n -> java.util.Arrays.asList(
        hydra.Serialization.cst(arrow.get()),
        hydra.graphviz.Serde.writeNodeOrSubgraph(
          directed,
          n))),
      r)));
    return hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      java.util.Arrays.asList(hydra.graphviz.Serde.writeNodeOrSubgraph(
        directed,
        l)),
      rhsParts.get(),
      attrParts.get())));
  }

  static hydra.ast.Expr writeEqualityPair(hydra.graphviz.dot.EqualityPair eq) {
    hydra.graphviz.dot.Id l = (eq).left;
    hydra.graphviz.dot.Id r = (eq).right;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.graphviz.Serde.writeId(l),
      hydra.Serialization.cst("="),
      hydra.graphviz.Serde.writeId(r)));
  }

  static hydra.ast.Expr writeGraph(hydra.graphviz.dot.Graph g) {
    Boolean directed = (g).directed;
    java.util.List<hydra.graphviz.dot.Stmt> stmts = (g).statements;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.Serialization.brackets(
      hydra.Serialization.curlyBraces(),
      hydra.Serialization.fullBlockStyle(),
      hydra.Serialization.symbolSep(
        ";",
        hydra.Serialization.fullBlockStyle(),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.graphviz.dot.Stmt, hydra.ast.Expr>) (v1 -> hydra.graphviz.Serde.writeStmt(
            directed,
            v1)),
          stmts))));
    hydra.util.Lazy<String> graphKeyword = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      directed,
      () -> "digraph",
      () -> "graph"));
    Boolean strict = (g).strict;
    hydra.util.Lazy<hydra.ast.Expr> graphExpr = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      strict,
      () -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.Serialization.cst("strict"),
        hydra.Serialization.cst(graphKeyword.get()))),
      () -> hydra.Serialization.cst(graphKeyword.get())));
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      graphExpr.get(),
      body.get()));
  }

  static hydra.ast.Expr writeId(hydra.graphviz.dot.Id i) {
    return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "\"",
      (i).value,
      "\"")));
  }

  static hydra.ast.Expr writeNodeId(hydra.graphviz.dot.NodeId nid) {
    hydra.graphviz.dot.Id i = (nid).id;
    hydra.util.Maybe<hydra.graphviz.dot.Port> mp = (nid).port;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Pure.apply(hydra.graphviz.Serde.writeId(i)),
      hydra.lib.maybes.Map.apply(
        hydra.graphviz.Serde::writePort,
        mp))));
  }

  static hydra.ast.Expr writeNodeOrSubgraph(Boolean directed, hydra.graphviz.dot.NodeOrSubgraph ns) {
    return (ns).accept(new hydra.graphviz.dot.NodeOrSubgraph.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.NodeOrSubgraph.Node n) {
        return hydra.graphviz.Serde.writeNodeId((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.NodeOrSubgraph.Subgraph sg) {
        return hydra.graphviz.Serde.writeSubgraph(
          directed,
          (sg).value);
      }
    });
  }

  static hydra.ast.Expr writeNodeStmt(hydra.graphviz.dot.NodeStmt ns) {
    hydra.util.Maybe<hydra.graphviz.dot.AttrList> attr = (ns).attributes;
    hydra.graphviz.dot.NodeId i = (ns).id;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Pure.apply(hydra.graphviz.Serde.writeNodeId(i)),
      hydra.lib.maybes.Map.apply(
        hydra.graphviz.Serde::writeAttrList,
        attr))));
  }

  static hydra.ast.Expr writePort(hydra.graphviz.dot.Port p) {
    hydra.util.Maybe<hydra.graphviz.dot.Id> mi = (p).id;
    hydra.util.Maybe<hydra.graphviz.dot.CompassPt> mp = (p).position;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> pre = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.graphviz.dot.Id, java.util.List<hydra.ast.Expr>>) (i -> java.util.Arrays.asList(
        hydra.Serialization.cst(":"),
        hydra.graphviz.Serde.writeId(i))),
      mi));
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> suf = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.graphviz.dot.CompassPt, java.util.List<hydra.ast.Expr>>) (cp -> java.util.Arrays.asList(
        hydra.Serialization.cst(":"),
        hydra.graphviz.Serde.writeCompassPt(cp))),
      mp));
    return hydra.Serialization.noSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      pre.get(),
      suf.get())));
  }

  static hydra.ast.Expr writeStmt(Boolean directed, hydra.graphviz.dot.Stmt s) {
    return (s).accept(new hydra.graphviz.dot.Stmt.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.Stmt.Node n) {
        return hydra.graphviz.Serde.writeNodeStmt((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.Stmt.Edge e) {
        return hydra.graphviz.Serde.writeEdgeStmt(
          directed,
          (e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.Stmt.Attr a) {
        return hydra.graphviz.Serde.writeAttrStmt((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.Stmt.Equals eq) {
        return hydra.graphviz.Serde.writeEqualityPair((eq).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.graphviz.dot.Stmt.Subgraph sg) {
        return hydra.graphviz.Serde.writeSubgraph(
          directed,
          (sg).value);
      }
    });
  }

  static hydra.ast.Expr writeSubgraph(Boolean directed, hydra.graphviz.dot.Subgraph sg) {
    java.util.List<hydra.graphviz.dot.Stmt> stmts = (sg).statements;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.Serialization.brackets(
      hydra.Serialization.curlyBraces(),
      hydra.Serialization.inlineStyle(),
      hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.graphviz.dot.Stmt, hydra.ast.Expr>) (v1 -> hydra.graphviz.Serde.writeStmt(
          directed,
          v1)),
        stmts))));
    hydra.util.Maybe<hydra.graphviz.dot.SubgraphId> mid = (sg).subgraphId;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        hydra.graphviz.Serde::writeSubgraphId,
        mid),
      hydra.lib.maybes.Pure.apply(body.get()))));
  }

  static hydra.ast.Expr writeSubgraphId(hydra.graphviz.dot.SubgraphId sid) {
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Pure.apply(hydra.Serialization.cst("subgraph")),
      hydra.lib.maybes.Map.apply(
        hydra.graphviz.Serde::writeId,
        (sid).value))));
  }
}
