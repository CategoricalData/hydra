// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz;

/**
 * Functions for converting Hydra terms to Graphviz DOT graphs
 */
public interface Coder {
  static hydra.graphviz.dot.EqualityPair labelAttr(String lab) {
    return new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("label"), new hydra.graphviz.dot.Id(lab));
  }

  static hydra.graphviz.dot.AttrList labelAttrs(String style, String lab) {
    hydra.util.Lazy<java.util.List<hydra.graphviz.dot.EqualityPair>> styleAttrs = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        style,
        hydra.graphviz.Coder.nodeStyleSimple()),
      () -> (java.util.List<hydra.graphviz.dot.EqualityPair>) (java.util.Collections.<hydra.graphviz.dot.EqualityPair>emptyList()),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          style,
          hydra.graphviz.Coder.nodeStyleElement()),
        () -> java.util.Arrays.asList(
          new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("style"), new hydra.graphviz.dot.Id("filled")),
          new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("fillcolor"), new hydra.graphviz.dot.Id("lightyellow"))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            style,
            hydra.graphviz.Coder.nodeStyleVariable()),
          () -> java.util.Arrays.asList(
            new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("style"), new hydra.graphviz.dot.Id("filled")),
            new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("fillcolor"), new hydra.graphviz.dot.Id("lightcyan"))),
          () -> java.util.Arrays.asList(
            new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("style"), new hydra.graphviz.dot.Id("filled")),
            new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("fillcolor"), new hydra.graphviz.dot.Id("linen")))))));
    return new hydra.graphviz.dot.AttrList(java.util.Arrays.asList(hydra.lib.lists.Concat2.apply(
      java.util.Arrays.asList(hydra.graphviz.Coder.labelAttr(lab)),
      styleAttrs.get())));
  }

  static String nodeStyleElement() {
    return "element";
  }

  static String nodeStylePrimitive() {
    return "primitive";
  }

  static String nodeStyleSimple() {
    return "simple";
  }

  static String nodeStyleVariable() {
    return "variable";
  }

  static java.util.Map<hydra.packaging.Namespace, String> standardNamespaces() {
    return new java.util.TreeMap(java.util.Map.ofEntries(
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.chars"),
        "chars"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.eithers"),
        "eithers"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.equality"),
        "equality"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.lists"),
        "lists"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.literals"),
        "literals"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.logic"),
        "logic"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.maps"),
        "maps"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.math"),
        "math"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.maybes"),
        "maybes"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.pairs"),
        "pairs"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.regex"),
        "regex"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.sets"),
        "sets"),
      java.util.Map.entry(
        new hydra.packaging.Namespace("hydra.lib.strings"),
        "strings")));
  }

  static hydra.util.Pair<String, String> termLabel(Boolean compact, java.util.Map<hydra.packaging.Namespace, String> namespaces, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<String, String> otherwise(hydra.core.Term instance) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          "?");
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Annotated ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          "@{}");
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Application ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.logic.IfElse.lazy(
            compact,
            () -> "$",
            () -> "apply"));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Lambda ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.logic.IfElse.lazy(
            compact,
            () -> "\u03BB",
            () -> "lambda"));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Project proj) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.Names.compactName(
              namespaces,
              (proj).value.typeName),
            "}.",
            (proj).value.field.value)));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Cases cs) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "cases_{",
            hydra.Names.compactName(
              namespaces,
              (cs).value.typeName),
            "}")));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Unwrap name) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "unwrap_{",
            hydra.Names.compactName(
              namespaces,
              (name).value),
            "}")));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Let ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          "let");
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.List ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.logic.IfElse.lazy(
            compact,
            () -> "[]",
            () -> "list"));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Literal l) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          (l).value.accept(new hydra.core.Literal.PartialVisitor<>() {
            @Override
            public String otherwise(hydra.core.Literal instance) {
              return "?";
            }

            @Override
            public String visit(hydra.core.Literal.Binary s) {
              return hydra.lib.literals.BinaryToString.apply((s).value);
            }

            @Override
            public String visit(hydra.core.Literal.Boolean_ b) {
              return hydra.lib.literals.ShowBoolean.apply((b).value);
            }

            @Override
            public String visit(hydra.core.Literal.Integer_ i) {
              return (i).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                @Override
                public String otherwise(hydra.core.IntegerValue instance) {
                  return "?";
                }

                @Override
                public String visit(hydra.core.IntegerValue.Bigint v) {
                  return hydra.lib.literals.ShowBigint.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Int8 v) {
                  return hydra.lib.literals.ShowInt8.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Int16 v) {
                  return hydra.lib.literals.ShowInt16.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Int32 v) {
                  return hydra.lib.literals.ShowInt32.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Int64 v) {
                  return hydra.lib.literals.ShowInt64.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Uint8 v) {
                  return hydra.lib.literals.ShowUint8.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Uint16 v) {
                  return hydra.lib.literals.ShowUint16.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Uint32 v) {
                  return hydra.lib.literals.ShowUint32.apply((v).value);
                }

                @Override
                public String visit(hydra.core.IntegerValue.Uint64 v) {
                  return hydra.lib.literals.ShowUint64.apply((v).value);
                }
              });
            }

            @Override
            public String visit(hydra.core.Literal.Float_ f) {
              return (f).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
                @Override
                public String otherwise(hydra.core.FloatValue instance) {
                  return "?";
                }

                @Override
                public String visit(hydra.core.FloatValue.Bigfloat v) {
                  return hydra.lib.literals.ShowBigfloat.apply((v).value);
                }

                @Override
                public String visit(hydra.core.FloatValue.Float32 v) {
                  return hydra.lib.literals.ShowFloat32.apply((v).value);
                }

                @Override
                public String visit(hydra.core.FloatValue.Float64 v) {
                  return hydra.lib.literals.ShowFloat64.apply((v).value);
                }
              });
            }

            @Override
            public String visit(hydra.core.Literal.String_ s) {
              return (s).value;
            }
          }));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Map ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.logic.IfElse.lazy(
            compact,
            () -> "<,>",
            () -> "map"));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Maybe ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.logic.IfElse.lazy(
            compact,
            () -> "opt",
            () -> "optional"));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Record rec) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.strings.Cat2.apply(
            "\u2227",
            hydra.Names.compactName(
              namespaces,
              (rec).value.typeName)));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.TypeLambda ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          "tyabs");
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.TypeApplication ignored) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          "tyapp");
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Inject inj) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.strings.Cat2.apply(
            "\u22BB",
            hydra.Names.compactName(
              namespaces,
              (inj).value.typeName)));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Variable name) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.Names.compactName(
            namespaces,
            (name).value));
      }

      @Override
      public hydra.util.Pair<String, String> visit(hydra.core.Term.Wrap wt) {
        return hydra.graphviz.Coder.termLabel_simpleLabel(
          hydra.graphviz.Coder.nodeStyleSimple(),
          hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "(",
            hydra.Names.compactName(
              namespaces,
              (wt).value.typeName),
            ")")));
      }
    });
  }

  static <T0> hydra.util.Pair<T0, String> termLabel_simpleLabel(String hydra_graphviz_coder_nodeStyleSimple, T0 lab) {
    return (hydra.util.Pair<T0, String>) ((hydra.util.Pair<T0, String>) (new hydra.util.Pair<T0, String>(lab, hydra_graphviz_coder_nodeStyleSimple)));
  }

  static hydra.graphviz.dot.Graph termToDotGraph(hydra.core.Term term) {
    return new hydra.graphviz.dot.Graph(false, true, (hydra.util.Maybe<hydra.graphviz.dot.Id>) (hydra.util.Maybe.<hydra.graphviz.dot.Id>nothing()), hydra.graphviz.Coder.termToDotStmts(
      hydra.graphviz.Coder.standardNamespaces(),
      term));
  }

  static java.util.List<hydra.graphviz.dot.Stmt> termToDotStmts(java.util.Map<hydra.packaging.Namespace, String> namespaces, hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<String, String>>, java.util.function.Function<Boolean, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Maybe<hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>>>>>> encode = new java.util.concurrent.atomic.AtomicReference<>();
    encode.set((java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<String, String>>, java.util.function.Function<Boolean, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Maybe<hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>>>>>) (mlabstyle -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Maybe<hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>>>>) (isElement -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Maybe<hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>>>) (ids -> (java.util.function.Function<hydra.util.Maybe<hydra.graphviz.dot.Id>, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>>) (mparent -> (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>) (stmtsVisited -> (java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>) (accessorTerm -> {
      hydra.util.Lazy<hydra.paths.SubtermStep> accessor = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accessorTerm));
      hydra.util.Lazy<hydra.core.Term> currentTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accessorTerm));
      java.util.function.Function<java.util.Set<String>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<String, String>>> labelOf = (java.util.function.Function<java.util.Set<String>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<String, String>>>) (vis -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<String, String>>) (t -> {
        hydra.util.Pair<String, String> tls = hydra.graphviz.Coder.termLabel(
          true,
          namespaces,
          t);
        hydra.util.Lazy<String> l = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tls));
        hydra.util.Lazy<String> s = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tls));
        return (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(hydra.Names.uniqueLabel(
          vis,
          l.get()), s.get())));
      }));
      hydra.util.Lazy<java.util.Set<String>> visited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stmtsVisited));
      hydra.util.Lazy<hydra.util.Pair<String, String>> labstyle = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
        () -> (labelOf).apply(visited.get()).apply(currentTerm.get()),
        (java.util.function.Function<hydra.util.Pair<String, String>, hydra.util.Pair<String, String>>) (ls -> ls),
        mlabstyle));
      hydra.util.Lazy<String> label = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(labstyle.get()));
      hydra.graphviz.dot.Id selfId = new hydra.graphviz.dot.Id(label.get());
      hydra.util.Pair<String, String> termLS = hydra.graphviz.Coder.termLabel(
        true,
        namespaces,
        currentTerm.get());
      hydra.util.Lazy<String> termNodeStyle = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(termLS));
      hydra.util.Lazy<String> nodeStyle = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        isElement,
        () -> hydra.graphviz.Coder.nodeStyleElement(),
        () -> termNodeStyle.get()));
      hydra.util.Lazy<String> rawLabel = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(termLS));
      hydra.graphviz.dot.Stmt nodeStmt = new hydra.graphviz.dot.Stmt.Node(new hydra.graphviz.dot.NodeStmt(hydra.graphviz.Coder.toNodeId(selfId), hydra.util.Maybe.just(hydra.graphviz.Coder.labelAttrs(
        nodeStyle.get(),
        rawLabel.get()))));
      hydra.util.Lazy<String> style = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(labstyle.get()));
      java.util.function.Function<hydra.paths.SubtermStep, java.util.function.Function<String, java.util.function.Function<hydra.graphviz.dot.Id, java.util.function.Function<hydra.graphviz.dot.Id, hydra.graphviz.dot.Stmt>>>> toAccessorEdgeStmt = (java.util.function.Function<hydra.paths.SubtermStep, java.util.function.Function<String, java.util.function.Function<hydra.graphviz.dot.Id, java.util.function.Function<hydra.graphviz.dot.Id, hydra.graphviz.dot.Stmt>>>>) (acc -> (java.util.function.Function<String, java.util.function.Function<hydra.graphviz.dot.Id, java.util.function.Function<hydra.graphviz.dot.Id, hydra.graphviz.dot.Stmt>>>) (sty -> (java.util.function.Function<hydra.graphviz.dot.Id, java.util.function.Function<hydra.graphviz.dot.Id, hydra.graphviz.dot.Stmt>>) (i1 -> (java.util.function.Function<hydra.graphviz.dot.Id, hydra.graphviz.dot.Stmt>) (i2 -> hydra.graphviz.Coder.toEdgeStmt(
        i1,
        i2,
        hydra.lib.maybes.Map.apply(
          (java.util.function.Function<String, hydra.graphviz.dot.AttrList>) (s -> hydra.graphviz.Coder.labelAttrs(
            sty,
            s)),
          hydra.show.Paths.subtermStep(acc)))))));
      hydra.util.Lazy<java.util.List<hydra.graphviz.dot.Stmt>> parentStmt = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
        () -> (java.util.List<hydra.graphviz.dot.Stmt>) (java.util.Collections.<hydra.graphviz.dot.Stmt>emptyList()),
        (java.util.function.Function<hydra.graphviz.dot.Id, java.util.List<hydra.graphviz.dot.Stmt>>) (parent -> java.util.Arrays.asList((toAccessorEdgeStmt).apply(accessor.get()).apply(style.get()).apply(parent).apply(selfId))),
        mparent));
      hydra.util.Lazy<java.util.List<hydra.graphviz.dot.Stmt>> stmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stmtsVisited));
      hydra.util.Lazy<java.util.List<hydra.graphviz.dot.Stmt>> selfStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
        stmts.get(),
        java.util.Arrays.asList(nodeStmt),
        parentStmt.get())));
      hydra.util.Lazy<java.util.Set<String>> selfVisited = new hydra.util.Lazy<>(() -> hydra.lib.sets.Insert.apply(
        label.get(),
        visited.get()));
      hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>) (v1 -> (java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>) (v2 -> encode.get().apply((hydra.util.Maybe<hydra.util.Pair<String, String>>) (hydra.util.Maybe.<hydra.util.Pair<String, String>>nothing())).apply(false).apply(ids).apply(hydra.util.Maybe.just(selfId)).apply(v1).apply(v2))),
        (hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) ((hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) (new hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>(selfStmts.get(), selfVisited.get()))),
        hydra.Rewriting.subtermsWithSteps(currentTerm.get())));
      java.util.function.Function<String, hydra.graphviz.dot.AttrList> edgeAttrs = (java.util.function.Function<String, hydra.graphviz.dot.AttrList>) (lab -> new hydra.graphviz.dot.AttrList(java.util.Arrays.asList(java.util.Arrays.asList(new hydra.graphviz.dot.EqualityPair(new hydra.graphviz.dot.Id("label"), new hydra.graphviz.dot.Id(lab))))));
      return currentTerm.get().accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>> otherwise(hydra.core.Term instance) {
          return dflt.get();
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>> visit(hydra.core.Term.Lambda lam) {
          hydra.core.Term body = (lam).value.body;
          hydra.core.Name v = (lam).value.parameter;
          String vstr = (v).value;
          String varLabel = hydra.Names.uniqueLabel(
            selfVisited.get(),
            vstr);
          hydra.graphviz.dot.Id varId = new hydra.graphviz.dot.Id(varLabel);
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>> ids1 = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
            v,
            varId,
            ids));
          hydra.graphviz.dot.Stmt varEdgeStmt = new hydra.graphviz.dot.Stmt.Edge(new hydra.graphviz.dot.EdgeStmt(hydra.graphviz.Coder.toNodeOrSubgraph(selfId), java.util.Arrays.asList(hydra.graphviz.Coder.toNodeOrSubgraph(varId)), hydra.util.Maybe.just((edgeAttrs).apply("var"))));
          hydra.graphviz.dot.Stmt varNodeStmt = new hydra.graphviz.dot.Stmt.Node(new hydra.graphviz.dot.NodeStmt(hydra.graphviz.Coder.toNodeId(varId), hydra.util.Maybe.just(hydra.graphviz.Coder.labelAttrs(
            hydra.graphviz.Coder.nodeStyleVariable(),
            vstr))));
          hydra.util.Lazy<java.util.Set<String>> visited1 = new hydra.util.Lazy<>(() -> hydra.lib.sets.Insert.apply(
            varLabel,
            selfVisited.get()));
          return encode.get().apply((hydra.util.Maybe<hydra.util.Pair<String, String>>) (hydra.util.Maybe.<hydra.util.Pair<String, String>>nothing())).apply(false).apply(ids1.get()).apply(hydra.util.Maybe.just(selfId)).apply((hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) ((hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) (new hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
            selfStmts.get(),
            java.util.Arrays.asList(
              varNodeStmt,
              varEdgeStmt))), visited1.get())))).apply((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.LambdaBody(), body))));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>> visit(hydra.core.Term.Let letExpr) {
          java.util.function.Function<hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>>> addBindingIds = (java.util.function.Function<hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>>>) (idsVis -> (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>>) (binding -> {
            hydra.core.Term bterm = (binding).term;
            hydra.util.Lazy<java.util.Set<String>> curVis = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(idsVis));
            hydra.util.Pair<String, String> bls = (labelOf).apply(curVis.get()).apply(bterm);
            hydra.util.Lazy<String> blab = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bls));
            hydra.core.Name bname = (binding).name;
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>> curIds = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(idsVis));
            return (hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>(hydra.lib.maps.Insert.apply(
              bname,
              new hydra.graphviz.dot.Id(blab.get()),
              curIds.get()), hydra.lib.sets.Insert.apply(
              blab.get(),
              curVis.get()))));
          }));
          java.util.List<hydra.core.Binding> bindings = (letExpr).value.bindings;
          hydra.util.Lazy<hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>> idsVis1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            addBindingIds,
            (hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>, java.util.Set<String>>(ids, visited.get()))),
            bindings));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>> ids1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(idsVis1.get()));
          java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>> addBindingTerm = (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>>) (stVis -> (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>) (binding -> {
            hydra.core.Name bname = (binding).name;
            hydra.util.Lazy<String> blab = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
              () -> new hydra.graphviz.dot.Id("?"),
              hydra.lib.maps.Lookup.apply(
                bname,
                ids1.get())).value);
            hydra.core.Term bterm = (binding).term;
            return encode.get().apply(hydra.util.Maybe.just((hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(blab.get(), hydra.graphviz.Coder.nodeStyleElement()))))).apply(true).apply(ids1.get()).apply(hydra.util.Maybe.just(selfId)).apply(stVis).apply((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.LetBinding(bname), bterm))));
          }));
          hydra.core.Term env = (letExpr).value.body;
          hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>> stmts1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            addBindingTerm,
            (hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) ((hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) (new hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>(selfStmts.get(), selfVisited.get()))),
            bindings));
          return encode.get().apply((hydra.util.Maybe<hydra.util.Pair<String, String>>) (hydra.util.Maybe.<hydra.util.Pair<String, String>>nothing())).apply(false).apply(ids1.get()).apply(hydra.util.Maybe.just(selfId)).apply(stmts1.get()).apply((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.LetBody(), env))));
        }

        @Override
        public hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>> visit(hydra.core.Term.Variable name) {
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> dflt.get(),
            (java.util.function.Function<hydra.graphviz.dot.Id, hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>>) (i -> (hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) ((hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) (new hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>(hydra.lib.lists.Concat2.apply(
              stmts.get(),
              java.util.Arrays.asList((toAccessorEdgeStmt).apply(accessor.get()).apply(style.get()).apply(hydra.lib.maybes.FromMaybe.applyLazy(
                () -> selfId,
                mparent)).apply(i))), visited.get())))),
            hydra.lib.maps.Lookup.apply(
              (name).value,
              ids));
        }
      });
    })))))));
    return hydra.lib.pairs.First.apply(encode.get().apply((hydra.util.Maybe<hydra.util.Pair<String, String>>) (hydra.util.Maybe.<hydra.util.Pair<String, String>>nothing())).apply(false).apply((java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>) ((java.util.Map<hydra.core.Name, hydra.graphviz.dot.Id>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graphviz.dot.Id>apply()))).apply((hydra.util.Maybe<hydra.graphviz.dot.Id>) (hydra.util.Maybe.<hydra.graphviz.dot.Id>nothing())).apply((hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) ((hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>) (new hydra.util.Pair<java.util.List<hydra.graphviz.dot.Stmt>, java.util.Set<String>>((java.util.List<hydra.graphviz.dot.Stmt>) (java.util.Collections.<hydra.graphviz.dot.Stmt>emptyList()), (java.util.Set<String>) (hydra.lib.sets.Empty.<String>apply()))))).apply((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.AnnotatedBody(), term)))));
  }

  static hydra.graphviz.dot.Graph termToSubtermDotGraph(hydra.core.Term term) {
    return new hydra.graphviz.dot.Graph(false, true, (hydra.util.Maybe<hydra.graphviz.dot.Id>) (hydra.util.Maybe.<hydra.graphviz.dot.Id>nothing()), hydra.graphviz.Coder.termToSubtermDotStmts(
      hydra.graphviz.Coder.standardNamespaces(),
      term));
  }

  static java.util.List<hydra.graphviz.dot.Stmt> termToSubtermDotStmts(java.util.Map<hydra.packaging.Namespace, String> namespaces, hydra.core.Term term) {
    hydra.paths.SubtermGraph accessorGraph = hydra.show.Paths.termToSubtermGraph(
      namespaces,
      term);
    java.util.function.Function<hydra.paths.SubtermEdge, hydra.graphviz.dot.Stmt> edgeStmt = (java.util.function.Function<hydra.paths.SubtermEdge, hydra.graphviz.dot.Stmt>) (edge -> {
      String lab1 = (edge).source.id;
      String lab2 = (edge).target.id;
      java.util.List<hydra.paths.SubtermStep> pathAccessors = (edge).path.value;
      hydra.util.Lazy<String> showPath = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
        "/",
        hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          hydra.show.Paths::subtermStep,
          pathAccessors))));
      return hydra.graphviz.Coder.toEdgeStmt(
        new hydra.graphviz.dot.Id(lab1),
        new hydra.graphviz.dot.Id(lab2),
        hydra.util.Maybe.just(new hydra.graphviz.dot.AttrList(java.util.Arrays.asList(java.util.Arrays.asList(hydra.graphviz.Coder.labelAttr(showPath.get()))))));
    });
    java.util.List<hydra.paths.SubtermEdge> edges = (accessorGraph).edges;
    java.util.function.Function<hydra.paths.SubtermNode, hydra.graphviz.dot.Stmt> nodeStmt = (java.util.function.Function<hydra.paths.SubtermNode, hydra.graphviz.dot.Stmt>) (node -> new hydra.graphviz.dot.Stmt.Node(new hydra.graphviz.dot.NodeStmt(hydra.graphviz.Coder.toNodeId(new hydra.graphviz.dot.Id((node).id)), hydra.util.Maybe.just(new hydra.graphviz.dot.AttrList(java.util.Arrays.asList(java.util.Arrays.asList(hydra.graphviz.Coder.labelAttr((node).label))))))));
    java.util.List<hydra.paths.SubtermNode> nodes = (accessorGraph).nodes;
    return hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        nodeStmt,
        nodes),
      hydra.lib.lists.Map.apply(
        edgeStmt,
        edges));
  }

  static hydra.graphviz.dot.Stmt toEdgeStmt(hydra.graphviz.dot.Id i1, hydra.graphviz.dot.Id i2, hydra.util.Maybe<hydra.graphviz.dot.AttrList> attrs) {
    return new hydra.graphviz.dot.Stmt.Edge(new hydra.graphviz.dot.EdgeStmt(hydra.graphviz.Coder.toNodeOrSubgraph(i1), java.util.Arrays.asList(hydra.graphviz.Coder.toNodeOrSubgraph(i2)), attrs));
  }

  static hydra.graphviz.dot.NodeId toNodeId(hydra.graphviz.dot.Id i) {
    return new hydra.graphviz.dot.NodeId(i, (hydra.util.Maybe<hydra.graphviz.dot.Port>) (hydra.util.Maybe.<hydra.graphviz.dot.Port>nothing()));
  }

  static hydra.graphviz.dot.NodeOrSubgraph toNodeOrSubgraph(hydra.graphviz.dot.Id i) {
    return new hydra.graphviz.dot.NodeOrSubgraph.Node(hydra.graphviz.Coder.toNodeId(i));
  }
}
