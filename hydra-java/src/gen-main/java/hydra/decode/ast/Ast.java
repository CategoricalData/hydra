// Note: this is an automatically generated file. Do not edit.

package hydra.decode.ast;

/**
 * Term decoders for hydra.ast
 */
public interface Ast {
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity> associativity(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Associativity>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Associativity>left(new hydra.util.DecodingError("expected union of type hydra.ast.Associativity"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>(new hydra.core.Name("none"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Associativity>) (t -> new hydra.ast.Associativity.None((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>(new hydra.core.Name("left"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Associativity>) (t -> new hydra.ast.Associativity.Left((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>(new hydra.core.Name("right"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Associativity>) (t -> new hydra.ast.Associativity.Right((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>(new hydra.core.Name("both"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Associativity>) (t -> new hydra.ast.Associativity.Both((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Associativity>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle> blockStyle(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.BlockStyle>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.BlockStyle>left(new hydra.util.DecodingError("expected record of type hydra.ast.BlockStyle"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "indent",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMaybe(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                      return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                        }
                      });
                    }
                  })),
                  hydra.lexical.Lexical.stripAndDereferenceTermEither(
                    (cx2),
                    (raw2))))),
                (v1),
                (v2)))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>>) (field_indent -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "newlineBeforeContent",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError((err)))))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Term.Literal v) {
                      return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Literal instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected boolean literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Literal.Boolean_ b) {
                          return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>right(((b)).value)));
                        }
                      });
                    }
                  })),
                  hydra.lexical.Lexical.stripAndDereferenceTermEither(
                    (cx2),
                    (raw2))))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<Boolean, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>>) (field_newlineBeforeContent -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "newlineAfterContent",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError((err)))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Term.Literal v) {
                        return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected boolean literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Literal.Boolean_ b) {
                            return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>right(((b)).value)));
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      (cx2),
                      (raw2))))),
                  (fieldMap),
                  (cx)),
                (java.util.function.Function<Boolean, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>>) (field_newlineAfterContent -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.BlockStyle>right(new hydra.ast.BlockStyle((field_indent), (field_newlineBeforeContent), (field_newlineAfterContent)))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr> bracketExpr(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.BracketExpr>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.BracketExpr>left(new hydra.util.DecodingError("expected record of type hydra.ast.BracketExpr"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "brackets",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>>>) (p0 -> p1 -> hydra.decode.ast.Ast.brackets(
                (p0),
                (p1))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.ast.Brackets, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>>) (field_brackets -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "enclosed",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (p0 -> p1 -> hydra.decode.ast.Ast.expr(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.ast.Expr, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>>) (field_enclosed -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "style",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BlockStyle>>>) (p0 -> p1 -> hydra.decode.ast.Ast.blockStyle(
                    (p0),
                    (p1))),
                  (fieldMap),
                  (cx)),
                (java.util.function.Function<hydra.ast.BlockStyle, hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>>) (field_style -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.BracketExpr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.BracketExpr>right(new hydra.ast.BracketExpr((field_brackets), (field_enclosed), (field_style)))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets> brackets(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Brackets>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Brackets>left(new hydra.util.DecodingError("expected record of type hydra.ast.Brackets"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "open",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>>>) (p0 -> p1 -> hydra.decode.ast.Ast.symbol(
                (p0),
                (p1))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.ast.Symbol, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>>) (field_open -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "close",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>>>) (p0 -> p1 -> hydra.decode.ast.Ast.symbol(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.ast.Symbol, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>>) (field_close -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Brackets>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Brackets>right(new hydra.ast.Brackets((field_open), (field_close)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr> expr(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Expr>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Expr>left(new hydra.util.DecodingError("expected union of type hydra.ast.Expr"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>(new hydra.core.Name("const"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.ast.Symbol, hydra.ast.Expr>) (t -> new hydra.ast.Expr.Const((t))),
              hydra.decode.ast.Ast.symbol(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>(new hydra.core.Name("indent"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.ast.IndentedExpression, hydra.ast.Expr>) (t -> new hydra.ast.Expr.Indent((t))),
              hydra.decode.ast.Ast.indentedExpression(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>(new hydra.core.Name("op"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.ast.OpExpr, hydra.ast.Expr>) (t -> new hydra.ast.Expr.Op((t))),
              hydra.decode.ast.Ast.opExpr(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>(new hydra.core.Name("brackets"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.ast.BracketExpr, hydra.ast.Expr>) (t -> new hydra.ast.Expr.Brackets((t))),
              hydra.decode.ast.Ast.bracketExpr(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Expr>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression> indentedExpression(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.IndentedExpression>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.IndentedExpression>left(new hydra.util.DecodingError("expected record of type hydra.ast.IndentedExpression"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "style",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>>) (p0 -> p1 -> hydra.decode.ast.Ast.indentStyle(
                (p0),
                (p1))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.ast.IndentStyle, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>>) (field_style -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "expr",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (p0 -> p1 -> hydra.decode.ast.Ast.expr(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.ast.Expr, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>>) (field_expr -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentedExpression>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.IndentedExpression>right(new hydra.ast.IndentedExpression((field_style), (field_expr)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle> indentStyle(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.IndentStyle>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.IndentStyle>left(new hydra.util.DecodingError("expected union of type hydra.ast.IndentStyle"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>>(new hydra.core.Name("allLines"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.ast.IndentStyle>) (t -> new hydra.ast.IndentStyle.AllLines((t))),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                    return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  (cx),
                  (input)))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>>(new hydra.core.Name("subsequentLines"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.ast.IndentStyle>) (t -> new hydra.ast.IndentStyle.SubsequentLines((t))),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                    return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  (cx),
                  (input))))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.IndentStyle>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>, hydra.util.Either<hydra.util.DecodingError, hydra.ast.IndentStyle>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op> op(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Op>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Op>left(new hydra.util.DecodingError("expected record of type hydra.ast.Op"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "symbol",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>>>) (p0 -> p1 -> hydra.decode.ast.Ast.symbol(
                (p0),
                (p1))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.ast.Symbol, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>>) (field_symbol -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "padding",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>>>) (p0 -> p1 -> hydra.decode.ast.Ast.padding(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.ast.Padding, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>>) (field_padding -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "precedence",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence>>>) (p0 -> p1 -> hydra.decode.ast.Ast.precedence(
                    (p0),
                    (p1))),
                  (fieldMap),
                  (cx)),
                (java.util.function.Function<hydra.ast.Precedence, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>>) (field_precedence -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "associativity",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Associativity>>>) (p0 -> p1 -> hydra.decode.ast.Ast.associativity(
                      (p0),
                      (p1))),
                    (fieldMap),
                    (cx)),
                  (java.util.function.Function<hydra.ast.Associativity, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>>) (field_associativity -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Op>right(new hydra.ast.Op((field_symbol), (field_padding), (field_precedence), (field_associativity)))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr> opExpr(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.OpExpr>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.OpExpr>left(new hydra.util.DecodingError("expected record of type hydra.ast.OpExpr"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "op",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Op>>>) (p0 -> p1 -> hydra.decode.ast.Ast.op(
                (p0),
                (p1))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.ast.Op, hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>>) (field_op -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "lhs",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (p0 -> p1 -> hydra.decode.ast.Ast.expr(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.ast.Expr, hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>>) (field_lhs -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "rhs",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (p0 -> p1 -> hydra.decode.ast.Ast.expr(
                    (p0),
                    (p1))),
                  (fieldMap),
                  (cx)),
                (java.util.function.Function<hydra.ast.Expr, hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>>) (field_rhs -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.OpExpr>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.OpExpr>right(new hydra.ast.OpExpr((field_op), (field_lhs), (field_rhs)))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding> padding(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Padding>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Padding>left(new hydra.util.DecodingError("expected record of type hydra.ast.Padding"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "left",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) (p0 -> p1 -> hydra.decode.ast.Ast.ws(
                (p0),
                (p1))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.ast.Ws, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>>) (field_left -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "right",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) (p0 -> p1 -> hydra.decode.ast.Ast.ws(
                  (p0),
                  (p1))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<hydra.ast.Ws, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>>) (field_right -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Padding>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Padding>right(new hydra.ast.Padding((field_left), (field_right)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence> precedence(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Precedence>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Precedence>left(new hydra.util.DecodingError("expected wrapped type hydra.ast.Precedence"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Precedence> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<Integer, hydra.ast.Precedence>) (b -> new hydra.ast.Precedence((b))),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError((err)))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                }
                
                @Override
                public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                  return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                      return (((v1)).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                          return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right(((i)).value)));
                        }
                      });
                    }
                  });
                }
              })),
              hydra.lexical.Lexical.stripAndDereferenceTermEither(
                (cx),
                (((wrappedTerm)).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol> symbol(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Symbol>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Symbol>left(new hydra.util.DecodingError("expected wrapped type hydra.ast.Symbol"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Symbol> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.ast.Symbol>) (b -> new hydra.ast.Symbol((b))),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                }
                
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                    }
                  });
                }
              })),
              hydra.lexical.Lexical.stripAndDereferenceTermEither(
                (cx),
                (((wrappedTerm)).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws> ws(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Ws>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Ws>left(new hydra.util.DecodingError("expected union of type hydra.ast.Ws"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>(new hydra.core.Name("none"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Ws>) (t -> new hydra.ast.Ws.None((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>(new hydra.core.Name("space"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Ws>) (t -> new hydra.ast.Ws.Space((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>(new hydra.core.Name("break"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Ws>) (t -> new hydra.ast.Ws.Break((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>(new hydra.core.Name("breakAndIndent"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.ast.Ws>) (t -> new hydra.ast.Ws.BreakAndIndent((t))),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                    return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  (cx),
                  (input)))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>>(new hydra.core.Name("doubleBreak"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.ast.Ws>) (t -> new hydra.ast.Ws.DoubleBreak((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>) ((hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>) (hydra.util.Either.<hydra.util.DecodingError, hydra.ast.Ws>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Ws>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
}
