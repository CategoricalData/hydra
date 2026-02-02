// Note: this is an automatically generated file. Do not edit.

package hydra.encode.variants;

/**
 * Term encoders for hydra.variants
 */
public interface Variants {
  static hydra.core.Term eliminationVariant(hydra.variants.EliminationVariant v1) {
    return ((v1)).accept(new hydra.variants.EliminationVariant.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.variants.EliminationVariant.Record y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.EliminationVariant"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.EliminationVariant.Union y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.EliminationVariant"), new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.EliminationVariant.Wrap y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.EliminationVariant"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term functionVariant(hydra.variants.FunctionVariant v1) {
    return ((v1)).accept(new hydra.variants.FunctionVariant.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.variants.FunctionVariant.Elimination y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.FunctionVariant"), new hydra.core.Field(new hydra.core.Name("elimination"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.FunctionVariant.Lambda y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.FunctionVariant"), new hydra.core.Field(new hydra.core.Name("lambda"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.FunctionVariant.Primitive y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.FunctionVariant"), new hydra.core.Field(new hydra.core.Name("primitive"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term literalVariant(hydra.variants.LiteralVariant v1) {
    return ((v1)).accept(new hydra.variants.LiteralVariant.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.variants.LiteralVariant.Binary y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.LiteralVariant.Boolean_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.LiteralVariant.Float_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.LiteralVariant.Integer_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.LiteralVariant.String_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term termVariant(hydra.variants.TermVariant v1) {
    return ((v1)).accept(new hydra.variants.TermVariant.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Annotated y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("annotated"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Application y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("application"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Either y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("either"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Function y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Let y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("let"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.List y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Literal y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Map y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("map"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Maybe y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("maybe"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Pair y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("pair"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Record y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Set y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.TypeApplication y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("typeApplication"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.TypeLambda y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("typeLambda"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Union y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Unit y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Variable y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("variable"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TermVariant.Wrap y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term typeVariant(hydra.variants.TypeVariant v1) {
    return ((v1)).accept(new hydra.variants.TypeVariant.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Annotated y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("annotated"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Application y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("application"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Either y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("either"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Forall y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("forall"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Function y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.List y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Literal y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Map y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("map"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Maybe y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("maybe"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Pair y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("pair"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Record y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Set y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Union y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Unit y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Variable y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("variable"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.variants.TypeVariant.Wrap y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Unit(true))));
      }
    });
  }
}
