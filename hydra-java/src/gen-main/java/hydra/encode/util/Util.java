// Note: this is an automatically generated file. Do not edit.

package hydra.encode.util;

/**
 * Term encoders for hydra.util
 */
public interface Util {
  static hydra.core.Term caseConvention(hydra.util.CaseConvention v1) {
    return (v1).accept(new hydra.util.CaseConvention.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.util.CaseConvention.Camel y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("camel"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.util.CaseConvention.Pascal y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("pascal"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.util.CaseConvention.LowerSnake y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("lowerSnake"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.util.CaseConvention.UpperSnake y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("upperSnake"), new hydra.core.Term.Unit())));
      }
    });
  }
  
  static hydra.core.Term comparison(hydra.util.Comparison v1) {
    return (v1).accept(new hydra.util.Comparison.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.util.Comparison.LessThan y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.util.Comparison.EqualTo y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.util.Comparison.GreaterThan y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Unit())));
      }
    });
  }
  
  static hydra.core.Term decodingError(hydra.util.DecodingError x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }
  
  static hydra.core.Term precision(hydra.util.Precision v1) {
    return (v1).accept(new hydra.util.Precision.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.util.Precision.Arbitrary y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Precision"), new hydra.core.Field(new hydra.core.Name("arbitrary"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.util.Precision.Bits y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Precision"), new hydra.core.Field(new hydra.core.Name("bits"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((y).value))))));
      }
    });
  }
}
