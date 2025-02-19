// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class SpecificCatchClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SpecificCatchClause");
  
  public static final hydra.core.Name FIELD_NAME_SPECIFIER = new hydra.core.Name("specifier");
  
  public static final hydra.core.Name FIELD_NAME_FILTER = new hydra.core.Name("filter");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ExceptionSpecifier> specifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.BooleanExpression> filter;
  
  public final hydra.ext.csharp.syntax.Block body;
  
  public SpecificCatchClause (hydra.util.Opt<hydra.ext.csharp.syntax.ExceptionSpecifier> specifier, hydra.util.Opt<hydra.ext.csharp.syntax.BooleanExpression> filter, hydra.ext.csharp.syntax.Block body) {
    java.util.Objects.requireNonNull((specifier));
    java.util.Objects.requireNonNull((filter));
    java.util.Objects.requireNonNull((body));
    this.specifier = specifier;
    this.filter = filter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SpecificCatchClause)) {
      return false;
    }
    SpecificCatchClause o = (SpecificCatchClause) (other);
    return specifier.equals(o.specifier) && filter.equals(o.filter) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * specifier.hashCode() + 3 * filter.hashCode() + 5 * body.hashCode();
  }
  
  public SpecificCatchClause withSpecifier(hydra.util.Opt<hydra.ext.csharp.syntax.ExceptionSpecifier> specifier) {
    java.util.Objects.requireNonNull((specifier));
    return new SpecificCatchClause(specifier, filter, body);
  }
  
  public SpecificCatchClause withFilter(hydra.util.Opt<hydra.ext.csharp.syntax.BooleanExpression> filter) {
    java.util.Objects.requireNonNull((filter));
    return new SpecificCatchClause(specifier, filter, body);
  }
  
  public SpecificCatchClause withBody(hydra.ext.csharp.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new SpecificCatchClause(specifier, filter, body);
  }
}