// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ExpressionName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ExpressionName");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.util.Opt<hydra.langs.java.syntax.AmbiguousName> qualifier;
  
  public final hydra.langs.java.syntax.Identifier identifier;
  
  public ExpressionName (hydra.util.Opt<hydra.langs.java.syntax.AmbiguousName> qualifier, hydra.langs.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((qualifier));
    java.util.Objects.requireNonNull((identifier));
    this.qualifier = qualifier;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExpressionName)) {
      return false;
    }
    ExpressionName o = (ExpressionName) (other);
    return qualifier.equals(o.qualifier) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifier.hashCode() + 3 * identifier.hashCode();
  }
  
  public ExpressionName withQualifier(hydra.util.Opt<hydra.langs.java.syntax.AmbiguousName> qualifier) {
    java.util.Objects.requireNonNull((qualifier));
    return new ExpressionName(qualifier, identifier);
  }
  
  public ExpressionName withIdentifier(hydra.langs.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ExpressionName(qualifier, identifier);
  }
}