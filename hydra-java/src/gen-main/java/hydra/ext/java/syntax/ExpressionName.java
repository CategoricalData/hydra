// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ExpressionName implements Serializable, Comparable<ExpressionName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ExpressionName");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName> qualifier;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public ExpressionName (hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName> qualifier, hydra.ext.java.syntax.Identifier identifier) {
    this.qualifier = qualifier;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExpressionName)) {
      return false;
    }
    ExpressionName o = (ExpressionName) other;
    return java.util.Objects.equals(
      this.qualifier,
      o.qualifier) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualifier) + 3 * java.util.Objects.hashCode(identifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExpressionName other) {
    int cmp = 0;
    cmp = Integer.compare(
      qualifier.hashCode(),
      other.qualifier.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) identifier).compareTo(other.identifier);
  }
  
  public ExpressionName withQualifier(hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName> qualifier) {
    return new ExpressionName(qualifier, identifier);
  }
  
  public ExpressionName withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new ExpressionName(qualifier, identifier);
  }
}
