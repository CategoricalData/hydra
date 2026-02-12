// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_Expression implements Serializable, Comparable<MethodReference_Expression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_Expression");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.ExpressionName name;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public MethodReference_Expression (hydra.ext.java.syntax.ExpressionName name, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier) {
    this.name = name;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Expression)) {
      return false;
    }
    MethodReference_Expression o = (MethodReference_Expression) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(typeArguments) + 5 * java.util.Objects.hashCode(identifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodReference_Expression other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) identifier).compareTo(other.identifier);
  }
  
  public MethodReference_Expression withName(hydra.ext.java.syntax.ExpressionName name) {
    return new MethodReference_Expression(name, typeArguments, identifier);
  }
  
  public MethodReference_Expression withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new MethodReference_Expression(name, typeArguments, identifier);
  }
  
  public MethodReference_Expression withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodReference_Expression(name, typeArguments, identifier);
  }
}
