// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TypeAlias implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TypeAlias");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMS = new hydra.core.Name("typeParams");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams;
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public TypeAlias (hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams, hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((typeParams));
    java.util.Objects.requireNonNull((expression));
    this.name = name;
    this.typeParams = typeParams;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAlias)) {
      return false;
    }
    TypeAlias o = (TypeAlias) (other);
    return name.equals(o.name) && typeParams.equals(o.typeParams) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typeParams.hashCode() + 5 * expression.hashCode();
  }
  
  public TypeAlias withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new TypeAlias(name, typeParams, expression);
  }
  
  public TypeAlias withTypeParams(java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams) {
    java.util.Objects.requireNonNull((typeParams));
    return new TypeAlias(name, typeParams, expression);
  }
  
  public TypeAlias withExpression(hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new TypeAlias(name, typeParams, expression);
  }
}