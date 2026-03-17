// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TypeAlias implements Serializable, Comparable<TypeAlias> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.TypeAlias");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE_PARAMS = new hydra.core.Name("typeParams");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.ext.python.syntax.Name name;

  public final hydra.util.ConsList<hydra.ext.python.syntax.TypeParameter> typeParams;

  public final hydra.ext.python.syntax.Expression expression;

  public TypeAlias (hydra.ext.python.syntax.Name name, hydra.util.ConsList<hydra.ext.python.syntax.TypeParameter> typeParams, hydra.ext.python.syntax.Expression expression) {
    this.name = name;
    this.typeParams = typeParams;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAlias)) {
      return false;
    }
    TypeAlias o = (TypeAlias) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.typeParams,
      o.typeParams) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(typeParams) + 5 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeAlias other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) typeParams).compareTo(other.typeParams);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }

  public TypeAlias withName(hydra.ext.python.syntax.Name name) {
    return new TypeAlias(name, typeParams, expression);
  }

  public TypeAlias withTypeParams(hydra.util.ConsList<hydra.ext.python.syntax.TypeParameter> typeParams) {
    return new TypeAlias(name, typeParams, expression);
  }

  public TypeAlias withExpression(hydra.ext.python.syntax.Expression expression) {
    return new TypeAlias(name, typeParams, expression);
  }
}
