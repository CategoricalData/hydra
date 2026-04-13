// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class FuncType implements Serializable, Comparable<FuncType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.FuncType");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.python.syntax.TypeExpression> type;

  public final hydra.python.syntax.Expression body;

  public FuncType (java.util.List<hydra.python.syntax.TypeExpression> type, hydra.python.syntax.Expression body) {
    this.type = type;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FuncType)) {
      return false;
    }
    FuncType o = (FuncType) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FuncType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public FuncType withType(java.util.List<hydra.python.syntax.TypeExpression> type) {
    return new FuncType(type, body);
  }

  public FuncType withBody(hydra.python.syntax.Expression body) {
    return new FuncType(type, body);
  }
}
