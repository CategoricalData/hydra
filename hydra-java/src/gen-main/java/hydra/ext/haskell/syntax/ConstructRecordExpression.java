// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A record constructor expression
 */
public class ConstructRecordExpression implements Serializable, Comparable<ConstructRecordExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.ConstructRecordExpression");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");

  /**
   * The constructor name
   */
  public final hydra.ext.haskell.syntax.Name name;

  /**
   * The field assignments
   */
  public final hydra.util.ConsList<hydra.ext.haskell.syntax.FieldUpdate> fields;

  public ConstructRecordExpression (hydra.ext.haskell.syntax.Name name, hydra.util.ConsList<hydra.ext.haskell.syntax.FieldUpdate> fields) {
    this.name = name;
    this.fields = fields;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructRecordExpression)) {
      return false;
    }
    ConstructRecordExpression o = (ConstructRecordExpression) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(fields);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstructRecordExpression other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) fields).compareTo(other.fields);
  }

  public ConstructRecordExpression withName(hydra.ext.haskell.syntax.Name name) {
    return new ConstructRecordExpression(name, fields);
  }

  public ConstructRecordExpression withFields(hydra.util.ConsList<hydra.ext.haskell.syntax.FieldUpdate> fields) {
    return new ConstructRecordExpression(name, fields);
  }
}
