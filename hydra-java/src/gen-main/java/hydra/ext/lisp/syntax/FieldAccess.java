// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * Field access on a record/struct. Serializes as (:field record) in Clojure, (struct-field record) in Emacs Lisp and Common Lisp, (record-field record) in Scheme
 */
public class FieldAccess implements Serializable, Comparable<FieldAccess> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.FieldAccess");

  public static final hydra.core.Name RECORD_TYPE = new hydra.core.Name("recordType");

  public static final hydra.core.Name FIELD = new hydra.core.Name("field");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  /**
   * The record type name (used to form accessor name)
   */
  public final hydra.ext.lisp.syntax.Symbol recordType;

  /**
   * The field name
   */
  public final hydra.ext.lisp.syntax.Symbol field;

  /**
   * The expression being accessed
   */
  public final hydra.ext.lisp.syntax.Expression target;

  public FieldAccess (hydra.ext.lisp.syntax.Symbol recordType, hydra.ext.lisp.syntax.Symbol field, hydra.ext.lisp.syntax.Expression target) {
    this.recordType = recordType;
    this.field = field;
    this.target = target;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldAccess)) {
      return false;
    }
    FieldAccess o = (FieldAccess) other;
    return java.util.Objects.equals(
      this.recordType,
      o.recordType) && java.util.Objects.equals(
      this.field,
      o.field) && java.util.Objects.equals(
      this.target,
      o.target);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(recordType) + 3 * java.util.Objects.hashCode(field) + 5 * java.util.Objects.hashCode(target);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldAccess other) {
    int cmp = 0;
    cmp = ((Comparable) recordType).compareTo(other.recordType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) field).compareTo(other.field);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) target).compareTo(other.target);
  }

  public FieldAccess withRecordType(hydra.ext.lisp.syntax.Symbol recordType) {
    return new FieldAccess(recordType, field, target);
  }

  public FieldAccess withField(hydra.ext.lisp.syntax.Symbol field) {
    return new FieldAccess(recordType, field, target);
  }

  public FieldAccess withTarget(hydra.ext.lisp.syntax.Expression target) {
    return new FieldAccess(recordType, field, target);
  }
}
