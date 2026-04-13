// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A record-style data constructor
 */
public class RecordConstructor implements Serializable, Comparable<RecordConstructor> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.RecordConstructor");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");

  /**
   * The name of the constructor
   */
  public final hydra.haskell.syntax.Name name;

  /**
   * The named fields of the record
   */
  public final java.util.List<hydra.haskell.syntax.FieldWithComments> fields;

  public RecordConstructor (hydra.haskell.syntax.Name name, java.util.List<hydra.haskell.syntax.FieldWithComments> fields) {
    this.name = name;
    this.fields = fields;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordConstructor)) {
      return false;
    }
    RecordConstructor o = (RecordConstructor) other;
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
  public int compareTo(RecordConstructor other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      fields,
      other.fields);
  }

  public RecordConstructor withName(hydra.haskell.syntax.Name name) {
    return new RecordConstructor(name, fields);
  }

  public RecordConstructor withFields(java.util.List<hydra.haskell.syntax.FieldWithComments> fields) {
    return new RecordConstructor(name, fields);
  }
}
