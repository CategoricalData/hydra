// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A field in a Record definition
 */
public class RecordField implements Serializable, Comparable<RecordField> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.RecordField");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public final hydra.coq.syntax.Ident name;

  public final hydra.coq.syntax.Type type;

  public RecordField (hydra.coq.syntax.Ident name, hydra.coq.syntax.Type type) {
    this.name = name;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordField)) {
      return false;
    }
    RecordField o = (RecordField) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RecordField other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public RecordField withName(hydra.coq.syntax.Ident name) {
    return new RecordField(name, type);
  }

  public RecordField withType(hydra.coq.syntax.Type type) {
    return new RecordField(name, type);
  }
}
