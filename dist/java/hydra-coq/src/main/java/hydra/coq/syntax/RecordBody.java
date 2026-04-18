// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * The body of a Record definition
 */
public class RecordBody implements Serializable, Comparable<RecordBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.RecordBody");

  public static final hydra.core.Name CONSTRUCTOR = new hydra.core.Name("constructor");

  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");

  public final hydra.util.Maybe<hydra.coq.syntax.Ident> constructor;

  public final java.util.List<hydra.coq.syntax.RecordField> fields;

  public RecordBody (hydra.util.Maybe<hydra.coq.syntax.Ident> constructor, java.util.List<hydra.coq.syntax.RecordField> fields) {
    this.constructor = constructor;
    this.fields = fields;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordBody)) {
      return false;
    }
    RecordBody o = (RecordBody) other;
    return java.util.Objects.equals(
      this.constructor,
      o.constructor) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(constructor) + 3 * java.util.Objects.hashCode(fields);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RecordBody other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      constructor,
      other.constructor);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      fields,
      other.fields);
  }

  public RecordBody withConstructor(hydra.util.Maybe<hydra.coq.syntax.Ident> constructor) {
    return new RecordBody(constructor, fields);
  }

  public RecordBody withFields(java.util.List<hydra.coq.syntax.RecordField> fields) {
    return new RecordBody(constructor, fields);
  }
}
