// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A record pattern
 */
public class RecordPattern implements Serializable, Comparable<RecordPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.RecordPattern");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");

  /**
   * The constructor name
   */
  public final hydra.haskell.syntax.Name name;

  /**
   * The field patterns
   */
  public final java.util.List<hydra.haskell.syntax.PatternField> fields;

  public RecordPattern (hydra.haskell.syntax.Name name, java.util.List<hydra.haskell.syntax.PatternField> fields) {
    this.name = name;
    this.fields = fields;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordPattern)) {
      return false;
    }
    RecordPattern o = (RecordPattern) other;
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
  public int compareTo(RecordPattern other) {
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

  public RecordPattern withName(hydra.haskell.syntax.Name name) {
    return new RecordPattern(name, fields);
  }

  public RecordPattern withFields(java.util.List<hydra.haskell.syntax.PatternField> fields) {
    return new RecordPattern(name, fields);
  }
}
