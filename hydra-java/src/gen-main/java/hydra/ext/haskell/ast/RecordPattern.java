// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A record pattern
 */
public class RecordPattern implements Serializable, Comparable<RecordPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.RecordPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  /**
   * The constructor name
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The field patterns
   */
  public final java.util.List<hydra.ext.haskell.ast.PatternField> fields;
  
  public RecordPattern (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.PatternField> fields) {
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordPattern)) {
      return false;
    }
    RecordPattern o = (RecordPattern) (other);
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
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      fields.hashCode(),
      other.fields.hashCode());
  }
  
  public RecordPattern withName(hydra.ext.haskell.ast.Name name) {
    return new RecordPattern(name, fields);
  }
  
  public RecordPattern withFields(java.util.List<hydra.ext.haskell.ast.PatternField> fields) {
    return new RecordPattern(name, fields);
  }
}
