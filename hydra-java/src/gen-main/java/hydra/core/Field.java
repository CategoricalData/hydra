// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A name/term pair
 */
public class Field implements Serializable, Comparable<Field> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Field");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  /**
   * The name of the field
   */
  public final hydra.core.Name name;
  
  /**
   * The term value of the field
   */
  public final hydra.core.Term term;
  
  public Field (hydra.core.Name name, hydra.core.Term term) {
    this.name = name;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.term,
      o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(term);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Field other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) term).compareTo(other.term);
  }
  
  public Field withName(hydra.core.Name name) {
    return new Field(name, term);
  }
  
  public Field withTerm(hydra.core.Term term) {
    return new Field(name, term);
  }
}
