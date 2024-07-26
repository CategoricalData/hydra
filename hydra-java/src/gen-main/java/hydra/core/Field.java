// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A labeled term
 */
public class Field implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Field");
  
  public final hydra.core.FieldName name;
  
  public final hydra.core.Term term;
  
  public Field (hydra.core.FieldName name, hydra.core.Term term) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((term));
    this.name = name;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) (other);
    return name.equals(o.name) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * term.hashCode();
  }
  
  public Field withName(hydra.core.FieldName name) {
    java.util.Objects.requireNonNull((name));
    return new Field(name, term);
  }
  
  public Field withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new Field(name, term);
  }
}