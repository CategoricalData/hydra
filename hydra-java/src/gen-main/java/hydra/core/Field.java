// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A labeled term
 */
public class Field<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Field");
  
  public final hydra.core.FieldName name;
  
  public final hydra.core.Term<A> term;
  
  public Field (hydra.core.FieldName name, hydra.core.Term<A> term) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (term == null) {
      throw new IllegalArgumentException("null value for 'term' argument");
    }
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
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Field(name, term);
  }
  
  public Field withTerm(hydra.core.Term<A> term) {
    if (term == null) {
      throw new IllegalArgumentException("null value for 'term' argument");
    }
    return new Field(name, term);
  }
}