package hydra.core;

/**
 * A labeled term
 */
public class Field<A> {
  public final hydra.core.FieldName name;
  
  public final hydra.core.Term<A> term;
  
  /**
   * Constructs an immutable Field object
   */
  public Field(hydra.core.FieldName name, hydra.core.Term<A> term) {
    this.name = name;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
        return false;
    }
    Field o = (Field) other;
    return name.equals(o.name)
        && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode()
        + 3 * term.hashCode();
  }
  
  /**
   * Construct a new immutable Field object in which name is overridden
   */
  public Field withName(hydra.core.FieldName name) {
    return new Field(name, term);
  }
  
  /**
   * Construct a new immutable Field object in which term is overridden
   */
  public Field withTerm(hydra.core.Term<A> term) {
    return new Field(name, term);
  }
}
