package hydra.core;

/**
 * A labeled term
 */
public class Field {
  /**
   * @type hydra/core.FieldName
   */
  public final FieldName name;
  
  /**
   * @type hydra/core.Term
   */
  public final Term term;
  
  /**
   * Constructs an immutable Field object
   */
  public Field(FieldName name, Term term) {
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
  public Field withName(FieldName name) {
    return new Field(name, term);
  }
  
  /**
   * Construct a new immutable Field object in which term is overridden
   */
  public Field withTerm(Term term) {
    return new Field(name, term);
  }
}
