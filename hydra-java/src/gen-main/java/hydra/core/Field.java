package hydra.core;

/**
 * A labeled term
 */
public class Field<M> {
  public final FieldName name;
  
  public final Term<M> term;
  
  public Field (FieldName name, Term<M> term) {
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
  
  public Field withName(FieldName name) {
    return new Field(name, term);
  }
  
  public Field withTerm(Term<M> term) {
    return new Field(name, term);
  }
}