package hydra.ext.coq.syntax;

public class TypeCast {
  public final Term10 term;
  
  public final Type type;
  
  public final TypeCastOperator operator;
  
  public TypeCast (Term10 term, Type type, TypeCastOperator operator) {
    this.term = term;
    this.type = type;
    this.operator = operator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeCast)) {
      return false;
    }
    TypeCast o = (TypeCast) (other);
    return term.equals(o.term) && type.equals(o.type) && operator.equals(o.operator);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * type.hashCode() + 5 * operator.hashCode();
  }
  
  public TypeCast withTerm(Term10 term) {
    return new TypeCast(term, type, operator);
  }
  
  public TypeCast withType(Type type) {
    return new TypeCast(term, type, operator);
  }
  
  public TypeCast withOperator(TypeCastOperator operator) {
    return new TypeCast(term, type, operator);
  }
}