package hydra.ext.coq.syntax;

public class TypeCast {
  public final hydra.ext.coq.syntax.Term10 term;
  
  public final hydra.ext.coq.syntax.Type type;
  
  public final hydra.ext.coq.syntax.TypeCastOperator operator;
  
  public TypeCast (hydra.ext.coq.syntax.Term10 term, hydra.ext.coq.syntax.Type type, hydra.ext.coq.syntax.TypeCastOperator operator) {
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
  
  public TypeCast withTerm(hydra.ext.coq.syntax.Term10 term) {
    return new TypeCast(term, type, operator);
  }
  
  public TypeCast withType(hydra.ext.coq.syntax.Type type) {
    return new TypeCast(term, type, operator);
  }
  
  public TypeCast withOperator(hydra.ext.coq.syntax.TypeCastOperator operator) {
    return new TypeCast(term, type, operator);
  }
}