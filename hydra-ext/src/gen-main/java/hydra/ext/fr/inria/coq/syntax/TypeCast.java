// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class TypeCast implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.TypeCast");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public final hydra.ext.fr.inria.coq.syntax.Term10 term;
  
  public final hydra.ext.fr.inria.coq.syntax.Type type;
  
  public final hydra.ext.fr.inria.coq.syntax.TypeCastOperator operator;
  
  public TypeCast (hydra.ext.fr.inria.coq.syntax.Term10 term, hydra.ext.fr.inria.coq.syntax.Type type, hydra.ext.fr.inria.coq.syntax.TypeCastOperator operator) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((operator));
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
  
  public TypeCast withTerm(hydra.ext.fr.inria.coq.syntax.Term10 term) {
    java.util.Objects.requireNonNull((term));
    return new TypeCast(term, type, operator);
  }
  
  public TypeCast withType(hydra.ext.fr.inria.coq.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypeCast(term, type, operator);
  }
  
  public TypeCast withOperator(hydra.ext.fr.inria.coq.syntax.TypeCastOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new TypeCast(term, type, operator);
  }
}