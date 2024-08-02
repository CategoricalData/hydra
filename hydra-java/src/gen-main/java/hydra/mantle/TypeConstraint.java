// Note: this is an automatically generated file. Do not edit.

package hydra.mantle;

import java.io.Serializable;

/**
 * An assertion that two types can be unified into a single type
 */
public class TypeConstraint implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/mantle.TypeConstraint");
  
  public final hydra.core.Type left;
  
  public final hydra.core.Type right;
  
  public final hydra.util.Opt<String> context;
  
  public TypeConstraint (hydra.core.Type left, hydra.core.Type right, hydra.util.Opt<String> context) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    java.util.Objects.requireNonNull((context));
    this.left = left;
    this.right = right;
    this.context = context;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeConstraint)) {
      return false;
    }
    TypeConstraint o = (TypeConstraint) (other);
    return left.equals(o.left) && right.equals(o.right) && context.equals(o.context);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode() + 5 * context.hashCode();
  }
  
  public TypeConstraint withLeft(hydra.core.Type left) {
    java.util.Objects.requireNonNull((left));
    return new TypeConstraint(left, right, context);
  }
  
  public TypeConstraint withRight(hydra.core.Type right) {
    java.util.Objects.requireNonNull((right));
    return new TypeConstraint(left, right, context);
  }
  
  public TypeConstraint withContext(hydra.util.Opt<String> context) {
    java.util.Objects.requireNonNull((context));
    return new TypeConstraint(left, right, context);
  }
}