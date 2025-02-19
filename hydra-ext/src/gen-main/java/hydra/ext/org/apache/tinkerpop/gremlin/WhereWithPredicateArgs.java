// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class WhereWithPredicateArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WhereWithPredicateArgs");
  
  public static final hydra.core.Name FIELD_NAME_LEFT_ARG = new hydra.core.Name("leftArg");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public final hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> leftArg;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public WhereWithPredicateArgs (hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> leftArg, hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    java.util.Objects.requireNonNull((leftArg));
    java.util.Objects.requireNonNull((predicate));
    this.leftArg = leftArg;
    this.predicate = predicate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhereWithPredicateArgs)) {
      return false;
    }
    WhereWithPredicateArgs o = (WhereWithPredicateArgs) (other);
    return leftArg.equals(o.leftArg) && predicate.equals(o.predicate);
  }
  
  @Override
  public int hashCode() {
    return 2 * leftArg.hashCode() + 3 * predicate.hashCode();
  }
  
  public WhereWithPredicateArgs withLeftArg(hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> leftArg) {
    java.util.Objects.requireNonNull((leftArg));
    return new WhereWithPredicateArgs(leftArg, predicate);
  }
  
  public WhereWithPredicateArgs withPredicate(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new WhereWithPredicateArgs(leftArg, predicate);
  }
}