// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class WhereWithPredicateArgs implements Serializable, Comparable<WhereWithPredicateArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WhereWithPredicateArgs");

  public static final hydra.core.Name LEFT_ARG = new hydra.core.Name("leftArg");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> leftArg;

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate;

  public WhereWithPredicateArgs (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> leftArg, hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    this.leftArg = leftArg;
    this.predicate = predicate;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhereWithPredicateArgs)) {
      return false;
    }
    WhereWithPredicateArgs o = (WhereWithPredicateArgs) other;
    return java.util.Objects.equals(
      this.leftArg,
      o.leftArg) && java.util.Objects.equals(
      this.predicate,
      o.predicate);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(leftArg) + 3 * java.util.Objects.hashCode(predicate);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WhereWithPredicateArgs other) {
    int cmp = 0;
    cmp = ((Comparable) leftArg).compareTo(other.leftArg);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) predicate).compareTo(other.predicate);
  }

  public WhereWithPredicateArgs withLeftArg(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> leftArg) {
    return new WhereWithPredicateArgs(leftArg, predicate);
  }

  public WhereWithPredicateArgs withPredicate(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    return new WhereWithPredicateArgs(leftArg, predicate);
  }
}
