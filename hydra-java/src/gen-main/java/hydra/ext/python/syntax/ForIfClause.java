// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ForIfClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ForIfClause");
  
  public static final hydra.core.Name FIELD_NAME_ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name FIELD_NAME_TARGETS = new hydra.core.Name("targets");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_IFS = new hydra.core.Name("ifs");
  
  public final Boolean async;
  
  public final java.util.List<hydra.ext.python.syntax.StarTarget> targets;
  
  public final hydra.ext.python.syntax.Disjunction in;
  
  public final java.util.List<hydra.ext.python.syntax.Disjunction> ifs;
  
  public ForIfClause (Boolean async, java.util.List<hydra.ext.python.syntax.StarTarget> targets, hydra.ext.python.syntax.Disjunction in, java.util.List<hydra.ext.python.syntax.Disjunction> ifs) {
    java.util.Objects.requireNonNull((async));
    java.util.Objects.requireNonNull((targets));
    java.util.Objects.requireNonNull((in));
    java.util.Objects.requireNonNull((ifs));
    this.async = async;
    this.targets = targets;
    this.in = in;
    this.ifs = ifs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForIfClause)) {
      return false;
    }
    ForIfClause o = (ForIfClause) (other);
    return async.equals(o.async) && targets.equals(o.targets) && in.equals(o.in) && ifs.equals(o.ifs);
  }
  
  @Override
  public int hashCode() {
    return 2 * async.hashCode() + 3 * targets.hashCode() + 5 * in.hashCode() + 7 * ifs.hashCode();
  }
  
  public ForIfClause withAsync(Boolean async) {
    java.util.Objects.requireNonNull((async));
    return new ForIfClause(async, targets, in, ifs);
  }
  
  public ForIfClause withTargets(java.util.List<hydra.ext.python.syntax.StarTarget> targets) {
    java.util.Objects.requireNonNull((targets));
    return new ForIfClause(async, targets, in, ifs);
  }
  
  public ForIfClause withIn(hydra.ext.python.syntax.Disjunction in) {
    java.util.Objects.requireNonNull((in));
    return new ForIfClause(async, targets, in, ifs);
  }
  
  public ForIfClause withIfs(java.util.List<hydra.ext.python.syntax.Disjunction> ifs) {
    java.util.Objects.requireNonNull((ifs));
    return new ForIfClause(async, targets, in, ifs);
  }
}