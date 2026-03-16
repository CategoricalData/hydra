// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ForIfClause implements Serializable, Comparable<ForIfClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.ForIfClause");
  
  public static final hydra.core.Name ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name TARGETS = new hydra.core.Name("targets");
  
  public static final hydra.core.Name IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name IFS = new hydra.core.Name("ifs");
  
  public final Boolean async;
  
  public final hydra.util.ConsList<hydra.ext.python.syntax.StarTarget> targets;
  
  public final hydra.ext.python.syntax.Disjunction in;
  
  public final hydra.util.ConsList<hydra.ext.python.syntax.Disjunction> ifs;
  
  public ForIfClause (Boolean async, hydra.util.ConsList<hydra.ext.python.syntax.StarTarget> targets, hydra.ext.python.syntax.Disjunction in, hydra.util.ConsList<hydra.ext.python.syntax.Disjunction> ifs) {
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
    ForIfClause o = (ForIfClause) other;
    return java.util.Objects.equals(
      this.async,
      o.async) && java.util.Objects.equals(
      this.targets,
      o.targets) && java.util.Objects.equals(
      this.in,
      o.in) && java.util.Objects.equals(
      this.ifs,
      o.ifs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(async) + 3 * java.util.Objects.hashCode(targets) + 5 * java.util.Objects.hashCode(in) + 7 * java.util.Objects.hashCode(ifs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForIfClause other) {
    int cmp = 0;
    cmp = ((Comparable) async).compareTo(other.async);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) targets).compareTo(other.targets);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) in).compareTo(other.in);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) ifs).compareTo(other.ifs);
  }
  
  public ForIfClause withAsync(Boolean async) {
    return new ForIfClause(async, targets, in, ifs);
  }
  
  public ForIfClause withTargets(hydra.util.ConsList<hydra.ext.python.syntax.StarTarget> targets) {
    return new ForIfClause(async, targets, in, ifs);
  }
  
  public ForIfClause withIn(hydra.ext.python.syntax.Disjunction in) {
    return new ForIfClause(async, targets, in, ifs);
  }
  
  public ForIfClause withIfs(hydra.util.ConsList<hydra.ext.python.syntax.Disjunction> ifs) {
    return new ForIfClause(async, targets, in, ifs);
  }
}
