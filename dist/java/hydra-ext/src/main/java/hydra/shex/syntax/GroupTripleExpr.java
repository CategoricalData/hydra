// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class GroupTripleExpr implements Serializable, Comparable<GroupTripleExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.GroupTripleExpr");

  public static final hydra.core.Name SINGLE_ELEMENT_GROUP = new hydra.core.Name("SingleElementGroup");

  public static final hydra.core.Name MULTI_ELEMENT_GROUP = new hydra.core.Name("MultiElementGroup");

  private GroupTripleExpr () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(SingleElementGroup instance) ;

    R visit(MultiElementGroup instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GroupTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(SingleElementGroup instance) {
      return otherwise(instance);
    }

    default R visit(MultiElementGroup instance) {
      return otherwise(instance);
    }
  }

  public static final class SingleElementGroup extends hydra.shex.syntax.GroupTripleExpr implements Serializable {
    public final hydra.shex.syntax.SingleElementGroup value;

    public SingleElementGroup (hydra.shex.syntax.SingleElementGroup value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleElementGroup)) {
        return false;
      }
      SingleElementGroup o = (SingleElementGroup) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupTripleExpr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SingleElementGroup o = (SingleElementGroup) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MultiElementGroup extends hydra.shex.syntax.GroupTripleExpr implements Serializable {
    public final hydra.shex.syntax.MultiElementGroup value;

    public MultiElementGroup (hydra.shex.syntax.MultiElementGroup value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiElementGroup)) {
        return false;
      }
      MultiElementGroup o = (MultiElementGroup) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupTripleExpr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultiElementGroup o = (MultiElementGroup) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
