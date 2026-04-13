// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class InnerTripleExpr implements Serializable, Comparable<InnerTripleExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.InnerTripleExpr");

  public static final hydra.core.Name MULTI_ELEMENT_GROUP = new hydra.core.Name("MultiElementGroup");

  public static final hydra.core.Name MULTI_ELEMENT_ONE_OF = new hydra.core.Name("MultiElementOneOf");

  private InnerTripleExpr () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(MultiElementGroup instance) ;

    R visit(MultiElementOneOf instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InnerTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(MultiElementGroup instance) {
      return otherwise(instance);
    }

    default R visit(MultiElementOneOf instance) {
      return otherwise(instance);
    }
  }

  public static final class MultiElementGroup extends hydra.shex.syntax.InnerTripleExpr implements Serializable {
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
    public int compareTo(InnerTripleExpr other) {
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

  public static final class MultiElementOneOf extends hydra.shex.syntax.InnerTripleExpr implements Serializable {
    public final hydra.shex.syntax.MultiElementOneOf value;

    public MultiElementOneOf (hydra.shex.syntax.MultiElementOneOf value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiElementOneOf)) {
        return false;
      }
      MultiElementOneOf o = (MultiElementOneOf) other;
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
    public int compareTo(InnerTripleExpr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultiElementOneOf o = (MultiElementOneOf) other;
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
