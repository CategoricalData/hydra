// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NullOrdering implements Serializable, Comparable<NullOrdering> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NullOrdering");

  public static final hydra.core.Name NULLS_FIRST = new hydra.core.Name("nullsFirst");

  public static final hydra.core.Name NULLS_LAST = new hydra.core.Name("nullsLast");

  private NullOrdering () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NullsFirst instance) ;

    R visit(NullsLast instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NullOrdering instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NullsFirst instance) {
      return otherwise(instance);
    }

    default R visit(NullsLast instance) {
      return otherwise(instance);
    }
  }

  public static final class NullsFirst extends openGql.grammar.NullOrdering implements Serializable {
    public NullsFirst () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullsFirst)) {
        return false;
      }
      NullsFirst o = (NullsFirst) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NullOrdering other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NullsLast extends openGql.grammar.NullOrdering implements Serializable {
    public NullsLast () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullsLast)) {
        return false;
      }
      NullsLast o = (NullsLast) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NullOrdering other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
