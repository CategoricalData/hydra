// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class IsOrColon implements Serializable, Comparable<IsOrColon> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.IsOrColon");

  public static final hydra.core.Name IS = new hydra.core.Name("is");

  public static final hydra.core.Name COLON = new hydra.core.Name("colon");

  private IsOrColon () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Is instance) ;

    R visit(Colon instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IsOrColon instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Is instance) {
      return otherwise(instance);
    }

    default R visit(Colon instance) {
      return otherwise(instance);
    }
  }

  public static final class Is extends openGql.grammar.IsOrColon implements Serializable {
    public Is () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Is)) {
        return false;
      }
      Is o = (Is) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IsOrColon other) {
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

  public static final class Colon extends openGql.grammar.IsOrColon implements Serializable {
    public Colon () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Colon)) {
        return false;
      }
      Colon o = (Colon) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IsOrColon other) {
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
