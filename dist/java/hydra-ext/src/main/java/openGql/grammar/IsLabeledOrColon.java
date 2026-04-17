// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class IsLabeledOrColon implements Serializable, Comparable<IsLabeledOrColon> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.IsLabeledOrColon");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name COLON = new hydra.core.Name("colon");

  private IsLabeledOrColon () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Not instance) ;

    R visit(Colon instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IsLabeledOrColon instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Not instance) {
      return otherwise(instance);
    }

    default R visit(Colon instance) {
      return otherwise(instance);
    }
  }

  public static final class Not extends openGql.grammar.IsLabeledOrColon implements Serializable {
    public final Boolean value;

    public Not (Boolean value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
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
    public int compareTo(IsLabeledOrColon other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Not o = (Not) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Colon extends openGql.grammar.IsLabeledOrColon implements Serializable {
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
    public int compareTo(IsLabeledOrColon other) {
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
