// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class Individual implements Serializable, Comparable<Individual> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Individual");

  public static final hydra.core.Name NAMED = new hydra.core.Name("named");

  public static final hydra.core.Name ANONYMOUS = new hydra.core.Name("anonymous");

  private Individual () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Named instance) ;

    R visit(Anonymous instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Individual instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Named instance) {
      return otherwise(instance);
    }

    default R visit(Anonymous instance) {
      return otherwise(instance);
    }
  }

  public static final class Named extends hydra.ext.org.w3.owl.syntax.Individual implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.NamedIndividual value;

    public Named (hydra.ext.org.w3.owl.syntax.NamedIndividual value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) other;
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
    public int compareTo(Individual other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Named o = (Named) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Anonymous extends hydra.ext.org.w3.owl.syntax.Individual implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnonymousIndividual value;

    public Anonymous (hydra.ext.org.w3.owl.syntax.AnonymousIndividual value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) other;
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
    public int compareTo(Individual other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Anonymous o = (Anonymous) other;
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
