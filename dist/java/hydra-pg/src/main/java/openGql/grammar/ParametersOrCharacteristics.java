// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ParametersOrCharacteristics implements Serializable, Comparable<ParametersOrCharacteristics> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ParametersOrCharacteristics");

  public static final hydra.core.Name PARAMETERS = new hydra.core.Name("parameters");

  public static final hydra.core.Name CHARACTERISTICS = new hydra.core.Name("characteristics");

  private ParametersOrCharacteristics () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Parameters instance) ;

    R visit(Characteristics instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ParametersOrCharacteristics instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Parameters instance) {
      return otherwise(instance);
    }

    default R visit(Characteristics instance) {
      return otherwise(instance);
    }
  }

  public static final class Parameters extends openGql.grammar.ParametersOrCharacteristics implements Serializable {
    public Parameters () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameters)) {
        return false;
      }
      Parameters o = (Parameters) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ParametersOrCharacteristics other) {
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

  public static final class Characteristics extends openGql.grammar.ParametersOrCharacteristics implements Serializable {
    public Characteristics () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Characteristics)) {
        return false;
      }
      Characteristics o = (Characteristics) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ParametersOrCharacteristics other) {
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
