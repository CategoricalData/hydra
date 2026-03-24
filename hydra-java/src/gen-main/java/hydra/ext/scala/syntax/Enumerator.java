// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Enumerator implements Serializable, Comparable<Enumerator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Enumerator");

  public static final hydra.core.Name GENERATOR = new hydra.core.Name("generator");

  public static final hydra.core.Name CASE_GENERATOR = new hydra.core.Name("caseGenerator");

  public static final hydra.core.Name VAL = new hydra.core.Name("val");

  public static final hydra.core.Name GUARD = new hydra.core.Name("guard");

  private Enumerator () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Generator instance) ;

    R visit(CaseGenerator instance) ;

    R visit(Val instance) ;

    R visit(Guard instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Enumerator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Generator instance) {
      return otherwise(instance);
    }

    default R visit(CaseGenerator instance) {
      return otherwise(instance);
    }

    default R visit(Val instance) {
      return otherwise(instance);
    }

    default R visit(Guard instance) {
      return otherwise(instance);
    }
  }

  public static final class Generator extends hydra.ext.scala.syntax.Enumerator implements Serializable {
    public final hydra.ext.scala.syntax.Enumerator_Generator value;

    public Generator (hydra.ext.scala.syntax.Enumerator_Generator value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Generator)) {
        return false;
      }
      Generator o = (Generator) other;
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
    public int compareTo(Enumerator other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Generator o = (Generator) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CaseGenerator extends hydra.ext.scala.syntax.Enumerator implements Serializable {
    public final hydra.ext.scala.syntax.Enumerator_CaseGenerator value;

    public CaseGenerator (hydra.ext.scala.syntax.Enumerator_CaseGenerator value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseGenerator)) {
        return false;
      }
      CaseGenerator o = (CaseGenerator) other;
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
    public int compareTo(Enumerator other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CaseGenerator o = (CaseGenerator) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Val extends hydra.ext.scala.syntax.Enumerator implements Serializable {
    public final hydra.ext.scala.syntax.Enumerator_Val value;

    public Val (hydra.ext.scala.syntax.Enumerator_Val value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Val)) {
        return false;
      }
      Val o = (Val) other;
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
    public int compareTo(Enumerator other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Val o = (Val) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Guard extends hydra.ext.scala.syntax.Enumerator implements Serializable {
    public final hydra.ext.scala.syntax.Enumerator_Guard value;

    public Guard (hydra.ext.scala.syntax.Enumerator_Guard value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Guard)) {
        return false;
      }
      Guard o = (Guard) other;
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
    public int compareTo(Enumerator other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Guard o = (Guard) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
