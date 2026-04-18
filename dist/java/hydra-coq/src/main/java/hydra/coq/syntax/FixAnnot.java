// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class FixAnnot implements Serializable, Comparable<FixAnnot> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.FixAnnot");

  public static final hydra.core.Name STRUCT = new hydra.core.Name("struct");

  public static final hydra.core.Name WF = new hydra.core.Name("wf");

  public static final hydra.core.Name MEASURE = new hydra.core.Name("measure");

  private FixAnnot () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Struct instance) ;

    R visit(Wf instance) ;

    R visit(Measure instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FixAnnot instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Struct instance) {
      return otherwise(instance);
    }

    default R visit(Wf instance) {
      return otherwise(instance);
    }

    default R visit(Measure instance) {
      return otherwise(instance);
    }
  }

  public static final class Struct extends hydra.coq.syntax.FixAnnot implements Serializable {
    public final hydra.coq.syntax.Ident value;

    public Struct (hydra.coq.syntax.Ident value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Struct)) {
        return false;
      }
      Struct o = (Struct) other;
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
    public int compareTo(FixAnnot other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Struct o = (Struct) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Wf extends hydra.coq.syntax.FixAnnot implements Serializable {
    public final hydra.coq.syntax.FixAnnot_Wf value;

    public Wf (hydra.coq.syntax.FixAnnot_Wf value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wf)) {
        return false;
      }
      Wf o = (Wf) other;
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
    public int compareTo(FixAnnot other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Wf o = (Wf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Measure extends hydra.coq.syntax.FixAnnot implements Serializable {
    public final hydra.coq.syntax.FixAnnot_Measure value;

    public Measure (hydra.coq.syntax.FixAnnot_Measure value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Measure)) {
        return false;
      }
      Measure o = (Measure) other;
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
    public int compareTo(FixAnnot other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Measure o = (Measure) other;
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
