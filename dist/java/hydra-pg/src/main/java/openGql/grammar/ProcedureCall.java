// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ProcedureCall implements Serializable, Comparable<ProcedureCall> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ProcedureCall");

  public static final hydra.core.Name INLINE = new hydra.core.Name("inline");

  public static final hydra.core.Name NAMED = new hydra.core.Name("named");

  private ProcedureCall () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Inline instance) ;

    R visit(Named instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ProcedureCall instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Inline instance) {
      return otherwise(instance);
    }

    default R visit(Named instance) {
      return otherwise(instance);
    }
  }

  public static final class Inline extends openGql.grammar.ProcedureCall implements Serializable {
    public final openGql.grammar.InlineProcedureCall value;

    public Inline (openGql.grammar.InlineProcedureCall value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inline)) {
        return false;
      }
      Inline o = (Inline) other;
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
    public int compareTo(ProcedureCall other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Inline o = (Inline) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Named extends openGql.grammar.ProcedureCall implements Serializable {
    public final openGql.grammar.NamedProcedureCall value;

    public Named (openGql.grammar.NamedProcedureCall value) {
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
    public int compareTo(ProcedureCall other) {
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
}
