// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class AmbientLinearQueryStatement implements Serializable, Comparable<AmbientLinearQueryStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AmbientLinearQueryStatement");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  private AmbientLinearQueryStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Simple instance) ;

    R visit(Nested instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AmbientLinearQueryStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }

    default R visit(Nested instance) {
      return otherwise(instance);
    }
  }

  public static final class Simple extends openGql.grammar.AmbientLinearQueryStatement implements Serializable {
    public final openGql.grammar.AmbientLinearQueryStatementSimpleAndPrimitiveResult value;

    public Simple (openGql.grammar.AmbientLinearQueryStatementSimpleAndPrimitiveResult value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(AmbientLinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Nested extends openGql.grammar.AmbientLinearQueryStatement implements Serializable {
    public final openGql.grammar.ProcedureBody value;

    public Nested (openGql.grammar.ProcedureBody value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nested)) {
        return false;
      }
      Nested o = (Nested) other;
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
    public int compareTo(AmbientLinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Nested o = (Nested) other;
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
