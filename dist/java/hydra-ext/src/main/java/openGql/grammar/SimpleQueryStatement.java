// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimpleQueryStatement implements Serializable, Comparable<SimpleQueryStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimpleQueryStatement");

  public static final hydra.core.Name PRIMITIVE = new hydra.core.Name("primitive");

  public static final hydra.core.Name CALL = new hydra.core.Name("call");

  private SimpleQueryStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Primitive instance) ;

    R visit(Call instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimpleQueryStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Primitive instance) {
      return otherwise(instance);
    }

    default R visit(Call instance) {
      return otherwise(instance);
    }
  }

  public static final class Primitive extends openGql.grammar.SimpleQueryStatement implements Serializable {
    public final openGql.grammar.PrimitiveQueryStatement value;

    public Primitive (openGql.grammar.PrimitiveQueryStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) other;
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
    public int compareTo(SimpleQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primitive o = (Primitive) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Call extends openGql.grammar.SimpleQueryStatement implements Serializable {
    public final openGql.grammar.CallProcedureStatement value;

    public Call (openGql.grammar.CallProcedureStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Call)) {
        return false;
      }
      Call o = (Call) other;
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
    public int compareTo(SimpleQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Call o = (Call) other;
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
