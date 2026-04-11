// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class RepeatRange_Sequence_Option_Option_Option implements Serializable, Comparable<RepeatRange_Sequence_Option_Option_Option> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("Integer");

  public static final hydra.core.Name AST = new hydra.core.Name("Ast");

  private RepeatRange_Sequence_Option_Option_Option () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Integer_ instance) ;

    R visit(Ast instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RepeatRange_Sequence_Option_Option_Option instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Integer_ instance) {
      return otherwise(instance);
    }

    default R visit(Ast instance) {
      return otherwise(instance);
    }
  }

  public static final class Integer_ extends hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option implements Serializable {
    public final hydra.ext.io.shex.syntax.Integer_ value;

    public Integer_ (hydra.ext.io.shex.syntax.Integer_ value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) other;
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
    public int compareTo(RepeatRange_Sequence_Option_Option_Option other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer_ o = (Integer_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Ast extends hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option implements Serializable {
    public Ast () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ast)) {
        return false;
      }
      Ast o = (Ast) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(RepeatRange_Sequence_Option_Option_Option other) {
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
