// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class CodeDecl_Alts implements Serializable, Comparable<CodeDecl_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.CodeDecl_Alts");

  public static final hydra.core.Name CODE = new hydra.core.Name("Code");

  public static final hydra.core.Name PERCNT = new hydra.core.Name("Percnt");

  private CodeDecl_Alts () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Code instance) ;

    R visit(Percnt instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CodeDecl_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Code instance) {
      return otherwise(instance);
    }

    default R visit(Percnt instance) {
      return otherwise(instance);
    }
  }

  public static final class Code extends hydra.shex.syntax.CodeDecl_Alts implements Serializable {
    public final hydra.shex.syntax.Code value;

    public Code (hydra.shex.syntax.Code value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Code)) {
        return false;
      }
      Code o = (Code) other;
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
    public int compareTo(CodeDecl_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Code o = (Code) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Percnt extends hydra.shex.syntax.CodeDecl_Alts implements Serializable {
    public Percnt () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Percnt)) {
        return false;
      }
      Percnt o = (Percnt) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CodeDecl_Alts other) {
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
