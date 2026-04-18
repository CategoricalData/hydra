// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class StringLength implements Serializable, Comparable<StringLength> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.StringLength");

  public static final hydra.core.Name L_E_N_G_T_H = new hydra.core.Name("LENGTH");

  public static final hydra.core.Name M_I_N_L_E_N_G_T_H = new hydra.core.Name("MINLENGTH");

  public static final hydra.core.Name M_A_X_L_E_N_G_T_H = new hydra.core.Name("MAXLENGTH");

  private StringLength () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LENGTH instance) ;

    R visit(MINLENGTH instance) ;

    R visit(MAXLENGTH instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringLength instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LENGTH instance) {
      return otherwise(instance);
    }

    default R visit(MINLENGTH instance) {
      return otherwise(instance);
    }

    default R visit(MAXLENGTH instance) {
      return otherwise(instance);
    }
  }

  public static final class LENGTH extends hydra.shex.syntax.StringLength implements Serializable {
    public LENGTH () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LENGTH)) {
        return false;
      }
      LENGTH o = (LENGTH) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StringLength other) {
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

  public static final class MINLENGTH extends hydra.shex.syntax.StringLength implements Serializable {
    public MINLENGTH () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MINLENGTH)) {
        return false;
      }
      MINLENGTH o = (MINLENGTH) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StringLength other) {
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

  public static final class MAXLENGTH extends hydra.shex.syntax.StringLength implements Serializable {
    public MAXLENGTH () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MAXLENGTH)) {
        return false;
      }
      MAXLENGTH o = (MAXLENGTH) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StringLength other) {
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
