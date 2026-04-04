// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class CaseTree implements Serializable, Comparable<CaseTree> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.CaseTree");

  public static final hydra.core.Name CASE = new hydra.core.Name("case");

  public static final hydra.core.Name TYPE_CASE = new hydra.core.Name("typeCase");

  private CaseTree () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Case instance) ;

    R visit(TypeCase instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CaseTree instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Case instance) {
      return otherwise(instance);
    }

    default R visit(TypeCase instance) {
      return otherwise(instance);
    }
  }

  public static final class Case extends hydra.ext.scala.syntax.CaseTree implements Serializable {
    public final hydra.ext.scala.syntax.Case value;

    public Case (hydra.ext.scala.syntax.Case value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
        return false;
      }
      Case o = (Case) other;
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
    public int compareTo(CaseTree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Case o = (Case) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TypeCase extends hydra.ext.scala.syntax.CaseTree implements Serializable {
    public final hydra.ext.scala.syntax.TypeCase value;

    public TypeCase (hydra.ext.scala.syntax.TypeCase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeCase)) {
        return false;
      }
      TypeCase o = (TypeCase) other;
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
    public int compareTo(CaseTree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeCase o = (TypeCase) other;
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
