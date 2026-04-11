// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class ShapeDefinition_ListOfAlts_Elmt implements Serializable, Comparable<ShapeDefinition_ListOfAlts_Elmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt");

  public static final hydra.core.Name INCLUDE_SET = new hydra.core.Name("IncludeSet");

  public static final hydra.core.Name EXTRA_PROPERTY_SET = new hydra.core.Name("ExtraPropertySet");

  public static final hydra.core.Name C_L_O_S_E_D = new hydra.core.Name("CLOSED");

  private ShapeDefinition_ListOfAlts_Elmt () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(IncludeSet instance) ;

    R visit(ExtraPropertySet instance) ;

    R visit(CLOSED instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShapeDefinition_ListOfAlts_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(IncludeSet instance) {
      return otherwise(instance);
    }

    default R visit(ExtraPropertySet instance) {
      return otherwise(instance);
    }

    default R visit(CLOSED instance) {
      return otherwise(instance);
    }
  }

  public static final class IncludeSet extends hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt implements Serializable {
    public final hydra.ext.io.shex.syntax.IncludeSet value;

    public IncludeSet (hydra.ext.io.shex.syntax.IncludeSet value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IncludeSet)) {
        return false;
      }
      IncludeSet o = (IncludeSet) other;
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
    public int compareTo(ShapeDefinition_ListOfAlts_Elmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IncludeSet o = (IncludeSet) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ExtraPropertySet extends hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt implements Serializable {
    public final hydra.ext.io.shex.syntax.ExtraPropertySet value;

    public ExtraPropertySet (hydra.ext.io.shex.syntax.ExtraPropertySet value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExtraPropertySet)) {
        return false;
      }
      ExtraPropertySet o = (ExtraPropertySet) other;
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
    public int compareTo(ShapeDefinition_ListOfAlts_Elmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ExtraPropertySet o = (ExtraPropertySet) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CLOSED extends hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt implements Serializable {
    public CLOSED () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CLOSED)) {
        return false;
      }
      CLOSED o = (CLOSED) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ShapeDefinition_ListOfAlts_Elmt other) {
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
