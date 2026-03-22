// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class ShexDoc_Sequence_Option_Alts implements Serializable, Comparable<ShexDoc_Sequence_Option_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts");

  public static final hydra.core.Name NOT_START_ACTION = new hydra.core.Name("NotStartAction");

  public static final hydra.core.Name START_ACTIONS = new hydra.core.Name("StartActions");

  private ShexDoc_Sequence_Option_Alts () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NotStartAction instance) ;

    R visit(StartActions instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShexDoc_Sequence_Option_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NotStartAction instance) {
      return otherwise(instance);
    }

    default R visit(StartActions instance) {
      return otherwise(instance);
    }
  }

  public static final class NotStartAction extends hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.NotStartAction value;

    public NotStartAction (hydra.ext.io.shex.syntax.NotStartAction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotStartAction)) {
        return false;
      }
      NotStartAction o = (NotStartAction) other;
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
    public int compareTo(ShexDoc_Sequence_Option_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotStartAction o = (NotStartAction) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class StartActions extends hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.StartActions value;

    public StartActions (hydra.ext.io.shex.syntax.StartActions value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StartActions)) {
        return false;
      }
      StartActions o = (StartActions) other;
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
    public int compareTo(ShexDoc_Sequence_Option_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StartActions o = (StartActions) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
