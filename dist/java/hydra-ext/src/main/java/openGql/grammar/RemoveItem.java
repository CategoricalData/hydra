// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class RemoveItem implements Serializable, Comparable<RemoveItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.RemoveItem");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  private RemoveItem () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Property instance) ;

    R visit(Label instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RemoveItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }

    default R visit(Label instance) {
      return otherwise(instance);
    }
  }

  public static final class Property extends openGql.grammar.RemoveItem implements Serializable {
    public final openGql.grammar.RemovePropertyItem value;

    public Property (openGql.grammar.RemovePropertyItem value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) other;
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
    public int compareTo(RemoveItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Label extends openGql.grammar.RemoveItem implements Serializable {
    public final openGql.grammar.RemoveLabelItem value;

    public Label (openGql.grammar.RemoveLabelItem value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Label)) {
        return false;
      }
      Label o = (Label) other;
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
    public int compareTo(RemoveItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Label o = (Label) other;
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
