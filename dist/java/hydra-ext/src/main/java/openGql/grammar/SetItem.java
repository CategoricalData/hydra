// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SetItem implements Serializable, Comparable<SetItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SetItem");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name ALL_PROPERTIES = new hydra.core.Name("allProperties");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  private SetItem () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Property instance) ;

    R visit(AllProperties instance) ;

    R visit(Label instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SetItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }

    default R visit(AllProperties instance) {
      return otherwise(instance);
    }

    default R visit(Label instance) {
      return otherwise(instance);
    }
  }

  public static final class Property extends openGql.grammar.SetItem implements Serializable {
    public final openGql.grammar.SetPropertyItem value;

    public Property (openGql.grammar.SetPropertyItem value) {
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
    public int compareTo(SetItem other) {
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

  public static final class AllProperties extends openGql.grammar.SetItem implements Serializable {
    public final openGql.grammar.SetAllPropertiesItem value;

    public AllProperties (openGql.grammar.SetAllPropertiesItem value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AllProperties)) {
        return false;
      }
      AllProperties o = (AllProperties) other;
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
    public int compareTo(SetItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AllProperties o = (AllProperties) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Label extends openGql.grammar.SetItem implements Serializable {
    public final openGql.grammar.SetLabelItem value;

    public Label (openGql.grammar.SetLabelItem value) {
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
    public int compareTo(SetItem other) {
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
