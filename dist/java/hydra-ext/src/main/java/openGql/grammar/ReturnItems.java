// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ReturnItems implements Serializable, Comparable<ReturnItems> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ReturnItems");

  public static final hydra.core.Name ASTERISK = new hydra.core.Name("asterisk");

  public static final hydra.core.Name ITEM_LIST = new hydra.core.Name("itemList");

  private ReturnItems () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Asterisk instance) ;

    R visit(ItemList instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ReturnItems instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Asterisk instance) {
      return otherwise(instance);
    }

    default R visit(ItemList instance) {
      return otherwise(instance);
    }
  }

  public static final class Asterisk extends openGql.grammar.ReturnItems implements Serializable {
    public Asterisk () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Asterisk)) {
        return false;
      }
      Asterisk o = (Asterisk) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ReturnItems other) {
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

  public static final class ItemList extends openGql.grammar.ReturnItems implements Serializable {
    public final java.util.List<openGql.grammar.ReturnItem> value;

    public ItemList (java.util.List<openGql.grammar.ReturnItem> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ItemList)) {
        return false;
      }
      ItemList o = (ItemList) other;
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
    public int compareTo(ReturnItems other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ItemList o = (ItemList) other;
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
