// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class BindingTableName implements Serializable, Comparable<BindingTableName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingTableName");

  public static final hydra.core.Name REGULAR_IDENTIFIER = new hydra.core.Name("regularIdentifier");

  public static final hydra.core.Name DELIMITED_BINDING_TABLE_NAME = new hydra.core.Name("delimitedBindingTableName");

  private BindingTableName () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(RegularIdentifier instance) ;

    R visit(DelimitedBindingTableName instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BindingTableName instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(RegularIdentifier instance) {
      return otherwise(instance);
    }

    default R visit(DelimitedBindingTableName instance) {
      return otherwise(instance);
    }
  }

  public static final class RegularIdentifier extends openGql.grammar.BindingTableName implements Serializable {
    public final String value;

    public RegularIdentifier (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RegularIdentifier)) {
        return false;
      }
      RegularIdentifier o = (RegularIdentifier) other;
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
    public int compareTo(BindingTableName other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RegularIdentifier o = (RegularIdentifier) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DelimitedBindingTableName extends openGql.grammar.BindingTableName implements Serializable {
    public final String value;

    public DelimitedBindingTableName (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DelimitedBindingTableName)) {
        return false;
      }
      DelimitedBindingTableName o = (DelimitedBindingTableName) other;
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
    public int compareTo(BindingTableName other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DelimitedBindingTableName o = (DelimitedBindingTableName) other;
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
