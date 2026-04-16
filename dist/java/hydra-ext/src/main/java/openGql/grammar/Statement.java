// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class Statement implements Serializable, Comparable<Statement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Statement");

  public static final hydra.core.Name LINEAR_CATALOG_MODIFYING = new hydra.core.Name("linearCatalogModifying");

  public static final hydra.core.Name LINEAR_DATA_MODIFYING = new hydra.core.Name("linearDataModifying");

  public static final hydra.core.Name COMPOSITE_QUERY = new hydra.core.Name("compositeQuery");

  private Statement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LinearCatalogModifying instance) ;

    R visit(LinearDataModifying instance) ;

    R visit(CompositeQuery instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Statement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LinearCatalogModifying instance) {
      return otherwise(instance);
    }

    default R visit(LinearDataModifying instance) {
      return otherwise(instance);
    }

    default R visit(CompositeQuery instance) {
      return otherwise(instance);
    }
  }

  public static final class LinearCatalogModifying extends openGql.grammar.Statement implements Serializable {
    public final java.util.List<openGql.grammar.SimpleCatalogModifyingStatement> value;

    public LinearCatalogModifying (java.util.List<openGql.grammar.SimpleCatalogModifyingStatement> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LinearCatalogModifying)) {
        return false;
      }
      LinearCatalogModifying o = (LinearCatalogModifying) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LinearCatalogModifying o = (LinearCatalogModifying) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LinearDataModifying extends openGql.grammar.Statement implements Serializable {
    public final openGql.grammar.LinearDataModifyingStatement value;

    public LinearDataModifying (openGql.grammar.LinearDataModifyingStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LinearDataModifying)) {
        return false;
      }
      LinearDataModifying o = (LinearDataModifying) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LinearDataModifying o = (LinearDataModifying) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CompositeQuery extends openGql.grammar.Statement implements Serializable {
    public final openGql.grammar.CompositeQueryExpression value;

    public CompositeQuery (openGql.grammar.CompositeQueryExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CompositeQuery)) {
        return false;
      }
      CompositeQuery o = (CompositeQuery) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CompositeQuery o = (CompositeQuery) other;
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
