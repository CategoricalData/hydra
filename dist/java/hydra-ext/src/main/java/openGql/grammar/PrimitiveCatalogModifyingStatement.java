// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PrimitiveCatalogModifyingStatement implements Serializable, Comparable<PrimitiveCatalogModifyingStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PrimitiveCatalogModifyingStatement");

  public static final hydra.core.Name CREATE_SCHEMA = new hydra.core.Name("createSchema");

  public static final hydra.core.Name DROP_SCHEMA = new hydra.core.Name("dropSchema");

  public static final hydra.core.Name CREATE_GRAPH = new hydra.core.Name("createGraph");

  public static final hydra.core.Name DROP_GRAPH = new hydra.core.Name("dropGraph");

  public static final hydra.core.Name CREATE_GRAPH_TYPE = new hydra.core.Name("createGraphType");

  public static final hydra.core.Name DROP_GRAPH_TYPE = new hydra.core.Name("dropGraphType");

  private PrimitiveCatalogModifyingStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(CreateSchema instance) ;

    R visit(DropSchema instance) ;

    R visit(CreateGraph instance) ;

    R visit(DropGraph instance) ;

    R visit(CreateGraphType instance) ;

    R visit(DropGraphType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimitiveCatalogModifyingStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(CreateSchema instance) {
      return otherwise(instance);
    }

    default R visit(DropSchema instance) {
      return otherwise(instance);
    }

    default R visit(CreateGraph instance) {
      return otherwise(instance);
    }

    default R visit(DropGraph instance) {
      return otherwise(instance);
    }

    default R visit(CreateGraphType instance) {
      return otherwise(instance);
    }

    default R visit(DropGraphType instance) {
      return otherwise(instance);
    }
  }

  public static final class CreateSchema extends openGql.grammar.PrimitiveCatalogModifyingStatement implements Serializable {
    public final openGql.grammar.CreateSchemaStatement value;

    public CreateSchema (openGql.grammar.CreateSchemaStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CreateSchema)) {
        return false;
      }
      CreateSchema o = (CreateSchema) other;
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
    public int compareTo(PrimitiveCatalogModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CreateSchema o = (CreateSchema) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DropSchema extends openGql.grammar.PrimitiveCatalogModifyingStatement implements Serializable {
    public final openGql.grammar.DropSchemaStatement value;

    public DropSchema (openGql.grammar.DropSchemaStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DropSchema)) {
        return false;
      }
      DropSchema o = (DropSchema) other;
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
    public int compareTo(PrimitiveCatalogModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DropSchema o = (DropSchema) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CreateGraph extends openGql.grammar.PrimitiveCatalogModifyingStatement implements Serializable {
    public final openGql.grammar.CreateGraphStatement value;

    public CreateGraph (openGql.grammar.CreateGraphStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CreateGraph)) {
        return false;
      }
      CreateGraph o = (CreateGraph) other;
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
    public int compareTo(PrimitiveCatalogModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CreateGraph o = (CreateGraph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DropGraph extends openGql.grammar.PrimitiveCatalogModifyingStatement implements Serializable {
    public final openGql.grammar.DropGraphStatement value;

    public DropGraph (openGql.grammar.DropGraphStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DropGraph)) {
        return false;
      }
      DropGraph o = (DropGraph) other;
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
    public int compareTo(PrimitiveCatalogModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DropGraph o = (DropGraph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CreateGraphType extends openGql.grammar.PrimitiveCatalogModifyingStatement implements Serializable {
    public final openGql.grammar.CreateGraphTypeStatement value;

    public CreateGraphType (openGql.grammar.CreateGraphTypeStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CreateGraphType)) {
        return false;
      }
      CreateGraphType o = (CreateGraphType) other;
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
    public int compareTo(PrimitiveCatalogModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CreateGraphType o = (CreateGraphType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DropGraphType extends openGql.grammar.PrimitiveCatalogModifyingStatement implements Serializable {
    public final openGql.grammar.DropGraphTypeStatement value;

    public DropGraphType (openGql.grammar.DropGraphTypeStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DropGraphType)) {
        return false;
      }
      DropGraphType o = (DropGraphType) other;
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
    public int compareTo(PrimitiveCatalogModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DropGraphType o = (DropGraphType) other;
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
