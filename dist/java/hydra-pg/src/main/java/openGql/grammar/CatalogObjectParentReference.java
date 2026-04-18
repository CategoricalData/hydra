// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CatalogObjectParentReference implements Serializable, Comparable<CatalogObjectParentReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CatalogObjectParentReference");

  public static final hydra.core.Name SCHEMA_AND_OBJECTS = new hydra.core.Name("schemaAndObjects");

  public static final hydra.core.Name OBJECTS_ONLY = new hydra.core.Name("objectsOnly");

  private CatalogObjectParentReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(SchemaAndObjects instance) ;

    R visit(ObjectsOnly instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CatalogObjectParentReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(SchemaAndObjects instance) {
      return otherwise(instance);
    }

    default R visit(ObjectsOnly instance) {
      return otherwise(instance);
    }
  }

  public static final class SchemaAndObjects extends openGql.grammar.CatalogObjectParentReference implements Serializable {
    public final openGql.grammar.SchemaAndObjects value;

    public SchemaAndObjects (openGql.grammar.SchemaAndObjects value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SchemaAndObjects)) {
        return false;
      }
      SchemaAndObjects o = (SchemaAndObjects) other;
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
    public int compareTo(CatalogObjectParentReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SchemaAndObjects o = (SchemaAndObjects) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ObjectsOnly extends openGql.grammar.CatalogObjectParentReference implements Serializable {
    public final java.util.List<String> value;

    public ObjectsOnly (java.util.List<String> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectsOnly)) {
        return false;
      }
      ObjectsOnly o = (ObjectsOnly) other;
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
    public int compareTo(CatalogObjectParentReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectsOnly o = (ObjectsOnly) other;
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
