// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class RelativeCatalogSchemaReference implements Serializable, Comparable<RelativeCatalogSchemaReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.RelativeCatalogSchemaReference");

  public static final hydra.core.Name PREDEFINED_REFERENCE = new hydra.core.Name("predefinedReference");

  public static final hydra.core.Name DIRECTORY_AND_SCHEMA = new hydra.core.Name("directoryAndSchema");

  private RelativeCatalogSchemaReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PredefinedReference instance) ;

    R visit(DirectoryAndSchema instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelativeCatalogSchemaReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PredefinedReference instance) {
      return otherwise(instance);
    }

    default R visit(DirectoryAndSchema instance) {
      return otherwise(instance);
    }
  }

  public static final class PredefinedReference extends openGql.grammar.RelativeCatalogSchemaReference implements Serializable {
    public final openGql.grammar.PredefinedSchemaReference value;

    public PredefinedReference (openGql.grammar.PredefinedSchemaReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PredefinedReference)) {
        return false;
      }
      PredefinedReference o = (PredefinedReference) other;
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
    public int compareTo(RelativeCatalogSchemaReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PredefinedReference o = (PredefinedReference) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DirectoryAndSchema extends openGql.grammar.RelativeCatalogSchemaReference implements Serializable {
    public final openGql.grammar.RelativeDirectoryAndSchema value;

    public DirectoryAndSchema (openGql.grammar.RelativeDirectoryAndSchema value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DirectoryAndSchema)) {
        return false;
      }
      DirectoryAndSchema o = (DirectoryAndSchema) other;
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
    public int compareTo(RelativeCatalogSchemaReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DirectoryAndSchema o = (DirectoryAndSchema) other;
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
