// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class AbsoluteCatalogSchemaReference implements Serializable, Comparable<AbsoluteCatalogSchemaReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AbsoluteCatalogSchemaReference");

  public static final hydra.core.Name ROOT = new hydra.core.Name("root");

  public static final hydra.core.Name DIRECTORY_AND_SCHEMA = new hydra.core.Name("directoryAndSchema");

  private AbsoluteCatalogSchemaReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Root instance) ;

    R visit(DirectoryAndSchema instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AbsoluteCatalogSchemaReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Root instance) {
      return otherwise(instance);
    }

    default R visit(DirectoryAndSchema instance) {
      return otherwise(instance);
    }
  }

  public static final class Root extends openGql.grammar.AbsoluteCatalogSchemaReference implements Serializable {
    public Root () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Root)) {
        return false;
      }
      Root o = (Root) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbsoluteCatalogSchemaReference other) {
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

  public static final class DirectoryAndSchema extends openGql.grammar.AbsoluteCatalogSchemaReference implements Serializable {
    public final openGql.grammar.AbsoluteDirectoryAndSchema value;

    public DirectoryAndSchema (openGql.grammar.AbsoluteDirectoryAndSchema value) {
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
    public int compareTo(AbsoluteCatalogSchemaReference other) {
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
