// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class ImportExportStat implements Serializable, Comparable<ImportExportStat> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.ImportExportStat");

  public static final hydra.core.Name IMPORT = new hydra.core.Name("import");

  public static final hydra.core.Name EXPORT = new hydra.core.Name("export");

  private ImportExportStat () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Import instance) ;

    R visit(Export instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImportExportStat instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Import instance) {
      return otherwise(instance);
    }

    default R visit(Export instance) {
      return otherwise(instance);
    }
  }

  public static final class Import extends hydra.ext.scala.syntax.ImportExportStat implements Serializable {
    public final hydra.ext.scala.syntax.Import value;

    public Import (hydra.ext.scala.syntax.Import value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Import)) {
        return false;
      }
      Import o = (Import) other;
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
    public int compareTo(ImportExportStat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Import o = (Import) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Export extends hydra.ext.scala.syntax.ImportExportStat implements Serializable {
    public final hydra.ext.scala.syntax.Export value;

    public Export (hydra.ext.scala.syntax.Export value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Export)) {
        return false;
      }
      Export o = (Export) other;
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
    public int compareTo(ImportExportStat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Export o = (Export) other;
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
