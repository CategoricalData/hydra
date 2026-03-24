// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Stat implements Serializable, Comparable<Stat> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Stat");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name DECL = new hydra.core.Name("decl");

  public static final hydra.core.Name DEFN = new hydra.core.Name("defn");

  public static final hydra.core.Name IMPORT_EXPORT = new hydra.core.Name("importExport");

  private Stat () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Term instance) ;

    R visit(Decl instance) ;

    R visit(Defn instance) ;

    R visit(ImportExport instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Stat instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }

    default R visit(Decl instance) {
      return otherwise(instance);
    }

    default R visit(Defn instance) {
      return otherwise(instance);
    }

    default R visit(ImportExport instance) {
      return otherwise(instance);
    }
  }

  public static final class Term extends hydra.ext.scala.syntax.Stat implements Serializable {
    public final hydra.ext.scala.syntax.Data value;

    public Term (hydra.ext.scala.syntax.Data value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) other;
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
    public int compareTo(Stat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term o = (Term) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Decl extends hydra.ext.scala.syntax.Stat implements Serializable {
    public final hydra.ext.scala.syntax.Decl value;

    public Decl (hydra.ext.scala.syntax.Decl value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decl)) {
        return false;
      }
      Decl o = (Decl) other;
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
    public int compareTo(Stat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Decl o = (Decl) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Defn extends hydra.ext.scala.syntax.Stat implements Serializable {
    public final hydra.ext.scala.syntax.Defn value;

    public Defn (hydra.ext.scala.syntax.Defn value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Defn)) {
        return false;
      }
      Defn o = (Defn) other;
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
    public int compareTo(Stat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Defn o = (Defn) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ImportExport extends hydra.ext.scala.syntax.Stat implements Serializable {
    public final hydra.ext.scala.syntax.ImportExportStat value;

    public ImportExport (hydra.ext.scala.syntax.ImportExportStat value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImportExport)) {
        return false;
      }
      ImportExport o = (ImportExport) other;
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
    public int compareTo(Stat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ImportExport o = (ImportExport) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
