// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Stat implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Stat");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_DECL = new hydra.core.Name("decl");
  
  public static final hydra.core.Name FIELD_NAME_DEFN = new hydra.core.Name("defn");
  
  public static final hydra.core.Name FIELD_NAME_IMPORT_EXPORT = new hydra.core.Name("importExport");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
    
    default R visit(Decl instance) {
      return otherwise((instance));
    }
    
    default R visit(Defn instance) {
      return otherwise((instance));
    }
    
    default R visit(ImportExport instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Term extends hydra.ext.scala.meta.Stat implements Serializable {
    public final hydra.ext.scala.meta.Data value;
    
    public Term (hydra.ext.scala.meta.Data value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Decl extends hydra.ext.scala.meta.Stat implements Serializable {
    public final hydra.ext.scala.meta.Decl value;
    
    public Decl (hydra.ext.scala.meta.Decl value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decl)) {
        return false;
      }
      Decl o = (Decl) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Defn extends hydra.ext.scala.meta.Stat implements Serializable {
    public final hydra.ext.scala.meta.Defn value;
    
    public Defn (hydra.ext.scala.meta.Defn value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Defn)) {
        return false;
      }
      Defn o = (Defn) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ImportExport extends hydra.ext.scala.meta.Stat implements Serializable {
    public final hydra.ext.scala.meta.ImportExportStat value;
    
    public ImportExport (hydra.ext.scala.meta.ImportExportStat value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImportExport)) {
        return false;
      }
      ImportExport o = (ImportExport) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
