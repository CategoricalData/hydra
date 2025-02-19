// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class ImportExportStat implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.ImportExportStat");
  
  public static final hydra.core.Name FIELD_NAME_IMPORT = new hydra.core.Name("import");
  
  public static final hydra.core.Name FIELD_NAME_EXPORT = new hydra.core.Name("export");
  
  private ImportExportStat () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Import instance) ;
    
    R visit(Export instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImportExportStat instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Import instance) {
      return otherwise((instance));
    }
    
    default R visit(Export instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Import extends hydra.ext.scala.meta.ImportExportStat implements Serializable {
    public final hydra.ext.scala.meta.Import value;
    
    public Import (hydra.ext.scala.meta.Import value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Import)) {
        return false;
      }
      Import o = (Import) (other);
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
  
  public static final class Export extends hydra.ext.scala.meta.ImportExportStat implements Serializable {
    public final hydra.ext.scala.meta.Export value;
    
    public Export (hydra.ext.scala.meta.Export value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Export)) {
        return false;
      }
      Export o = (Export) (other);
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