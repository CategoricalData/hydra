package hydra.ext.scala.meta;

public abstract class ImportExportStat {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.ImportExportStat");
  
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
  
  public static final class Import extends hydra.ext.scala.meta.ImportExportStat {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Import");
    
    public final hydra.ext.scala.meta.Import value;
    
    public Import (hydra.ext.scala.meta.Import value) {
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
  
  public static final class Export extends hydra.ext.scala.meta.ImportExportStat {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Export");
    
    public final hydra.ext.scala.meta.Export value;
    
    public Export (hydra.ext.scala.meta.Export value) {
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