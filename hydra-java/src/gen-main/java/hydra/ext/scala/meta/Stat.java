package hydra.ext.scala.meta;

public abstract class Stat {
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
  
  public static final class Term extends hydra.ext.scala.meta.Stat {
    public final hydra.ext.scala.meta.Data value;
    
    public Term (hydra.ext.scala.meta.Data value) {
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
  
  public static final class Decl extends hydra.ext.scala.meta.Stat {
    public final hydra.ext.scala.meta.Decl value;
    
    public Decl (hydra.ext.scala.meta.Decl value) {
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
  
  public static final class Defn extends hydra.ext.scala.meta.Stat {
    public final hydra.ext.scala.meta.Defn value;
    
    public Defn (hydra.ext.scala.meta.Defn value) {
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
  
  public static final class ImportExport extends hydra.ext.scala.meta.Stat {
    public final hydra.ext.scala.meta.ImportExportStat value;
    
    public ImportExport (hydra.ext.scala.meta.ImportExportStat value) {
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