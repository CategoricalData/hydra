package hydra.ext.haskell.ast;

public abstract class ImportExportSpec_Subspec {
  private ImportExportSpec_Subspec () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(All instance) ;
    
    R visit(List instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImportExportSpec_Subspec instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(All instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
  }
  
  public static final class All extends ImportExportSpec_Subspec {
    public All () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class List extends ImportExportSpec_Subspec {
    public final java.util.List<Name> value;
    
    public List (java.util.List<Name> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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