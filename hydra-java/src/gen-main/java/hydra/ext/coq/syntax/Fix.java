package hydra.ext.coq.syntax;

public abstract class Fix {
  private Fix () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Decl instance) ;
    
    R visit(Qual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Fix instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Decl instance) {
      return otherwise((instance));
    }
    
    default R visit(Qual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Decl extends Fix {
    public final Fix_Decl value;
    
    public Decl (Fix_Decl value) {
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
  
  public static final class Qual extends Fix {
    public final java.util.Optional<Fix_Qual> value;
    
    public Qual (java.util.Optional<Fix_Qual> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qual)) {
        return false;
      }
      Qual o = (Qual) (other);
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