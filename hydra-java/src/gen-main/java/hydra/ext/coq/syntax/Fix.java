package hydra.ext.coq.syntax;

public abstract class Fix {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Fix");
  
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
  
  public static final class Decl extends hydra.ext.coq.syntax.Fix {
    public final hydra.ext.coq.syntax.Fix_Decl value;
    
    public Decl (hydra.ext.coq.syntax.Fix_Decl value) {
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
  
  public static final class Qual extends hydra.ext.coq.syntax.Fix {
    public final java.util.Optional<hydra.ext.coq.syntax.Fix_Qual> value;
    
    public Qual (java.util.Optional<hydra.ext.coq.syntax.Fix_Qual> value) {
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