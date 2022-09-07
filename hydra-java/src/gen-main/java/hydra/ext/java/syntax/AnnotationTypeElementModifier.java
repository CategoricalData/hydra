package hydra.ext.java.syntax;

public abstract class AnnotationTypeElementModifier {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.AnnotationTypeElementModifier");
  
  private AnnotationTypeElementModifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Public instance) ;
    
    R visit(Abstract instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotationTypeElementModifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Public instance) {
      return otherwise((instance));
    }
    
    default R visit(Abstract instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Public extends hydra.ext.java.syntax.AnnotationTypeElementModifier {
    public final hydra.ext.java.syntax.Annotation value;
    
    public Public (hydra.ext.java.syntax.Annotation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Public)) {
        return false;
      }
      Public o = (Public) (other);
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
  
  public static final class Abstract extends hydra.ext.java.syntax.AnnotationTypeElementModifier {
    public Abstract () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Abstract)) {
        return false;
      }
      Abstract o = (Abstract) (other);
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
}