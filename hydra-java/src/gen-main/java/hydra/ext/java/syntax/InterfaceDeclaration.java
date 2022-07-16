package hydra.ext.java.syntax;

public abstract class InterfaceDeclaration {
  private InterfaceDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NormalInterface instance) ;
    
    R visit(AnnotationType instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InterfaceDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NormalInterface instance) {
      return otherwise((instance));
    }
    
    default R visit(AnnotationType instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NormalInterface extends InterfaceDeclaration {
    public final NormalInterfaceDeclaration value;
    
    public NormalInterface (NormalInterfaceDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NormalInterface)) {
        return false;
      }
      NormalInterface o = (NormalInterface) (other);
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
  
  public static final class AnnotationType extends InterfaceDeclaration {
    public final AnnotationTypeDeclaration value;
    
    public AnnotationType (AnnotationTypeDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationType)) {
        return false;
      }
      AnnotationType o = (AnnotationType) (other);
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