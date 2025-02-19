// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class InterfaceDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.InterfaceDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_NORMAL_INTERFACE = new hydra.core.Name("normalInterface");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION_TYPE = new hydra.core.Name("annotationType");
  
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
  
  public static final class NormalInterface extends hydra.ext.java.syntax.InterfaceDeclaration implements Serializable {
    public final hydra.ext.java.syntax.NormalInterfaceDeclaration value;
    
    public NormalInterface (hydra.ext.java.syntax.NormalInterfaceDeclaration value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class AnnotationType extends hydra.ext.java.syntax.InterfaceDeclaration implements Serializable {
    public final hydra.ext.java.syntax.AnnotationTypeDeclaration value;
    
    public AnnotationType (hydra.ext.java.syntax.AnnotationTypeDeclaration value) {
      java.util.Objects.requireNonNull((value));
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