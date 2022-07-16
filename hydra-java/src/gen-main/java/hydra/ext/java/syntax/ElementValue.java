package hydra.ext.java.syntax;

public abstract class ElementValue {
  private ElementValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ConditionalExpression instance) ;
    
    R visit(ElementValueArrayInitializer instance) ;
    
    R visit(Annotation instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ElementValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ConditionalExpression instance) {
      return otherwise((instance));
    }
    
    default R visit(ElementValueArrayInitializer instance) {
      return otherwise((instance));
    }
    
    default R visit(Annotation instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ConditionalExpression extends ElementValue {
    public final ConditionalExpression value;
    
    public ConditionalExpression (ConditionalExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConditionalExpression)) {
        return false;
      }
      ConditionalExpression o = (ConditionalExpression) (other);
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
  
  public static final class ElementValueArrayInitializer extends ElementValue {
    public final ElementValueArrayInitializer value;
    
    public ElementValueArrayInitializer (ElementValueArrayInitializer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementValueArrayInitializer)) {
        return false;
      }
      ElementValueArrayInitializer o = (ElementValueArrayInitializer) (other);
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
  
  public static final class Annotation extends ElementValue {
    public final Annotation value;
    
    public Annotation (Annotation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotation)) {
        return false;
      }
      Annotation o = (Annotation) (other);
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