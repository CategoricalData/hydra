// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ElementValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ElementValue");
  
  public static final hydra.core.Name FIELD_NAME_CONDITIONAL_EXPRESSION = new hydra.core.Name("conditionalExpression");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_VALUE_ARRAY_INITIALIZER = new hydra.core.Name("elementValueArrayInitializer");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
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
  
  public static final class ConditionalExpression extends hydra.ext.java.syntax.ElementValue implements Serializable {
    public final hydra.ext.java.syntax.ConditionalExpression value;
    
    public ConditionalExpression (hydra.ext.java.syntax.ConditionalExpression value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class ElementValueArrayInitializer extends hydra.ext.java.syntax.ElementValue implements Serializable {
    public final hydra.ext.java.syntax.ElementValueArrayInitializer value;
    
    public ElementValueArrayInitializer (hydra.ext.java.syntax.ElementValueArrayInitializer value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Annotation extends hydra.ext.java.syntax.ElementValue implements Serializable {
    public final hydra.ext.java.syntax.Annotation value;
    
    public Annotation (hydra.ext.java.syntax.Annotation value) {
      java.util.Objects.requireNonNull((value));
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