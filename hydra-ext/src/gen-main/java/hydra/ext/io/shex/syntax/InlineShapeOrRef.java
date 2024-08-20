// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class InlineShapeOrRef implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.InlineShapeOrRef");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_SHAPE_DEFINITION = new hydra.core.Name("inlineShapeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_ATP_NAME_LN = new hydra.core.Name("atpNameLn");
  
  public static final hydra.core.Name FIELD_NAME_ATP_NAME_NS = new hydra.core.Name("atpNameNs");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  private InlineShapeOrRef () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(InlineShapeDefinition instance) ;
    
    R visit(AtpNameLn instance) ;
    
    R visit(AtpNameNs instance) ;
    
    R visit(Sequence instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InlineShapeOrRef instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(InlineShapeDefinition instance) {
      return otherwise((instance));
    }
    
    default R visit(AtpNameLn instance) {
      return otherwise((instance));
    }
    
    default R visit(AtpNameNs instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
  }
  
  public static final class InlineShapeDefinition extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.InlineShapeDefinition value;
    
    public InlineShapeDefinition (hydra.ext.io.shex.syntax.InlineShapeDefinition value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InlineShapeDefinition)) {
        return false;
      }
      InlineShapeDefinition o = (InlineShapeDefinition) (other);
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
  
  public static final class AtpNameLn extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.AtpNameLn value;
    
    public AtpNameLn (hydra.ext.io.shex.syntax.AtpNameLn value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AtpNameLn)) {
        return false;
      }
      AtpNameLn o = (AtpNameLn) (other);
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
  
  public static final class AtpNameNs extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.AtpNameNs value;
    
    public AtpNameNs (hydra.ext.io.shex.syntax.AtpNameNs value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AtpNameNs)) {
        return false;
      }
      AtpNameNs o = (AtpNameNs) (other);
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
  
  public static final class Sequence extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.ShapeExprLabel value;
    
    public Sequence (hydra.ext.io.shex.syntax.ShapeExprLabel value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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