// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class StarEtc implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarEtc");
  
  public static final hydra.core.Name FIELD_NAME_STAR_NO_DEFAULT = new hydra.core.Name("starNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_NO_DEFAULT_STAR_ANNOTATION = new hydra.core.Name("starNoDefaultStarAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_STAR_COMMA = new hydra.core.Name("starComma");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORDS = new hydra.core.Name("keywords");
  
  private StarEtc () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(StarNoDefault instance) ;
    
    R visit(StarNoDefaultStarAnnotation instance) ;
    
    R visit(StarComma instance) ;
    
    R visit(Keywords instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StarEtc instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(StarNoDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(StarNoDefaultStarAnnotation instance) {
      return otherwise((instance));
    }
    
    default R visit(StarComma instance) {
      return otherwise((instance));
    }
    
    default R visit(Keywords instance) {
      return otherwise((instance));
    }
  }
  
  public static final class StarNoDefault extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.NoDefaultStarEtc value;
    
    public StarNoDefault (hydra.ext.python.syntax.NoDefaultStarEtc value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarNoDefault)) {
        return false;
      }
      StarNoDefault o = (StarNoDefault) (other);
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
  
  public static final class StarNoDefaultStarAnnotation extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc value;
    
    public StarNoDefaultStarAnnotation (hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarNoDefaultStarAnnotation)) {
        return false;
      }
      StarNoDefaultStarAnnotation o = (StarNoDefaultStarAnnotation) (other);
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
  
  public static final class StarComma extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.CommaStarEtc value;
    
    public StarComma (hydra.ext.python.syntax.CommaStarEtc value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarComma)) {
        return false;
      }
      StarComma o = (StarComma) (other);
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
  
  public static final class Keywords extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.Keywords value;
    
    public Keywords (hydra.ext.python.syntax.Keywords value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keywords)) {
        return false;
      }
      Keywords o = (Keywords) (other);
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