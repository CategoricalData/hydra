// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class StarEtc implements Serializable, Comparable<StarEtc> {
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(StarNoDefault instance) {
      return otherwise(instance);
    }
    
    default R visit(StarNoDefaultStarAnnotation instance) {
      return otherwise(instance);
    }
    
    default R visit(StarComma instance) {
      return otherwise(instance);
    }
    
    default R visit(Keywords instance) {
      return otherwise(instance);
    }
  }
  
  public static final class StarNoDefault extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.NoDefaultStarEtc value;
    
    public StarNoDefault (hydra.ext.python.syntax.NoDefaultStarEtc value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarNoDefault)) {
        return false;
      }
      StarNoDefault o = (StarNoDefault) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StarEtc other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StarNoDefault o = (StarNoDefault) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StarNoDefaultStarAnnotation extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc value;
    
    public StarNoDefaultStarAnnotation (hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarNoDefaultStarAnnotation)) {
        return false;
      }
      StarNoDefaultStarAnnotation o = (StarNoDefaultStarAnnotation) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StarEtc other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StarNoDefaultStarAnnotation o = (StarNoDefaultStarAnnotation) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StarComma extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.CommaStarEtc value;
    
    public StarComma (hydra.ext.python.syntax.CommaStarEtc value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarComma)) {
        return false;
      }
      StarComma o = (StarComma) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StarEtc other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StarComma o = (StarComma) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Keywords extends hydra.ext.python.syntax.StarEtc implements Serializable {
    public final hydra.ext.python.syntax.Keywords value;
    
    public Keywords (hydra.ext.python.syntax.Keywords value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keywords)) {
        return false;
      }
      Keywords o = (Keywords) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StarEtc other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Keywords o = (Keywords) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
