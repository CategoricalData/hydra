// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class ExistentialSubquery implements Serializable, Comparable<ExistentialSubquery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ExistentialSubquery");
  
  public static final hydra.core.Name REGULAR = new hydra.core.Name("regular");
  
  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");
  
  private ExistentialSubquery () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Regular instance) ;
    
    R visit(Pattern instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExistentialSubquery instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Regular instance) {
      return otherwise(instance);
    }
    
    default R visit(Pattern instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Regular extends hydra.ext.cypher.openCypher.ExistentialSubquery implements Serializable {
    public final hydra.ext.cypher.openCypher.RegularQuery value;
    
    public Regular (hydra.ext.cypher.openCypher.RegularQuery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regular)) {
        return false;
      }
      Regular o = (Regular) other;
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
    public int compareTo(ExistentialSubquery other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Regular o = (Regular) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Pattern extends hydra.ext.cypher.openCypher.ExistentialSubquery implements Serializable {
    public final hydra.ext.cypher.openCypher.PatternWhere value;
    
    public Pattern (hydra.ext.cypher.openCypher.PatternWhere value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) other;
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
    public int compareTo(ExistentialSubquery other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pattern o = (Pattern) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
