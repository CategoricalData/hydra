// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ModuleDirective implements Serializable, Comparable<ModuleDirective> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModuleDirective");
  
  public static final hydra.core.Name FIELD_NAME_REQUIRES = new hydra.core.Name("requires");
  
  public static final hydra.core.Name FIELD_NAME_EXPORTS = new hydra.core.Name("exports");
  
  public static final hydra.core.Name FIELD_NAME_OPENS = new hydra.core.Name("opens");
  
  public static final hydra.core.Name FIELD_NAME_USES = new hydra.core.Name("uses");
  
  public static final hydra.core.Name FIELD_NAME_PROVIDES = new hydra.core.Name("provides");
  
  private ModuleDirective () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Requires instance) ;
    
    R visit(Exports instance) ;
    
    R visit(Opens instance) ;
    
    R visit(Uses instance) ;
    
    R visit(Provides instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ModuleDirective instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Requires instance) {
      return otherwise(instance);
    }
    
    default R visit(Exports instance) {
      return otherwise(instance);
    }
    
    default R visit(Opens instance) {
      return otherwise(instance);
    }
    
    default R visit(Uses instance) {
      return otherwise(instance);
    }
    
    default R visit(Provides instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Requires extends hydra.ext.java.syntax.ModuleDirective implements Serializable {
    public final hydra.ext.java.syntax.ModuleDirective_Requires value;
    
    public Requires (hydra.ext.java.syntax.ModuleDirective_Requires value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Requires)) {
        return false;
      }
      Requires o = (Requires) other;
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
    public int compareTo(ModuleDirective other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Requires o = (Requires) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Exports extends hydra.ext.java.syntax.ModuleDirective implements Serializable {
    public final hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value;
    
    public Exports (hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Exports)) {
        return false;
      }
      Exports o = (Exports) other;
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
    public int compareTo(ModuleDirective other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Exports o = (Exports) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Opens extends hydra.ext.java.syntax.ModuleDirective implements Serializable {
    public final hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value;
    
    public Opens (hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Opens)) {
        return false;
      }
      Opens o = (Opens) other;
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
    public int compareTo(ModuleDirective other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Opens o = (Opens) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Uses extends hydra.ext.java.syntax.ModuleDirective implements Serializable {
    public final hydra.ext.java.syntax.TypeName value;
    
    public Uses (hydra.ext.java.syntax.TypeName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uses)) {
        return false;
      }
      Uses o = (Uses) other;
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
    public int compareTo(ModuleDirective other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uses o = (Uses) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Provides extends hydra.ext.java.syntax.ModuleDirective implements Serializable {
    public final hydra.ext.java.syntax.ModuleDirective_Provides value;
    
    public Provides (hydra.ext.java.syntax.ModuleDirective_Provides value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Provides)) {
        return false;
      }
      Provides o = (Provides) other;
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
    public int compareTo(ModuleDirective other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Provides o = (Provides) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
