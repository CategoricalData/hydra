// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class Statement implements Serializable, Comparable<Statement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Statement");
  
  public static final hydra.core.Name FIELD_NAME_COMPOUND = new hydra.core.Name("compound");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATED = new hydra.core.Name("annotated");
  
  private Statement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Compound instance) ;
    
    R visit(Simple instance) ;
    
    R visit(Annotated instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Statement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Compound instance) {
      return otherwise(instance);
    }
    
    default R visit(Simple instance) {
      return otherwise(instance);
    }
    
    default R visit(Annotated instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Compound extends hydra.ext.python.syntax.Statement implements Serializable {
    public final hydra.ext.python.syntax.CompoundStatement value;
    
    public Compound (hydra.ext.python.syntax.CompoundStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Compound)) {
        return false;
      }
      Compound o = (Compound) other;
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
    public int compareTo(Statement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Compound o = (Compound) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Simple extends hydra.ext.python.syntax.Statement implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.SimpleStatement> value;
    
    public Simple (java.util.List<hydra.ext.python.syntax.SimpleStatement> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(Statement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Annotated extends hydra.ext.python.syntax.Statement implements Serializable {
    public final hydra.ext.python.syntax.AnnotatedStatement value;
    
    public Annotated (hydra.ext.python.syntax.AnnotatedStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotated)) {
        return false;
      }
      Annotated o = (Annotated) other;
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
    public int compareTo(Statement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Annotated o = (Annotated) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
