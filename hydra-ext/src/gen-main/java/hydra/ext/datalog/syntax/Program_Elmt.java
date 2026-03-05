// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public abstract class Program_Elmt implements Serializable, Comparable<Program_Elmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.datalog.syntax.Program_Elmt");
  
  public static final hydra.core.Name FACT = new hydra.core.Name("Fact");
  
  public static final hydra.core.Name RULE = new hydra.core.Name("Rule");
  
  private Program_Elmt () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Fact instance) ;
    
    R visit(Rule instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Program_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Fact instance) {
      return otherwise(instance);
    }
    
    default R visit(Rule instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Fact extends hydra.ext.datalog.syntax.Program_Elmt implements Serializable {
    public final hydra.ext.datalog.syntax.Fact value;
    
    public Fact (hydra.ext.datalog.syntax.Fact value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fact)) {
        return false;
      }
      Fact o = (Fact) other;
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
    public int compareTo(Program_Elmt other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fact o = (Fact) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Rule extends hydra.ext.datalog.syntax.Program_Elmt implements Serializable {
    public final hydra.ext.datalog.syntax.Rule value;
    
    public Rule (hydra.ext.datalog.syntax.Rule value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rule)) {
        return false;
      }
      Rule o = (Rule) other;
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
    public int compareTo(Program_Elmt other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Rule o = (Rule) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
