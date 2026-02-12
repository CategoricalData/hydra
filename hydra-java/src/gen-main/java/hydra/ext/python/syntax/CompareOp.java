// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class CompareOp implements Serializable, Comparable<CompareOp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CompareOp");
  
  public static final hydra.core.Name FIELD_NAME_EQ = new hydra.core.Name("eq");
  
  public static final hydra.core.Name FIELD_NAME_NOTEQ = new hydra.core.Name("noteq");
  
  public static final hydra.core.Name FIELD_NAME_LTE = new hydra.core.Name("lte");
  
  public static final hydra.core.Name FIELD_NAME_LT = new hydra.core.Name("lt");
  
  public static final hydra.core.Name FIELD_NAME_GTE = new hydra.core.Name("gte");
  
  public static final hydra.core.Name FIELD_NAME_GT = new hydra.core.Name("gt");
  
  public static final hydra.core.Name FIELD_NAME_NOTIN = new hydra.core.Name("notin");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_ISNOT = new hydra.core.Name("isnot");
  
  public static final hydra.core.Name FIELD_NAME_IS = new hydra.core.Name("is");
  
  private CompareOp () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Eq instance) ;
    
    R visit(Noteq instance) ;
    
    R visit(Lte instance) ;
    
    R visit(Lt instance) ;
    
    R visit(Gte instance) ;
    
    R visit(Gt instance) ;
    
    R visit(Notin instance) ;
    
    R visit(In instance) ;
    
    R visit(Isnot instance) ;
    
    R visit(Is instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CompareOp instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Eq instance) {
      return otherwise(instance);
    }
    
    default R visit(Noteq instance) {
      return otherwise(instance);
    }
    
    default R visit(Lte instance) {
      return otherwise(instance);
    }
    
    default R visit(Lt instance) {
      return otherwise(instance);
    }
    
    default R visit(Gte instance) {
      return otherwise(instance);
    }
    
    default R visit(Gt instance) {
      return otherwise(instance);
    }
    
    default R visit(Notin instance) {
      return otherwise(instance);
    }
    
    default R visit(In instance) {
      return otherwise(instance);
    }
    
    default R visit(Isnot instance) {
      return otherwise(instance);
    }
    
    default R visit(Is instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Eq extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Eq () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Eq)) {
        return false;
      }
      Eq o = (Eq) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Noteq extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Noteq () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Noteq)) {
        return false;
      }
      Noteq o = (Noteq) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Lte extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Lte () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lte)) {
        return false;
      }
      Lte o = (Lte) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Lt extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Lt () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lt)) {
        return false;
      }
      Lt o = (Lt) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Gte extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Gte () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gte)) {
        return false;
      }
      Gte o = (Gte) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Gt extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Gt () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gt)) {
        return false;
      }
      Gt o = (Gt) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Notin extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Notin () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Notin)) {
        return false;
      }
      Notin o = (Notin) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class In extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public In () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Isnot extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Isnot () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Isnot)) {
        return false;
      }
      Isnot o = (Isnot) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Is extends hydra.ext.python.syntax.CompareOp implements Serializable {
    public Is () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Is)) {
        return false;
      }
      Is o = (Is) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompareOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
