// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class StatementNoShortIf implements Serializable, Comparable<StatementNoShortIf> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.StatementNoShortIf");
  
  public static final hydra.core.Name FIELD_NAME_WITHOUT_TRAILING = new hydra.core.Name("withoutTrailing");
  
  public static final hydra.core.Name FIELD_NAME_LABELED = new hydra.core.Name("labeled");
  
  public static final hydra.core.Name FIELD_NAME_IF_THEN_ELSE = new hydra.core.Name("ifThenElse");
  
  public static final hydra.core.Name FIELD_NAME_WHILE = new hydra.core.Name("while");
  
  public static final hydra.core.Name FIELD_NAME_FOR = new hydra.core.Name("for");
  
  private StatementNoShortIf () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(WithoutTrailing instance) ;
    
    R visit(Labeled instance) ;
    
    R visit(IfThenElse instance) ;
    
    R visit(While instance) ;
    
    R visit(For instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StatementNoShortIf instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(WithoutTrailing instance) {
      return otherwise(instance);
    }
    
    default R visit(Labeled instance) {
      return otherwise(instance);
    }
    
    default R visit(IfThenElse instance) {
      return otherwise(instance);
    }
    
    default R visit(While instance) {
      return otherwise(instance);
    }
    
    default R visit(For instance) {
      return otherwise(instance);
    }
  }
  
  public static final class WithoutTrailing extends hydra.ext.java.syntax.StatementNoShortIf implements Serializable {
    public final hydra.ext.java.syntax.StatementWithoutTrailingSubstatement value;
    
    public WithoutTrailing (hydra.ext.java.syntax.StatementWithoutTrailingSubstatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithoutTrailing)) {
        return false;
      }
      WithoutTrailing o = (WithoutTrailing) other;
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
    public int compareTo(StatementNoShortIf other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithoutTrailing o = (WithoutTrailing) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Labeled extends hydra.ext.java.syntax.StatementNoShortIf implements Serializable {
    public final hydra.ext.java.syntax.LabeledStatementNoShortIf value;
    
    public Labeled (hydra.ext.java.syntax.LabeledStatementNoShortIf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Labeled)) {
        return false;
      }
      Labeled o = (Labeled) other;
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
    public int compareTo(StatementNoShortIf other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Labeled o = (Labeled) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class IfThenElse extends hydra.ext.java.syntax.StatementNoShortIf implements Serializable {
    public final hydra.ext.java.syntax.IfThenElseStatementNoShortIf value;
    
    public IfThenElse (hydra.ext.java.syntax.IfThenElseStatementNoShortIf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IfThenElse)) {
        return false;
      }
      IfThenElse o = (IfThenElse) other;
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
    public int compareTo(StatementNoShortIf other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IfThenElse o = (IfThenElse) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class While extends hydra.ext.java.syntax.StatementNoShortIf implements Serializable {
    public final hydra.ext.java.syntax.WhileStatementNoShortIf value;
    
    public While (hydra.ext.java.syntax.WhileStatementNoShortIf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof While)) {
        return false;
      }
      While o = (While) other;
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
    public int compareTo(StatementNoShortIf other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      While o = (While) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class For extends hydra.ext.java.syntax.StatementNoShortIf implements Serializable {
    public final hydra.ext.java.syntax.ForStatementNoShortIf value;
    
    public For (hydra.ext.java.syntax.ForStatementNoShortIf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof For)) {
        return false;
      }
      For o = (For) other;
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
    public int compareTo(StatementNoShortIf other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      For o = (For) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
