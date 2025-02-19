// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class MemberAccessHead implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MemberAccessHead");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_PREDEFINED = new hydra.core.Name("predefined");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_ALIAS = new hydra.core.Name("qualifiedAlias");
  
  private MemberAccessHead () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primary instance) ;
    
    R visit(Predefined instance) ;
    
    R visit(QualifiedAlias instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MemberAccessHead instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
    
    default R visit(Predefined instance) {
      return otherwise((instance));
    }
    
    default R visit(QualifiedAlias instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primary extends hydra.ext.csharp.syntax.MemberAccessHead implements Serializable {
    public final hydra.ext.csharp.syntax.PrimaryExpression value;
    
    public Primary (hydra.ext.csharp.syntax.PrimaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) (other);
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
  
  public static final class Predefined extends hydra.ext.csharp.syntax.MemberAccessHead implements Serializable {
    public final hydra.ext.csharp.syntax.PredefinedType value;
    
    public Predefined (hydra.ext.csharp.syntax.PredefinedType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predefined)) {
        return false;
      }
      Predefined o = (Predefined) (other);
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
  
  public static final class QualifiedAlias extends hydra.ext.csharp.syntax.MemberAccessHead implements Serializable {
    public final hydra.ext.csharp.syntax.QualifiedAliasMember value;
    
    public QualifiedAlias (hydra.ext.csharp.syntax.QualifiedAliasMember value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QualifiedAlias)) {
        return false;
      }
      QualifiedAlias o = (QualifiedAlias) (other);
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