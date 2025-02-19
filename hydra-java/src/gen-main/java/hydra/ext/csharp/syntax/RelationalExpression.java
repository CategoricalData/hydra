// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class RelationalExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RelationalExpression");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_BINARY = new hydra.core.Name("binary");
  
  public static final hydra.core.Name FIELD_NAME_IS_TYPE = new hydra.core.Name("isType");
  
  public static final hydra.core.Name FIELD_NAME_IS_PATTERN = new hydra.core.Name("isPattern");
  
  public static final hydra.core.Name FIELD_NAME_AS_TYPE = new hydra.core.Name("asType");
  
  private RelationalExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(Binary instance) ;
    
    R visit(IsType instance) ;
    
    R visit(IsPattern instance) ;
    
    R visit(AsType instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelationalExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(IsType instance) {
      return otherwise((instance));
    }
    
    default R visit(IsPattern instance) {
      return otherwise((instance));
    }
    
    default R visit(AsType instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.ext.csharp.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ShiftExpression value;
    
    public Simple (hydra.ext.csharp.syntax.ShiftExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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
  
  public static final class Binary extends hydra.ext.csharp.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.BinaryRelationalExpression value;
    
    public Binary (hydra.ext.csharp.syntax.BinaryRelationalExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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
  
  public static final class IsType extends hydra.ext.csharp.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.IsTypeExpression value;
    
    public IsType (hydra.ext.csharp.syntax.IsTypeExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IsType)) {
        return false;
      }
      IsType o = (IsType) (other);
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
  
  public static final class IsPattern extends hydra.ext.csharp.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.IsPatternExpression value;
    
    public IsPattern (hydra.ext.csharp.syntax.IsPatternExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IsPattern)) {
        return false;
      }
      IsPattern o = (IsPattern) (other);
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
  
  public static final class AsType extends hydra.ext.csharp.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.AsTypeExpression value;
    
    public AsType (hydra.ext.csharp.syntax.AsTypeExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AsType)) {
        return false;
      }
      AsType o = (AsType) (other);
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