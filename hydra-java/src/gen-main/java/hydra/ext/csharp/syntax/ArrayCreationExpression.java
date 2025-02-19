// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ArrayCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ArrayCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_NON_ARRAY_TYPE = new hydra.core.Name("nonArrayType");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY_TYPE = new hydra.core.Name("arrayType");
  
  public static final hydra.core.Name FIELD_NAME_RANK_SPECIFIER = new hydra.core.Name("rankSpecifier");
  
  private ArrayCreationExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NonArrayType instance) ;
    
    R visit(ArrayType instance) ;
    
    R visit(RankSpecifier instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ArrayCreationExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NonArrayType instance) {
      return otherwise((instance));
    }
    
    default R visit(ArrayType instance) {
      return otherwise((instance));
    }
    
    default R visit(RankSpecifier instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NonArrayType extends hydra.ext.csharp.syntax.ArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.NonArrayTypeArrayCreationExpression value;
    
    public NonArrayType (hydra.ext.csharp.syntax.NonArrayTypeArrayCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonArrayType)) {
        return false;
      }
      NonArrayType o = (NonArrayType) (other);
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
  
  public static final class ArrayType extends hydra.ext.csharp.syntax.ArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ArrayTypeArrayCreationExpression value;
    
    public ArrayType (hydra.ext.csharp.syntax.ArrayTypeArrayCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ArrayType)) {
        return false;
      }
      ArrayType o = (ArrayType) (other);
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
  
  public static final class RankSpecifier extends hydra.ext.csharp.syntax.ArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.RankSpecifierArrayCreationExpression value;
    
    public RankSpecifier (hydra.ext.csharp.syntax.RankSpecifierArrayCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RankSpecifier)) {
        return false;
      }
      RankSpecifier o = (RankSpecifier) (other);
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