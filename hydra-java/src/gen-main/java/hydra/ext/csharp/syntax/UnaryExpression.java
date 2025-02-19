// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class UnaryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.UnaryExpression");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_PLUS = new hydra.core.Name("plus");
  
  public static final hydra.core.Name FIELD_NAME_MINUS = new hydra.core.Name("minus");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_BITWISE_COMPLEMENT = new hydra.core.Name("bitwiseComplement");
  
  public static final hydra.core.Name FIELD_NAME_PRE_INCREMENT = new hydra.core.Name("preIncrement");
  
  public static final hydra.core.Name FIELD_NAME_PRE_DECREMENT = new hydra.core.Name("preDecrement");
  
  public static final hydra.core.Name FIELD_NAME_CAST = new hydra.core.Name("cast");
  
  public static final hydra.core.Name FIELD_NAME_AWAIT = new hydra.core.Name("await");
  
  public static final hydra.core.Name FIELD_NAME_POINTER_INDIRECTION = new hydra.core.Name("pointerIndirection");
  
  public static final hydra.core.Name FIELD_NAME_ADDRESS_OF = new hydra.core.Name("addressOf");
  
  private UnaryExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primary instance) ;
    
    R visit(Plus instance) ;
    
    R visit(Minus instance) ;
    
    R visit(Not instance) ;
    
    R visit(BitwiseComplement instance) ;
    
    R visit(PreIncrement instance) ;
    
    R visit(PreDecrement instance) ;
    
    R visit(Cast instance) ;
    
    R visit(Await instance) ;
    
    R visit(PointerIndirection instance) ;
    
    R visit(AddressOf instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
    
    default R visit(Plus instance) {
      return otherwise((instance));
    }
    
    default R visit(Minus instance) {
      return otherwise((instance));
    }
    
    default R visit(Not instance) {
      return otherwise((instance));
    }
    
    default R visit(BitwiseComplement instance) {
      return otherwise((instance));
    }
    
    default R visit(PreIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PreDecrement instance) {
      return otherwise((instance));
    }
    
    default R visit(Cast instance) {
      return otherwise((instance));
    }
    
    default R visit(Await instance) {
      return otherwise((instance));
    }
    
    default R visit(PointerIndirection instance) {
      return otherwise((instance));
    }
    
    default R visit(AddressOf instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primary extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
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
  
  public static final class Plus extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public Plus (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) (other);
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
  
  public static final class Minus extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public Minus (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minus)) {
        return false;
      }
      Minus o = (Minus) (other);
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
  
  public static final class Not extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public Not (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) (other);
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
  
  public static final class BitwiseComplement extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public BitwiseComplement (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseComplement)) {
        return false;
      }
      BitwiseComplement o = (BitwiseComplement) (other);
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
  
  public static final class PreIncrement extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public PreIncrement (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreIncrement)) {
        return false;
      }
      PreIncrement o = (PreIncrement) (other);
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
  
  public static final class PreDecrement extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public PreDecrement (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreDecrement)) {
        return false;
      }
      PreDecrement o = (PreDecrement) (other);
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
  
  public static final class Cast extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.CastExpression value;
    
    public Cast (hydra.ext.csharp.syntax.CastExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cast)) {
        return false;
      }
      Cast o = (Cast) (other);
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
  
  public static final class Await extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public Await (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Await)) {
        return false;
      }
      Await o = (Await) (other);
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
  
  public static final class PointerIndirection extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public PointerIndirection (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointerIndirection)) {
        return false;
      }
      PointerIndirection o = (PointerIndirection) (other);
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
  
  public static final class AddressOf extends hydra.ext.csharp.syntax.UnaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public AddressOf (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddressOf)) {
        return false;
      }
      AddressOf o = (AddressOf) (other);
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