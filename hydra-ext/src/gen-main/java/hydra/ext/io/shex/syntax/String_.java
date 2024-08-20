// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class String_ implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.String");
  
  public static final hydra.core.Name FIELD_NAME_STRING_LITERAL1 = new hydra.core.Name("stringLiteral1");
  
  public static final hydra.core.Name FIELD_NAME_STRING_LITERAL_LONG1 = new hydra.core.Name("stringLiteralLong1");
  
  public static final hydra.core.Name FIELD_NAME_STRING_LITERAL2 = new hydra.core.Name("stringLiteral2");
  
  public static final hydra.core.Name FIELD_NAME_STRING_LITERAL_LONG2 = new hydra.core.Name("stringLiteralLong2");
  
  private String_ () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(StringLiteral1 instance) ;
    
    R visit(StringLiteralLong1 instance) ;
    
    R visit(StringLiteral2 instance) ;
    
    R visit(StringLiteralLong2 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(String_ instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(StringLiteral1 instance) {
      return otherwise((instance));
    }
    
    default R visit(StringLiteralLong1 instance) {
      return otherwise((instance));
    }
    
    default R visit(StringLiteral2 instance) {
      return otherwise((instance));
    }
    
    default R visit(StringLiteralLong2 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class StringLiteral1 extends hydra.ext.io.shex.syntax.String_ implements Serializable {
    public final hydra.ext.io.shex.syntax.StringLiteral1 value;
    
    public StringLiteral1 (hydra.ext.io.shex.syntax.StringLiteral1 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringLiteral1)) {
        return false;
      }
      StringLiteral1 o = (StringLiteral1) (other);
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
  
  public static final class StringLiteralLong1 extends hydra.ext.io.shex.syntax.String_ implements Serializable {
    public final hydra.ext.io.shex.syntax.StringLiteralLong1 value;
    
    public StringLiteralLong1 (hydra.ext.io.shex.syntax.StringLiteralLong1 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringLiteralLong1)) {
        return false;
      }
      StringLiteralLong1 o = (StringLiteralLong1) (other);
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
  
  public static final class StringLiteral2 extends hydra.ext.io.shex.syntax.String_ implements Serializable {
    public final hydra.ext.io.shex.syntax.StringLiteral2 value;
    
    public StringLiteral2 (hydra.ext.io.shex.syntax.StringLiteral2 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringLiteral2)) {
        return false;
      }
      StringLiteral2 o = (StringLiteral2) (other);
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
  
  public static final class StringLiteralLong2 extends hydra.ext.io.shex.syntax.String_ implements Serializable {
    public final hydra.ext.io.shex.syntax.StringLiteralLong2 value;
    
    public StringLiteralLong2 (hydra.ext.io.shex.syntax.StringLiteralLong2 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringLiteralLong2)) {
        return false;
      }
      StringLiteralLong2 o = (StringLiteralLong2) (other);
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