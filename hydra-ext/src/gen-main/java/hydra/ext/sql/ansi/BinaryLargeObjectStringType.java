// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public abstract class BinaryLargeObjectStringType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.BinaryLargeObjectStringType");
  
  public static final hydra.core.Name FIELD_NAME_BINARY = new hydra.core.Name("binary");
  
  public static final hydra.core.Name FIELD_NAME_BLOB = new hydra.core.Name("blob");
  
  private BinaryLargeObjectStringType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Binary instance) ;
    
    R visit(Blob instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BinaryLargeObjectStringType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Blob instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Binary extends hydra.ext.sql.ansi.BinaryLargeObjectStringType implements Serializable {
    public final hydra.util.Opt<hydra.ext.sql.ansi.LargeObjectLength> value;
    
    public Binary (hydra.util.Opt<hydra.ext.sql.ansi.LargeObjectLength> value) {
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
  
  public static final class Blob extends hydra.ext.sql.ansi.BinaryLargeObjectStringType implements Serializable {
    public final hydra.util.Opt<hydra.ext.sql.ansi.LargeObjectLength> value;
    
    public Blob (hydra.util.Opt<hydra.ext.sql.ansi.LargeObjectLength> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Blob)) {
        return false;
      }
      Blob o = (Blob) (other);
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
