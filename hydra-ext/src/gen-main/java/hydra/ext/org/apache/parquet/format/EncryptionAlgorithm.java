// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

public abstract class EncryptionAlgorithm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.parquet.format.EncryptionAlgorithm");
  
  public static final hydra.core.Name FIELD_NAME_AES_GCM_V1 = new hydra.core.Name("aesGcmV1");
  
  public static final hydra.core.Name FIELD_NAME_AES_GCM_CTR_V1 = new hydra.core.Name("aesGcmCtrV1");
  
  private EncryptionAlgorithm () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AesGcmV1 instance) ;
    
    R visit(AesGcmCtrV1 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EncryptionAlgorithm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AesGcmV1 instance) {
      return otherwise((instance));
    }
    
    default R visit(AesGcmCtrV1 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AesGcmV1 extends hydra.ext.org.apache.parquet.format.EncryptionAlgorithm implements Serializable {
    public final hydra.ext.org.apache.parquet.format.AesGcmV1 value;
    
    public AesGcmV1 (hydra.ext.org.apache.parquet.format.AesGcmV1 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AesGcmV1)) {
        return false;
      }
      AesGcmV1 o = (AesGcmV1) (other);
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
  
  public static final class AesGcmCtrV1 extends hydra.ext.org.apache.parquet.format.EncryptionAlgorithm implements Serializable {
    public final hydra.ext.org.apache.parquet.format.AesGcmCtrV1 value;
    
    public AesGcmCtrV1 (hydra.ext.org.apache.parquet.format.AesGcmCtrV1 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AesGcmCtrV1)) {
        return false;
      }
      AesGcmCtrV1 o = (AesGcmCtrV1) (other);
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