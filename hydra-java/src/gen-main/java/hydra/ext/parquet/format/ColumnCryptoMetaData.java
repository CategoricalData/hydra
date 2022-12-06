package hydra.ext.parquet.format;

public abstract class ColumnCryptoMetaData {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/parquet/format.ColumnCryptoMetaData");
  
  private ColumnCryptoMetaData () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(EncryptionWithFooterKey instance) ;
    
    R visit(EncryptionWithColumnKey instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ColumnCryptoMetaData instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(EncryptionWithFooterKey instance) {
      return otherwise((instance));
    }
    
    default R visit(EncryptionWithColumnKey instance) {
      return otherwise((instance));
    }
  }
  
  public static final class EncryptionWithFooterKey extends hydra.ext.parquet.format.ColumnCryptoMetaData {
    public final hydra.ext.parquet.format.EncryptionWithFooterKey value;
    
    public EncryptionWithFooterKey (hydra.ext.parquet.format.EncryptionWithFooterKey value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EncryptionWithFooterKey)) {
        return false;
      }
      EncryptionWithFooterKey o = (EncryptionWithFooterKey) (other);
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
  
  public static final class EncryptionWithColumnKey extends hydra.ext.parquet.format.ColumnCryptoMetaData {
    public final hydra.ext.parquet.format.EncryptionWithColumnKey value;
    
    public EncryptionWithColumnKey (hydra.ext.parquet.format.EncryptionWithColumnKey value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EncryptionWithColumnKey)) {
        return false;
      }
      EncryptionWithColumnKey o = (EncryptionWithColumnKey) (other);
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