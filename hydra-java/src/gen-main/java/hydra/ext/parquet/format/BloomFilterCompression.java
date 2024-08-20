// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * The compression used in the Bloom filter.
 */
public abstract class BloomFilterCompression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.BloomFilterCompression");
  
  public static final hydra.core.Name FIELD_NAME_UNCOMPRESSED = new hydra.core.Name("uncompressed");
  
  private BloomFilterCompression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Uncompressed instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BloomFilterCompression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Uncompressed instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Uncompressed extends hydra.ext.parquet.format.BloomFilterCompression implements Serializable {
    public Uncompressed () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uncompressed)) {
        return false;
      }
      Uncompressed o = (Uncompressed) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
