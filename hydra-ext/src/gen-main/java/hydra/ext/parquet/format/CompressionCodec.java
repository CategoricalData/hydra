// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * Supported compression algorithms. Codecs added in format version X.Y can be read by readers based on X.Y and later. Codec support may vary between readers based on the format version and libraries available at runtime. See Compression.md for a detailed specification of these algorithms.
 */
public abstract class CompressionCodec implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.CompressionCodec");
  
  public static final hydra.core.Name FIELD_NAME_UNCOMPRESSED = new hydra.core.Name("uncompressed");
  
  public static final hydra.core.Name FIELD_NAME_SNAPPY = new hydra.core.Name("snappy");
  
  public static final hydra.core.Name FIELD_NAME_GZIP = new hydra.core.Name("gzip");
  
  public static final hydra.core.Name FIELD_NAME_LZO = new hydra.core.Name("lzo");
  
  public static final hydra.core.Name FIELD_NAME_BROTLI = new hydra.core.Name("brotli");
  
  public static final hydra.core.Name FIELD_NAME_ZSTD = new hydra.core.Name("zstd");
  
  public static final hydra.core.Name FIELD_NAME_LZ4_RAW = new hydra.core.Name("lz4Raw");
  
  private CompressionCodec () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Uncompressed instance) ;
    
    R visit(Snappy instance) ;
    
    R visit(Gzip instance) ;
    
    R visit(Lzo instance) ;
    
    R visit(Brotli instance) ;
    
    R visit(Zstd instance) ;
    
    R visit(Lz4Raw instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CompressionCodec instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Uncompressed instance) {
      return otherwise((instance));
    }
    
    default R visit(Snappy instance) {
      return otherwise((instance));
    }
    
    default R visit(Gzip instance) {
      return otherwise((instance));
    }
    
    default R visit(Lzo instance) {
      return otherwise((instance));
    }
    
    default R visit(Brotli instance) {
      return otherwise((instance));
    }
    
    default R visit(Zstd instance) {
      return otherwise((instance));
    }
    
    default R visit(Lz4Raw instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Uncompressed extends hydra.ext.parquet.format.CompressionCodec implements Serializable {
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
  
  public static final class Snappy extends hydra.ext.parquet.format.CompressionCodec implements Serializable {
    public Snappy () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Snappy)) {
        return false;
      }
      Snappy o = (Snappy) (other);
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
  
  public static final class Gzip extends hydra.ext.parquet.format.CompressionCodec implements Serializable {
    public Gzip () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gzip)) {
        return false;
      }
      Gzip o = (Gzip) (other);
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
  
  public static final class Lzo extends hydra.ext.parquet.format.CompressionCodec implements Serializable {
    public Lzo () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lzo)) {
        return false;
      }
      Lzo o = (Lzo) (other);
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
  
  /**
   * Added in 2.4
   */
  public static final class Brotli extends hydra.ext.parquet.format.CompressionCodec implements Serializable {
    public Brotli () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Brotli)) {
        return false;
      }
      Brotli o = (Brotli) (other);
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
  
  /**
   * Added in 2.4
   */
  public static final class Zstd extends hydra.ext.parquet.format.CompressionCodec implements Serializable {
    public Zstd () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Zstd)) {
        return false;
      }
      Zstd o = (Zstd) (other);
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
  
  /**
   * Added in 2.9
   */
  public static final class Lz4Raw extends hydra.ext.parquet.format.CompressionCodec implements Serializable {
    public Lz4Raw () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lz4Raw)) {
        return false;
      }
      Lz4Raw o = (Lz4Raw) (other);
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
