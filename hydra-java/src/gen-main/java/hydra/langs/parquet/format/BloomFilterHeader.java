// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Bloom filter header is stored at beginning of Bloom filter data of each column and followed by its bitset.
 */
public class BloomFilterHeader implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.BloomFilterHeader");
  
  /**
   * The size of bitset in bytes
   */
  public final Integer numBytes;
  
  /**
   * The algorithm for setting bits.
   */
  public final hydra.langs.parquet.format.BloomFilterAlgorithm algorithm;
  
  /**
   * The hash function used for Bloom filter.
   */
  public final hydra.langs.parquet.format.BloomFilterHash hash;
  
  /**
   * The compression used in the Bloom filter
   */
  public final hydra.langs.parquet.format.BloomFilterCompression compression;
  
  public BloomFilterHeader (Integer numBytes, hydra.langs.parquet.format.BloomFilterAlgorithm algorithm, hydra.langs.parquet.format.BloomFilterHash hash, hydra.langs.parquet.format.BloomFilterCompression compression) {
    if (numBytes == null) {
      throw new IllegalArgumentException("null value for 'numBytes' argument");
    }
    if (algorithm == null) {
      throw new IllegalArgumentException("null value for 'algorithm' argument");
    }
    if (hash == null) {
      throw new IllegalArgumentException("null value for 'hash' argument");
    }
    if (compression == null) {
      throw new IllegalArgumentException("null value for 'compression' argument");
    }
    this.numBytes = numBytes;
    this.algorithm = algorithm;
    this.hash = hash;
    this.compression = compression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BloomFilterHeader)) {
      return false;
    }
    BloomFilterHeader o = (BloomFilterHeader) (other);
    return numBytes.equals(o.numBytes) && algorithm.equals(o.algorithm) && hash.equals(o.hash) && compression.equals(o.compression);
  }
  
  @Override
  public int hashCode() {
    return 2 * numBytes.hashCode() + 3 * algorithm.hashCode() + 5 * hash.hashCode() + 7 * compression.hashCode();
  }
  
  public BloomFilterHeader withNumBytes(Integer numBytes) {
    if (numBytes == null) {
      throw new IllegalArgumentException("null value for 'numBytes' argument");
    }
    return new BloomFilterHeader(numBytes, algorithm, hash, compression);
  }
  
  public BloomFilterHeader withAlgorithm(hydra.langs.parquet.format.BloomFilterAlgorithm algorithm) {
    if (algorithm == null) {
      throw new IllegalArgumentException("null value for 'algorithm' argument");
    }
    return new BloomFilterHeader(numBytes, algorithm, hash, compression);
  }
  
  public BloomFilterHeader withHash(hydra.langs.parquet.format.BloomFilterHash hash) {
    if (hash == null) {
      throw new IllegalArgumentException("null value for 'hash' argument");
    }
    return new BloomFilterHeader(numBytes, algorithm, hash, compression);
  }
  
  public BloomFilterHeader withCompression(hydra.langs.parquet.format.BloomFilterCompression compression) {
    if (compression == null) {
      throw new IllegalArgumentException("null value for 'compression' argument");
    }
    return new BloomFilterHeader(numBytes, algorithm, hash, compression);
  }
}