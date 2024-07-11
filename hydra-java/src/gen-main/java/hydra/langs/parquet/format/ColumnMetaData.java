// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Description for column metadata
 */
public class ColumnMetaData implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.ColumnMetaData");
  
  /**
   * Type of this column
   */
  public final hydra.langs.parquet.format.Type type;
  
  /**
   * Set of all encodings used for this column. The purpose is to validate whether we can decode those pages.
   */
  public final java.util.List<hydra.langs.parquet.format.Encoding> encodings;
  
  /**
   * Path in schema
   */
  public final java.util.List<String> pathInSchema;
  
  /**
   * Compression codec
   */
  public final hydra.langs.parquet.format.CompressionCodec codec;
  
  /**
   * Number of values in this column
   */
  public final Long numValues;
  
  /**
   * total byte size of all uncompressed pages in this column chunk (including the headers)
   */
  public final Long totalUncompressedSize;
  
  /**
   * total byte size of all compressed, and potentially encrypted, pages in this column chunk (including the headers)
   */
  public final Long totalCompressedSize;
  
  /**
   * Optional key/value metadata
   */
  public final hydra.util.Opt<java.util.List<hydra.langs.parquet.format.KeyValue>> keyValueMetadata;
  
  /**
   * Byte offset from beginning of file to first data page
   */
  public final Long dataPageOffset;
  
  /**
   * Byte offset from beginning of file to root index page
   */
  public final hydra.util.Opt<Long> indexPageOffset;
  
  /**
   * Byte offset from the beginning of file to first (only) dictionary page
   */
  public final hydra.util.Opt<Long> dictionaryPageOffset;
  
  /**
   * optional statistics for this column chunk
   */
  public final hydra.util.Opt<hydra.langs.parquet.format.Statistics> statistics;
  
  /**
   * Set of all encodings used for pages in this column chunk. This information can be used to determine if all data pages are dictionary encoded for example
   */
  public final hydra.util.Opt<java.util.List<hydra.langs.parquet.format.PageEncodingStats>> encodingStats;
  
  /**
   * Byte offset from beginning of file to Bloom filter data.
   */
  public final hydra.util.Opt<Long> bloomFilterOffset;
  
  public ColumnMetaData (hydra.langs.parquet.format.Type type, java.util.List<hydra.langs.parquet.format.Encoding> encodings, java.util.List<String> pathInSchema, hydra.langs.parquet.format.CompressionCodec codec, Long numValues, Long totalUncompressedSize, Long totalCompressedSize, hydra.util.Opt<java.util.List<hydra.langs.parquet.format.KeyValue>> keyValueMetadata, Long dataPageOffset, hydra.util.Opt<Long> indexPageOffset, hydra.util.Opt<Long> dictionaryPageOffset, hydra.util.Opt<hydra.langs.parquet.format.Statistics> statistics, hydra.util.Opt<java.util.List<hydra.langs.parquet.format.PageEncodingStats>> encodingStats, hydra.util.Opt<Long> bloomFilterOffset) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((encodings));
    java.util.Objects.requireNonNull((pathInSchema));
    java.util.Objects.requireNonNull((codec));
    java.util.Objects.requireNonNull((numValues));
    java.util.Objects.requireNonNull((totalUncompressedSize));
    java.util.Objects.requireNonNull((totalCompressedSize));
    java.util.Objects.requireNonNull((keyValueMetadata));
    java.util.Objects.requireNonNull((dataPageOffset));
    java.util.Objects.requireNonNull((indexPageOffset));
    java.util.Objects.requireNonNull((dictionaryPageOffset));
    java.util.Objects.requireNonNull((statistics));
    java.util.Objects.requireNonNull((encodingStats));
    java.util.Objects.requireNonNull((bloomFilterOffset));
    this.type = type;
    this.encodings = encodings;
    this.pathInSchema = pathInSchema;
    this.codec = codec;
    this.numValues = numValues;
    this.totalUncompressedSize = totalUncompressedSize;
    this.totalCompressedSize = totalCompressedSize;
    this.keyValueMetadata = keyValueMetadata;
    this.dataPageOffset = dataPageOffset;
    this.indexPageOffset = indexPageOffset;
    this.dictionaryPageOffset = dictionaryPageOffset;
    this.statistics = statistics;
    this.encodingStats = encodingStats;
    this.bloomFilterOffset = bloomFilterOffset;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnMetaData)) {
      return false;
    }
    ColumnMetaData o = (ColumnMetaData) (other);
    return type.equals(o.type) && encodings.equals(o.encodings) && pathInSchema.equals(o.pathInSchema) && codec.equals(o.codec) && numValues.equals(o.numValues) && totalUncompressedSize.equals(o.totalUncompressedSize) && totalCompressedSize.equals(o.totalCompressedSize) && keyValueMetadata.equals(o.keyValueMetadata) && dataPageOffset.equals(o.dataPageOffset) && indexPageOffset.equals(o.indexPageOffset) && dictionaryPageOffset.equals(o.dictionaryPageOffset) && statistics.equals(o.statistics) && encodingStats.equals(o.encodingStats) && bloomFilterOffset.equals(o.bloomFilterOffset);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * encodings.hashCode() + 5 * pathInSchema.hashCode() + 7 * codec.hashCode() + 11 * numValues.hashCode() + 13 * totalUncompressedSize.hashCode() + 17 * totalCompressedSize.hashCode() + 19 * keyValueMetadata.hashCode() + 23 * dataPageOffset.hashCode() + 29 * indexPageOffset.hashCode() + 31 * dictionaryPageOffset.hashCode() + 37 * statistics.hashCode() + 41 * encodingStats.hashCode() + 43 * bloomFilterOffset.hashCode();
  }
  
  public ColumnMetaData withType(hydra.langs.parquet.format.Type type) {
    java.util.Objects.requireNonNull((type));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withEncodings(java.util.List<hydra.langs.parquet.format.Encoding> encodings) {
    java.util.Objects.requireNonNull((encodings));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withPathInSchema(java.util.List<String> pathInSchema) {
    java.util.Objects.requireNonNull((pathInSchema));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withCodec(hydra.langs.parquet.format.CompressionCodec codec) {
    java.util.Objects.requireNonNull((codec));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withNumValues(Long numValues) {
    java.util.Objects.requireNonNull((numValues));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withTotalUncompressedSize(Long totalUncompressedSize) {
    java.util.Objects.requireNonNull((totalUncompressedSize));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withTotalCompressedSize(Long totalCompressedSize) {
    java.util.Objects.requireNonNull((totalCompressedSize));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withKeyValueMetadata(hydra.util.Opt<java.util.List<hydra.langs.parquet.format.KeyValue>> keyValueMetadata) {
    java.util.Objects.requireNonNull((keyValueMetadata));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withDataPageOffset(Long dataPageOffset) {
    java.util.Objects.requireNonNull((dataPageOffset));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withIndexPageOffset(hydra.util.Opt<Long> indexPageOffset) {
    java.util.Objects.requireNonNull((indexPageOffset));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withDictionaryPageOffset(hydra.util.Opt<Long> dictionaryPageOffset) {
    java.util.Objects.requireNonNull((dictionaryPageOffset));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withStatistics(hydra.util.Opt<hydra.langs.parquet.format.Statistics> statistics) {
    java.util.Objects.requireNonNull((statistics));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withEncodingStats(hydra.util.Opt<java.util.List<hydra.langs.parquet.format.PageEncodingStats>> encodingStats) {
    java.util.Objects.requireNonNull((encodingStats));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
  
  public ColumnMetaData withBloomFilterOffset(hydra.util.Opt<Long> bloomFilterOffset) {
    java.util.Objects.requireNonNull((bloomFilterOffset));
    return new ColumnMetaData(type, encodings, pathInSchema, codec, numValues, totalUncompressedSize, totalCompressedSize, keyValueMetadata, dataPageOffset, indexPageOffset, dictionaryPageOffset, statistics, encodingStats, bloomFilterOffset);
  }
}