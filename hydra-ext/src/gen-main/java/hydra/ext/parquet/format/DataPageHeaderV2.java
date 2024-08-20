// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * New page format allowing reading levels without decompressing the data Repetition and definition levels are uncompressed The remaining section containing the data is compressed if is_compressed is true
 */
public class DataPageHeaderV2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.DataPageHeaderV2");
  
  public static final hydra.core.Name FIELD_NAME_NUM_VALUES = new hydra.core.Name("numValues");
  
  public static final hydra.core.Name FIELD_NAME_NUM_NULLS = new hydra.core.Name("numNulls");
  
  public static final hydra.core.Name FIELD_NAME_NUM_ROWS = new hydra.core.Name("numRows");
  
  public static final hydra.core.Name FIELD_NAME_ENCODING = new hydra.core.Name("encoding");
  
  public static final hydra.core.Name FIELD_NAME_DEFINITION_LEVELS_BYTE_LENGTH = new hydra.core.Name("definitionLevelsByteLength");
  
  public static final hydra.core.Name FIELD_NAME_REPETITION_LEVELS_BYTE_LENGTH = new hydra.core.Name("repetitionLevelsByteLength");
  
  public static final hydra.core.Name FIELD_NAME_IS_COMPRESSED = new hydra.core.Name("isCompressed");
  
  public static final hydra.core.Name FIELD_NAME_STATISTICS = new hydra.core.Name("statistics");
  
  /**
   * Number of values, including NULLs, in this data page.
   */
  public final Integer numValues;
  
  /**
   * Number of NULL values, in this data page. Number of non-null = num_values - num_nulls which is also the number of values in the data section
   */
  public final Integer numNulls;
  
  /**
   * Number of rows in this data page. which means pages change on record boundaries (r = 0)
   */
  public final Integer numRows;
  
  /**
   * Encoding used for data in this page
   */
  public final hydra.ext.parquet.format.Encoding encoding;
  
  /**
   * length of the definition levels
   */
  public final Integer definitionLevelsByteLength;
  
  /**
   * length of the repetition levels
   */
  public final Integer repetitionLevelsByteLength;
  
  /**
   * whether the values are compressed. Which means the section of the page between definition_levels_byte_length + repetition_levels_byte_length + 1 and compressed_page_size (included) is compressed with the compression_codec. If missing it is considered compressed
   */
  public final hydra.util.Opt<Boolean> isCompressed;
  
  /**
   * optional statistics for the data in this page
   */
  public final hydra.util.Opt<hydra.ext.parquet.format.Statistics> statistics;
  
  public DataPageHeaderV2 (Integer numValues, Integer numNulls, Integer numRows, hydra.ext.parquet.format.Encoding encoding, Integer definitionLevelsByteLength, Integer repetitionLevelsByteLength, hydra.util.Opt<Boolean> isCompressed, hydra.util.Opt<hydra.ext.parquet.format.Statistics> statistics) {
    java.util.Objects.requireNonNull((numValues));
    java.util.Objects.requireNonNull((numNulls));
    java.util.Objects.requireNonNull((numRows));
    java.util.Objects.requireNonNull((encoding));
    java.util.Objects.requireNonNull((definitionLevelsByteLength));
    java.util.Objects.requireNonNull((repetitionLevelsByteLength));
    java.util.Objects.requireNonNull((isCompressed));
    java.util.Objects.requireNonNull((statistics));
    this.numValues = numValues;
    this.numNulls = numNulls;
    this.numRows = numRows;
    this.encoding = encoding;
    this.definitionLevelsByteLength = definitionLevelsByteLength;
    this.repetitionLevelsByteLength = repetitionLevelsByteLength;
    this.isCompressed = isCompressed;
    this.statistics = statistics;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPageHeaderV2)) {
      return false;
    }
    DataPageHeaderV2 o = (DataPageHeaderV2) (other);
    return numValues.equals(o.numValues) && numNulls.equals(o.numNulls) && numRows.equals(o.numRows) && encoding.equals(o.encoding) && definitionLevelsByteLength.equals(o.definitionLevelsByteLength) && repetitionLevelsByteLength.equals(o.repetitionLevelsByteLength) && isCompressed.equals(o.isCompressed) && statistics.equals(o.statistics);
  }
  
  @Override
  public int hashCode() {
    return 2 * numValues.hashCode() + 3 * numNulls.hashCode() + 5 * numRows.hashCode() + 7 * encoding.hashCode() + 11 * definitionLevelsByteLength.hashCode() + 13 * repetitionLevelsByteLength.hashCode() + 17 * isCompressed.hashCode() + 19 * statistics.hashCode();
  }
  
  public DataPageHeaderV2 withNumValues(Integer numValues) {
    java.util.Objects.requireNonNull((numValues));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withNumNulls(Integer numNulls) {
    java.util.Objects.requireNonNull((numNulls));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withNumRows(Integer numRows) {
    java.util.Objects.requireNonNull((numRows));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withEncoding(hydra.ext.parquet.format.Encoding encoding) {
    java.util.Objects.requireNonNull((encoding));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withDefinitionLevelsByteLength(Integer definitionLevelsByteLength) {
    java.util.Objects.requireNonNull((definitionLevelsByteLength));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withRepetitionLevelsByteLength(Integer repetitionLevelsByteLength) {
    java.util.Objects.requireNonNull((repetitionLevelsByteLength));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withIsCompressed(hydra.util.Opt<Boolean> isCompressed) {
    java.util.Objects.requireNonNull((isCompressed));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withStatistics(hydra.util.Opt<hydra.ext.parquet.format.Statistics> statistics) {
    java.util.Objects.requireNonNull((statistics));
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
}