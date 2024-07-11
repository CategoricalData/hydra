// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * New page format allowing reading levels without decompressing the data Repetition and definition levels are uncompressed The remaining section containing the data is compressed if is_compressed is true
 */
public class DataPageHeaderV2 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.DataPageHeaderV2");
  
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
  public final hydra.langs.parquet.format.Encoding encoding;
  
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
  public final hydra.util.Opt<hydra.langs.parquet.format.Statistics> statistics;
  
  public DataPageHeaderV2 (Integer numValues, Integer numNulls, Integer numRows, hydra.langs.parquet.format.Encoding encoding, Integer definitionLevelsByteLength, Integer repetitionLevelsByteLength, hydra.util.Opt<Boolean> isCompressed, hydra.util.Opt<hydra.langs.parquet.format.Statistics> statistics) {
    if (numValues == null) {
      throw new IllegalArgumentException("null value for 'numValues' argument");
    }
    if (numNulls == null) {
      throw new IllegalArgumentException("null value for 'numNulls' argument");
    }
    if (numRows == null) {
      throw new IllegalArgumentException("null value for 'numRows' argument");
    }
    if (encoding == null) {
      throw new IllegalArgumentException("null value for 'encoding' argument");
    }
    if (definitionLevelsByteLength == null) {
      throw new IllegalArgumentException("null value for 'definitionLevelsByteLength' argument");
    }
    if (repetitionLevelsByteLength == null) {
      throw new IllegalArgumentException("null value for 'repetitionLevelsByteLength' argument");
    }
    if (isCompressed == null) {
      throw new IllegalArgumentException("null value for 'isCompressed' argument");
    }
    if (statistics == null) {
      throw new IllegalArgumentException("null value for 'statistics' argument");
    }
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
    if (numValues == null) {
      throw new IllegalArgumentException("null value for 'numValues' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withNumNulls(Integer numNulls) {
    if (numNulls == null) {
      throw new IllegalArgumentException("null value for 'numNulls' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withNumRows(Integer numRows) {
    if (numRows == null) {
      throw new IllegalArgumentException("null value for 'numRows' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withEncoding(hydra.langs.parquet.format.Encoding encoding) {
    if (encoding == null) {
      throw new IllegalArgumentException("null value for 'encoding' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withDefinitionLevelsByteLength(Integer definitionLevelsByteLength) {
    if (definitionLevelsByteLength == null) {
      throw new IllegalArgumentException("null value for 'definitionLevelsByteLength' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withRepetitionLevelsByteLength(Integer repetitionLevelsByteLength) {
    if (repetitionLevelsByteLength == null) {
      throw new IllegalArgumentException("null value for 'repetitionLevelsByteLength' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withIsCompressed(hydra.util.Opt<Boolean> isCompressed) {
    if (isCompressed == null) {
      throw new IllegalArgumentException("null value for 'isCompressed' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
  
  public DataPageHeaderV2 withStatistics(hydra.util.Opt<hydra.langs.parquet.format.Statistics> statistics) {
    if (statistics == null) {
      throw new IllegalArgumentException("null value for 'statistics' argument");
    }
    return new DataPageHeaderV2(numValues, numNulls, numRows, encoding, definitionLevelsByteLength, repetitionLevelsByteLength, isCompressed, statistics);
  }
}