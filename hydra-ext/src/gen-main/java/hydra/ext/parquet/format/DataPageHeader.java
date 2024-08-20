// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * Data page header
 */
public class DataPageHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.DataPageHeader");
  
  public static final hydra.core.Name FIELD_NAME_NUM_VALUES = new hydra.core.Name("numValues");
  
  public static final hydra.core.Name FIELD_NAME_ENCODING = new hydra.core.Name("encoding");
  
  public static final hydra.core.Name FIELD_NAME_DEFINITION_LEVEL_ENCODING = new hydra.core.Name("definitionLevelEncoding");
  
  public static final hydra.core.Name FIELD_NAME_REPETITION_LEVEL_ENCODING = new hydra.core.Name("repetitionLevelEncoding");
  
  public static final hydra.core.Name FIELD_NAME_STATISTICS = new hydra.core.Name("statistics");
  
  /**
   * Number of values, including NULLs, in this data page.
   */
  public final Integer numValues;
  
  /**
   * Encoding used for this data page
   */
  public final hydra.ext.parquet.format.Encoding encoding;
  
  /**
   * Encoding used for definition levels
   */
  public final hydra.ext.parquet.format.Encoding definitionLevelEncoding;
  
  /**
   * Encoding used for repetition levels
   */
  public final hydra.ext.parquet.format.Encoding repetitionLevelEncoding;
  
  /**
   * Optional statistics for the data in this page
   */
  public final hydra.util.Opt<hydra.ext.parquet.format.Statistics> statistics;
  
  public DataPageHeader (Integer numValues, hydra.ext.parquet.format.Encoding encoding, hydra.ext.parquet.format.Encoding definitionLevelEncoding, hydra.ext.parquet.format.Encoding repetitionLevelEncoding, hydra.util.Opt<hydra.ext.parquet.format.Statistics> statistics) {
    java.util.Objects.requireNonNull((numValues));
    java.util.Objects.requireNonNull((encoding));
    java.util.Objects.requireNonNull((definitionLevelEncoding));
    java.util.Objects.requireNonNull((repetitionLevelEncoding));
    java.util.Objects.requireNonNull((statistics));
    this.numValues = numValues;
    this.encoding = encoding;
    this.definitionLevelEncoding = definitionLevelEncoding;
    this.repetitionLevelEncoding = repetitionLevelEncoding;
    this.statistics = statistics;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPageHeader)) {
      return false;
    }
    DataPageHeader o = (DataPageHeader) (other);
    return numValues.equals(o.numValues) && encoding.equals(o.encoding) && definitionLevelEncoding.equals(o.definitionLevelEncoding) && repetitionLevelEncoding.equals(o.repetitionLevelEncoding) && statistics.equals(o.statistics);
  }
  
  @Override
  public int hashCode() {
    return 2 * numValues.hashCode() + 3 * encoding.hashCode() + 5 * definitionLevelEncoding.hashCode() + 7 * repetitionLevelEncoding.hashCode() + 11 * statistics.hashCode();
  }
  
  public DataPageHeader withNumValues(Integer numValues) {
    java.util.Objects.requireNonNull((numValues));
    return new DataPageHeader(numValues, encoding, definitionLevelEncoding, repetitionLevelEncoding, statistics);
  }
  
  public DataPageHeader withEncoding(hydra.ext.parquet.format.Encoding encoding) {
    java.util.Objects.requireNonNull((encoding));
    return new DataPageHeader(numValues, encoding, definitionLevelEncoding, repetitionLevelEncoding, statistics);
  }
  
  public DataPageHeader withDefinitionLevelEncoding(hydra.ext.parquet.format.Encoding definitionLevelEncoding) {
    java.util.Objects.requireNonNull((definitionLevelEncoding));
    return new DataPageHeader(numValues, encoding, definitionLevelEncoding, repetitionLevelEncoding, statistics);
  }
  
  public DataPageHeader withRepetitionLevelEncoding(hydra.ext.parquet.format.Encoding repetitionLevelEncoding) {
    java.util.Objects.requireNonNull((repetitionLevelEncoding));
    return new DataPageHeader(numValues, encoding, definitionLevelEncoding, repetitionLevelEncoding, statistics);
  }
  
  public DataPageHeader withStatistics(hydra.util.Opt<hydra.ext.parquet.format.Statistics> statistics) {
    java.util.Objects.requireNonNull((statistics));
    return new DataPageHeader(numValues, encoding, definitionLevelEncoding, repetitionLevelEncoding, statistics);
  }
}