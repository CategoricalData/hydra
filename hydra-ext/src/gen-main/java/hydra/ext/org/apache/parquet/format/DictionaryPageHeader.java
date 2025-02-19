// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

/**
 * The dictionary page must be placed at the first position of the column chunk if it is partly or completely dictionary encoded. At most one dictionary page can be placed in a column chunk.
 */
public class DictionaryPageHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.parquet.format.DictionaryPageHeader");
  
  public static final hydra.core.Name FIELD_NAME_NUM_VALUES = new hydra.core.Name("numValues");
  
  public static final hydra.core.Name FIELD_NAME_ENCODING = new hydra.core.Name("encoding");
  
  public static final hydra.core.Name FIELD_NAME_IS_SORTED = new hydra.core.Name("isSorted");
  
  /**
   * Number of values in the dictionary
   */
  public final Integer numValues;
  
  /**
   * Encoding using this dictionary page
   */
  public final hydra.ext.org.apache.parquet.format.Encoding encoding;
  
  /**
   * If true, the entries in the dictionary are sorted in ascending order
   */
  public final hydra.util.Opt<Boolean> isSorted;
  
  public DictionaryPageHeader (Integer numValues, hydra.ext.org.apache.parquet.format.Encoding encoding, hydra.util.Opt<Boolean> isSorted) {
    java.util.Objects.requireNonNull((numValues));
    java.util.Objects.requireNonNull((encoding));
    java.util.Objects.requireNonNull((isSorted));
    this.numValues = numValues;
    this.encoding = encoding;
    this.isSorted = isSorted;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DictionaryPageHeader)) {
      return false;
    }
    DictionaryPageHeader o = (DictionaryPageHeader) (other);
    return numValues.equals(o.numValues) && encoding.equals(o.encoding) && isSorted.equals(o.isSorted);
  }
  
  @Override
  public int hashCode() {
    return 2 * numValues.hashCode() + 3 * encoding.hashCode() + 5 * isSorted.hashCode();
  }
  
  public DictionaryPageHeader withNumValues(Integer numValues) {
    java.util.Objects.requireNonNull((numValues));
    return new DictionaryPageHeader(numValues, encoding, isSorted);
  }
  
  public DictionaryPageHeader withEncoding(hydra.ext.org.apache.parquet.format.Encoding encoding) {
    java.util.Objects.requireNonNull((encoding));
    return new DictionaryPageHeader(numValues, encoding, isSorted);
  }
  
  public DictionaryPageHeader withIsSorted(hydra.util.Opt<Boolean> isSorted) {
    java.util.Objects.requireNonNull((isSorted));
    return new DictionaryPageHeader(numValues, encoding, isSorted);
  }
}