// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

public class PageLocation implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.PageLocation");
  
  /**
   * Offset of the page in the file
   */
  public final Long offset;
  
  /**
   * Size of the page, including header. Sum of compressed_page_size and header length
   */
  public final Integer compressedPageSize;
  
  /**
   * Index within the RowGroup of the first row of the page; this means pages change on record boundaries (r = 0).
   */
  public final Long firstRowIndex;
  
  public PageLocation (Long offset, Integer compressedPageSize, Long firstRowIndex) {
    if (offset == null) {
      throw new IllegalArgumentException("null value for 'offset' argument");
    }
    if (compressedPageSize == null) {
      throw new IllegalArgumentException("null value for 'compressedPageSize' argument");
    }
    if (firstRowIndex == null) {
      throw new IllegalArgumentException("null value for 'firstRowIndex' argument");
    }
    this.offset = offset;
    this.compressedPageSize = compressedPageSize;
    this.firstRowIndex = firstRowIndex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PageLocation)) {
      return false;
    }
    PageLocation o = (PageLocation) (other);
    return offset.equals(o.offset) && compressedPageSize.equals(o.compressedPageSize) && firstRowIndex.equals(o.firstRowIndex);
  }
  
  @Override
  public int hashCode() {
    return 2 * offset.hashCode() + 3 * compressedPageSize.hashCode() + 5 * firstRowIndex.hashCode();
  }
  
  public PageLocation withOffset(Long offset) {
    if (offset == null) {
      throw new IllegalArgumentException("null value for 'offset' argument");
    }
    return new PageLocation(offset, compressedPageSize, firstRowIndex);
  }
  
  public PageLocation withCompressedPageSize(Integer compressedPageSize) {
    if (compressedPageSize == null) {
      throw new IllegalArgumentException("null value for 'compressedPageSize' argument");
    }
    return new PageLocation(offset, compressedPageSize, firstRowIndex);
  }
  
  public PageLocation withFirstRowIndex(Long firstRowIndex) {
    if (firstRowIndex == null) {
      throw new IllegalArgumentException("null value for 'firstRowIndex' argument");
    }
    return new PageLocation(offset, compressedPageSize, firstRowIndex);
  }
}