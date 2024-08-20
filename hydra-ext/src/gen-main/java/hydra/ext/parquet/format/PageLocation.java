// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

public class PageLocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.PageLocation");
  
  public static final hydra.core.Name FIELD_NAME_OFFSET = new hydra.core.Name("offset");
  
  public static final hydra.core.Name FIELD_NAME_COMPRESSED_PAGE_SIZE = new hydra.core.Name("compressedPageSize");
  
  public static final hydra.core.Name FIELD_NAME_FIRST_ROW_INDEX = new hydra.core.Name("firstRowIndex");
  
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
    java.util.Objects.requireNonNull((offset));
    java.util.Objects.requireNonNull((compressedPageSize));
    java.util.Objects.requireNonNull((firstRowIndex));
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
    java.util.Objects.requireNonNull((offset));
    return new PageLocation(offset, compressedPageSize, firstRowIndex);
  }
  
  public PageLocation withCompressedPageSize(Integer compressedPageSize) {
    java.util.Objects.requireNonNull((compressedPageSize));
    return new PageLocation(offset, compressedPageSize, firstRowIndex);
  }
  
  public PageLocation withFirstRowIndex(Long firstRowIndex) {
    java.util.Objects.requireNonNull((firstRowIndex));
    return new PageLocation(offset, compressedPageSize, firstRowIndex);
  }
}
