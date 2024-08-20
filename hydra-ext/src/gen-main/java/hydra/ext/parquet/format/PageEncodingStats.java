// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * statistics of a given page type and encoding
 */
public class PageEncodingStats implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.PageEncodingStats");
  
  public static final hydra.core.Name FIELD_NAME_PAGE_TYPE = new hydra.core.Name("pageType");
  
  public static final hydra.core.Name FIELD_NAME_ENCODING = new hydra.core.Name("encoding");
  
  public static final hydra.core.Name FIELD_NAME_COUNT = new hydra.core.Name("count");
  
  /**
   * the page type (data/dic/...)
   */
  public final hydra.ext.parquet.format.PageType pageType;
  
  /**
   * encoding of the page
   */
  public final hydra.ext.parquet.format.Encoding encoding;
  
  /**
   * number of pages of this type with this encoding
   */
  public final Integer count;
  
  public PageEncodingStats (hydra.ext.parquet.format.PageType pageType, hydra.ext.parquet.format.Encoding encoding, Integer count) {
    java.util.Objects.requireNonNull((pageType));
    java.util.Objects.requireNonNull((encoding));
    java.util.Objects.requireNonNull((count));
    this.pageType = pageType;
    this.encoding = encoding;
    this.count = count;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PageEncodingStats)) {
      return false;
    }
    PageEncodingStats o = (PageEncodingStats) (other);
    return pageType.equals(o.pageType) && encoding.equals(o.encoding) && count.equals(o.count);
  }
  
  @Override
  public int hashCode() {
    return 2 * pageType.hashCode() + 3 * encoding.hashCode() + 5 * count.hashCode();
  }
  
  public PageEncodingStats withPageType(hydra.ext.parquet.format.PageType pageType) {
    java.util.Objects.requireNonNull((pageType));
    return new PageEncodingStats(pageType, encoding, count);
  }
  
  public PageEncodingStats withEncoding(hydra.ext.parquet.format.Encoding encoding) {
    java.util.Objects.requireNonNull((encoding));
    return new PageEncodingStats(pageType, encoding, count);
  }
  
  public PageEncodingStats withCount(Integer count) {
    java.util.Objects.requireNonNull((count));
    return new PageEncodingStats(pageType, encoding, count);
  }
}