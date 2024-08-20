// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

public class PageHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.PageHeader");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_UNCOMPRESSED_PAGE_SIZE = new hydra.core.Name("uncompressedPageSize");
  
  public static final hydra.core.Name FIELD_NAME_COMPRESSED_PAGE_SIZE = new hydra.core.Name("compressedPageSize");
  
  public static final hydra.core.Name FIELD_NAME_CRC = new hydra.core.Name("crc");
  
  public static final hydra.core.Name FIELD_NAME_DATA_PAGE_HEADER = new hydra.core.Name("dataPageHeader");
  
  public static final hydra.core.Name FIELD_NAME_INDEX_PAGE_HEADER = new hydra.core.Name("indexPageHeader");
  
  public static final hydra.core.Name FIELD_NAME_DICTIONARY_PAGE_HEADER = new hydra.core.Name("dictionaryPageHeader");
  
  public static final hydra.core.Name FIELD_NAME_DATA_PAGE_HEADER_V2 = new hydra.core.Name("dataPageHeaderV2");
  
  /**
   * the type of the page: indicates which of the *_header fields is set
   */
  public final hydra.ext.parquet.format.PageType type;
  
  /**
   * Uncompressed page size in bytes (not including this header)
   */
  public final Integer uncompressedPageSize;
  
  /**
   * Compressed (and potentially encrypted) page size in bytes, not including this header
   */
  public final Integer compressedPageSize;
  
  /**
   * The 32bit CRC for the page, to be be calculated as follows:
   * - Using the standard CRC32 algorithm
   * - On the data only, i.e. this header should not be included. 'Data'
   *   hereby refers to the concatenation of the repetition levels, the
   *   definition levels and the column value, in this exact order.
   * - On the encoded versions of the repetition levels, definition levels and
   *   column values
   * - On the compressed versions of the repetition levels, definition levels
   *   and column values where possible;
   *   - For v1 data pages, the repetition levels, definition levels and column
   *     values are always compressed together. If a compression scheme is
   *     specified, the CRC shall be calculated on the compressed version of
   *     this concatenation. If no compression scheme is specified, the CRC
   *     shall be calculated on the uncompressed version of this concatenation.
   *   - For v2 data pages, the repetition levels and definition levels are
   *     handled separately from the data and are never compressed (only
   *     encoded). If a compression scheme is specified, the CRC shall be
   *     calculated on the concatenation of the uncompressed repetition levels,
   *     uncompressed definition levels and the compressed column values.
   *     If no compression scheme is specified, the CRC shall be calculated on
   *     the uncompressed concatenation.
   * - In encrypted columns, CRC is calculated after page encryption; the
   *   encryption itself is performed after page compression (if compressed)
   * If enabled, this allows for disabling checksumming in HDFS if only a few pages need to be read. 
   */
  public final hydra.util.Opt<Integer> crc;
  
  public final hydra.util.Opt<hydra.ext.parquet.format.DataPageHeader> dataPageHeader;
  
  public final hydra.util.Opt<hydra.ext.parquet.format.IndexPageHeader> indexPageHeader;
  
  public final hydra.util.Opt<hydra.ext.parquet.format.DictionaryPageHeader> dictionaryPageHeader;
  
  public final hydra.util.Opt<hydra.ext.parquet.format.DataPageHeaderV2> dataPageHeaderV2;
  
  public PageHeader (hydra.ext.parquet.format.PageType type, Integer uncompressedPageSize, Integer compressedPageSize, hydra.util.Opt<Integer> crc, hydra.util.Opt<hydra.ext.parquet.format.DataPageHeader> dataPageHeader, hydra.util.Opt<hydra.ext.parquet.format.IndexPageHeader> indexPageHeader, hydra.util.Opt<hydra.ext.parquet.format.DictionaryPageHeader> dictionaryPageHeader, hydra.util.Opt<hydra.ext.parquet.format.DataPageHeaderV2> dataPageHeaderV2) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((uncompressedPageSize));
    java.util.Objects.requireNonNull((compressedPageSize));
    java.util.Objects.requireNonNull((crc));
    java.util.Objects.requireNonNull((dataPageHeader));
    java.util.Objects.requireNonNull((indexPageHeader));
    java.util.Objects.requireNonNull((dictionaryPageHeader));
    java.util.Objects.requireNonNull((dataPageHeaderV2));
    this.type = type;
    this.uncompressedPageSize = uncompressedPageSize;
    this.compressedPageSize = compressedPageSize;
    this.crc = crc;
    this.dataPageHeader = dataPageHeader;
    this.indexPageHeader = indexPageHeader;
    this.dictionaryPageHeader = dictionaryPageHeader;
    this.dataPageHeaderV2 = dataPageHeaderV2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PageHeader)) {
      return false;
    }
    PageHeader o = (PageHeader) (other);
    return type.equals(o.type) && uncompressedPageSize.equals(o.uncompressedPageSize) && compressedPageSize.equals(o.compressedPageSize) && crc.equals(o.crc) && dataPageHeader.equals(o.dataPageHeader) && indexPageHeader.equals(o.indexPageHeader) && dictionaryPageHeader.equals(o.dictionaryPageHeader) && dataPageHeaderV2.equals(o.dataPageHeaderV2);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * uncompressedPageSize.hashCode() + 5 * compressedPageSize.hashCode() + 7 * crc.hashCode() + 11 * dataPageHeader.hashCode() + 13 * indexPageHeader.hashCode() + 17 * dictionaryPageHeader.hashCode() + 19 * dataPageHeaderV2.hashCode();
  }
  
  public PageHeader withType(hydra.ext.parquet.format.PageType type) {
    java.util.Objects.requireNonNull((type));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
  
  public PageHeader withUncompressedPageSize(Integer uncompressedPageSize) {
    java.util.Objects.requireNonNull((uncompressedPageSize));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
  
  public PageHeader withCompressedPageSize(Integer compressedPageSize) {
    java.util.Objects.requireNonNull((compressedPageSize));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
  
  public PageHeader withCrc(hydra.util.Opt<Integer> crc) {
    java.util.Objects.requireNonNull((crc));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
  
  public PageHeader withDataPageHeader(hydra.util.Opt<hydra.ext.parquet.format.DataPageHeader> dataPageHeader) {
    java.util.Objects.requireNonNull((dataPageHeader));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
  
  public PageHeader withIndexPageHeader(hydra.util.Opt<hydra.ext.parquet.format.IndexPageHeader> indexPageHeader) {
    java.util.Objects.requireNonNull((indexPageHeader));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
  
  public PageHeader withDictionaryPageHeader(hydra.util.Opt<hydra.ext.parquet.format.DictionaryPageHeader> dictionaryPageHeader) {
    java.util.Objects.requireNonNull((dictionaryPageHeader));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
  
  public PageHeader withDataPageHeaderV2(hydra.util.Opt<hydra.ext.parquet.format.DataPageHeaderV2> dataPageHeaderV2) {
    java.util.Objects.requireNonNull((dataPageHeaderV2));
    return new PageHeader(type, uncompressedPageSize, compressedPageSize, crc, dataPageHeader, indexPageHeader, dictionaryPageHeader, dataPageHeaderV2);
  }
}
