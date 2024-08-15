// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

public class RowGroup implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/parquet/format.RowGroup");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS = new hydra.core.Name("columns");
  
  public static final hydra.core.Name FIELD_NAME_TOTAL_BYTE_SIZE = new hydra.core.Name("totalByteSize");
  
  public static final hydra.core.Name FIELD_NAME_NUM_ROWS = new hydra.core.Name("numRows");
  
  public static final hydra.core.Name FIELD_NAME_SORTING_COLUMNS = new hydra.core.Name("sortingColumns");
  
  public static final hydra.core.Name FIELD_NAME_FILE_OFFSET = new hydra.core.Name("fileOffset");
  
  public static final hydra.core.Name FIELD_NAME_TOTAL_COMPRESSED_SIZE = new hydra.core.Name("totalCompressedSize");
  
  public static final hydra.core.Name FIELD_NAME_ORDINAL = new hydra.core.Name("ordinal");
  
  /**
   * Metadata for each column chunk in this row group. This list must have the same order as the SchemaElement list in FileMetaData.
   */
  public final java.util.List<hydra.langs.parquet.format.ColumnChunk> columns;
  
  /**
   * Total byte size of all the uncompressed column data in this row group
   */
  public final Long totalByteSize;
  
  /**
   * Number of rows in this row group
   */
  public final Long numRows;
  
  /**
   * If set, specifies a sort ordering of the rows in this RowGroup. The sorting columns can be a subset of all the columns.
   */
  public final hydra.util.Opt<java.util.List<hydra.langs.parquet.format.SortingColumn>> sortingColumns;
  
  /**
   * Byte offset from beginning of file to first page (data or dictionary) in this row group
   */
  public final hydra.util.Opt<Long> fileOffset;
  
  /**
   * Total byte size of all compressed (and potentially encrypted) column data in this row group
   */
  public final hydra.util.Opt<Long> totalCompressedSize;
  
  /**
   * Row group ordinal in the file
   */
  public final hydra.util.Opt<Short> ordinal;
  
  public RowGroup (java.util.List<hydra.langs.parquet.format.ColumnChunk> columns, Long totalByteSize, Long numRows, hydra.util.Opt<java.util.List<hydra.langs.parquet.format.SortingColumn>> sortingColumns, hydra.util.Opt<Long> fileOffset, hydra.util.Opt<Long> totalCompressedSize, hydra.util.Opt<Short> ordinal) {
    java.util.Objects.requireNonNull((columns));
    java.util.Objects.requireNonNull((totalByteSize));
    java.util.Objects.requireNonNull((numRows));
    java.util.Objects.requireNonNull((sortingColumns));
    java.util.Objects.requireNonNull((fileOffset));
    java.util.Objects.requireNonNull((totalCompressedSize));
    java.util.Objects.requireNonNull((ordinal));
    this.columns = columns;
    this.totalByteSize = totalByteSize;
    this.numRows = numRows;
    this.sortingColumns = sortingColumns;
    this.fileOffset = fileOffset;
    this.totalCompressedSize = totalCompressedSize;
    this.ordinal = ordinal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RowGroup)) {
      return false;
    }
    RowGroup o = (RowGroup) (other);
    return columns.equals(o.columns) && totalByteSize.equals(o.totalByteSize) && numRows.equals(o.numRows) && sortingColumns.equals(o.sortingColumns) && fileOffset.equals(o.fileOffset) && totalCompressedSize.equals(o.totalCompressedSize) && ordinal.equals(o.ordinal);
  }
  
  @Override
  public int hashCode() {
    return 2 * columns.hashCode() + 3 * totalByteSize.hashCode() + 5 * numRows.hashCode() + 7 * sortingColumns.hashCode() + 11 * fileOffset.hashCode() + 13 * totalCompressedSize.hashCode() + 17 * ordinal.hashCode();
  }
  
  public RowGroup withColumns(java.util.List<hydra.langs.parquet.format.ColumnChunk> columns) {
    java.util.Objects.requireNonNull((columns));
    return new RowGroup(columns, totalByteSize, numRows, sortingColumns, fileOffset, totalCompressedSize, ordinal);
  }
  
  public RowGroup withTotalByteSize(Long totalByteSize) {
    java.util.Objects.requireNonNull((totalByteSize));
    return new RowGroup(columns, totalByteSize, numRows, sortingColumns, fileOffset, totalCompressedSize, ordinal);
  }
  
  public RowGroup withNumRows(Long numRows) {
    java.util.Objects.requireNonNull((numRows));
    return new RowGroup(columns, totalByteSize, numRows, sortingColumns, fileOffset, totalCompressedSize, ordinal);
  }
  
  public RowGroup withSortingColumns(hydra.util.Opt<java.util.List<hydra.langs.parquet.format.SortingColumn>> sortingColumns) {
    java.util.Objects.requireNonNull((sortingColumns));
    return new RowGroup(columns, totalByteSize, numRows, sortingColumns, fileOffset, totalCompressedSize, ordinal);
  }
  
  public RowGroup withFileOffset(hydra.util.Opt<Long> fileOffset) {
    java.util.Objects.requireNonNull((fileOffset));
    return new RowGroup(columns, totalByteSize, numRows, sortingColumns, fileOffset, totalCompressedSize, ordinal);
  }
  
  public RowGroup withTotalCompressedSize(hydra.util.Opt<Long> totalCompressedSize) {
    java.util.Objects.requireNonNull((totalCompressedSize));
    return new RowGroup(columns, totalByteSize, numRows, sortingColumns, fileOffset, totalCompressedSize, ordinal);
  }
  
  public RowGroup withOrdinal(hydra.util.Opt<Short> ordinal) {
    java.util.Objects.requireNonNull((ordinal));
    return new RowGroup(columns, totalByteSize, numRows, sortingColumns, fileOffset, totalCompressedSize, ordinal);
  }
}