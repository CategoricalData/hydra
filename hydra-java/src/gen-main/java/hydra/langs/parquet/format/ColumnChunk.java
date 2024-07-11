// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

public class ColumnChunk implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.ColumnChunk");
  
  /**
   * File where column data is stored.  If not set, assumed to be same file as metadata.  This path is relative to the current file.
   */
  public final hydra.util.Opt<String> filePath;
  
  /**
   * Byte offset in file_path to the ColumnMetaData
   */
  public final Long fileOffset;
  
  /**
   * Column metadata for this chunk. This is the same content as what is at file_path/file_offset.  Having it here has it replicated in the file metadata.
   */
  public final hydra.util.Opt<hydra.langs.parquet.format.ColumnMetaData> metaData;
  
  /**
   * File offset of ColumnChunk's OffsetIndex
   */
  public final hydra.util.Opt<Long> offsetIndexOffset;
  
  /**
   * Size of ColumnChunk's OffsetIndex, in bytes
   */
  public final hydra.util.Opt<Integer> offsetIndexLength;
  
  /**
   * File offset of ColumnChunk's ColumnIndex
   */
  public final hydra.util.Opt<Long> columnIndexOffset;
  
  /**
   * Size of ColumnChunk's ColumnIndex, in bytes
   */
  public final hydra.util.Opt<Integer> columnIndexLength;
  
  /**
   * Crypto metadata of encrypted columns
   */
  public final hydra.util.Opt<hydra.langs.parquet.format.ColumnCryptoMetaData> cryptoMetadata;
  
  public final hydra.util.Opt<String> encryptedColumnMetadata;
  
  public ColumnChunk (hydra.util.Opt<String> filePath, Long fileOffset, hydra.util.Opt<hydra.langs.parquet.format.ColumnMetaData> metaData, hydra.util.Opt<Long> offsetIndexOffset, hydra.util.Opt<Integer> offsetIndexLength, hydra.util.Opt<Long> columnIndexOffset, hydra.util.Opt<Integer> columnIndexLength, hydra.util.Opt<hydra.langs.parquet.format.ColumnCryptoMetaData> cryptoMetadata, hydra.util.Opt<String> encryptedColumnMetadata) {
    if (filePath == null) {
      throw new IllegalArgumentException("null value for 'filePath' argument");
    }
    if (fileOffset == null) {
      throw new IllegalArgumentException("null value for 'fileOffset' argument");
    }
    if (metaData == null) {
      throw new IllegalArgumentException("null value for 'metaData' argument");
    }
    if (offsetIndexOffset == null) {
      throw new IllegalArgumentException("null value for 'offsetIndexOffset' argument");
    }
    if (offsetIndexLength == null) {
      throw new IllegalArgumentException("null value for 'offsetIndexLength' argument");
    }
    if (columnIndexOffset == null) {
      throw new IllegalArgumentException("null value for 'columnIndexOffset' argument");
    }
    if (columnIndexLength == null) {
      throw new IllegalArgumentException("null value for 'columnIndexLength' argument");
    }
    if (cryptoMetadata == null) {
      throw new IllegalArgumentException("null value for 'cryptoMetadata' argument");
    }
    if (encryptedColumnMetadata == null) {
      throw new IllegalArgumentException("null value for 'encryptedColumnMetadata' argument");
    }
    this.filePath = filePath;
    this.fileOffset = fileOffset;
    this.metaData = metaData;
    this.offsetIndexOffset = offsetIndexOffset;
    this.offsetIndexLength = offsetIndexLength;
    this.columnIndexOffset = columnIndexOffset;
    this.columnIndexLength = columnIndexLength;
    this.cryptoMetadata = cryptoMetadata;
    this.encryptedColumnMetadata = encryptedColumnMetadata;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnChunk)) {
      return false;
    }
    ColumnChunk o = (ColumnChunk) (other);
    return filePath.equals(o.filePath) && fileOffset.equals(o.fileOffset) && metaData.equals(o.metaData) && offsetIndexOffset.equals(o.offsetIndexOffset) && offsetIndexLength.equals(o.offsetIndexLength) && columnIndexOffset.equals(o.columnIndexOffset) && columnIndexLength.equals(o.columnIndexLength) && cryptoMetadata.equals(o.cryptoMetadata) && encryptedColumnMetadata.equals(o.encryptedColumnMetadata);
  }
  
  @Override
  public int hashCode() {
    return 2 * filePath.hashCode() + 3 * fileOffset.hashCode() + 5 * metaData.hashCode() + 7 * offsetIndexOffset.hashCode() + 11 * offsetIndexLength.hashCode() + 13 * columnIndexOffset.hashCode() + 17 * columnIndexLength.hashCode() + 19 * cryptoMetadata.hashCode() + 23 * encryptedColumnMetadata.hashCode();
  }
  
  public ColumnChunk withFilePath(hydra.util.Opt<String> filePath) {
    if (filePath == null) {
      throw new IllegalArgumentException("null value for 'filePath' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withFileOffset(Long fileOffset) {
    if (fileOffset == null) {
      throw new IllegalArgumentException("null value for 'fileOffset' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withMetaData(hydra.util.Opt<hydra.langs.parquet.format.ColumnMetaData> metaData) {
    if (metaData == null) {
      throw new IllegalArgumentException("null value for 'metaData' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withOffsetIndexOffset(hydra.util.Opt<Long> offsetIndexOffset) {
    if (offsetIndexOffset == null) {
      throw new IllegalArgumentException("null value for 'offsetIndexOffset' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withOffsetIndexLength(hydra.util.Opt<Integer> offsetIndexLength) {
    if (offsetIndexLength == null) {
      throw new IllegalArgumentException("null value for 'offsetIndexLength' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withColumnIndexOffset(hydra.util.Opt<Long> columnIndexOffset) {
    if (columnIndexOffset == null) {
      throw new IllegalArgumentException("null value for 'columnIndexOffset' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withColumnIndexLength(hydra.util.Opt<Integer> columnIndexLength) {
    if (columnIndexLength == null) {
      throw new IllegalArgumentException("null value for 'columnIndexLength' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withCryptoMetadata(hydra.util.Opt<hydra.langs.parquet.format.ColumnCryptoMetaData> cryptoMetadata) {
    if (cryptoMetadata == null) {
      throw new IllegalArgumentException("null value for 'cryptoMetadata' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withEncryptedColumnMetadata(hydra.util.Opt<String> encryptedColumnMetadata) {
    if (encryptedColumnMetadata == null) {
      throw new IllegalArgumentException("null value for 'encryptedColumnMetadata' argument");
    }
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
}