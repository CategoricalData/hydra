// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

public class ColumnChunk implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/parquet/format.ColumnChunk");
  
  public static final hydra.core.Name FIELD_NAME_FILE_PATH = new hydra.core.Name("filePath");
  
  public static final hydra.core.Name FIELD_NAME_FILE_OFFSET = new hydra.core.Name("fileOffset");
  
  public static final hydra.core.Name FIELD_NAME_META_DATA = new hydra.core.Name("metaData");
  
  public static final hydra.core.Name FIELD_NAME_OFFSET_INDEX_OFFSET = new hydra.core.Name("offsetIndexOffset");
  
  public static final hydra.core.Name FIELD_NAME_OFFSET_INDEX_LENGTH = new hydra.core.Name("offsetIndexLength");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN_INDEX_OFFSET = new hydra.core.Name("columnIndexOffset");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN_INDEX_LENGTH = new hydra.core.Name("columnIndexLength");
  
  public static final hydra.core.Name FIELD_NAME_CRYPTO_METADATA = new hydra.core.Name("cryptoMetadata");
  
  public static final hydra.core.Name FIELD_NAME_ENCRYPTED_COLUMN_METADATA = new hydra.core.Name("encryptedColumnMetadata");
  
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
  
  /**
   * Encrypted column metadata for this chunk
   */
  public final hydra.util.Opt<String> encryptedColumnMetadata;
  
  public ColumnChunk (hydra.util.Opt<String> filePath, Long fileOffset, hydra.util.Opt<hydra.langs.parquet.format.ColumnMetaData> metaData, hydra.util.Opt<Long> offsetIndexOffset, hydra.util.Opt<Integer> offsetIndexLength, hydra.util.Opt<Long> columnIndexOffset, hydra.util.Opt<Integer> columnIndexLength, hydra.util.Opt<hydra.langs.parquet.format.ColumnCryptoMetaData> cryptoMetadata, hydra.util.Opt<String> encryptedColumnMetadata) {
    java.util.Objects.requireNonNull((filePath));
    java.util.Objects.requireNonNull((fileOffset));
    java.util.Objects.requireNonNull((metaData));
    java.util.Objects.requireNonNull((offsetIndexOffset));
    java.util.Objects.requireNonNull((offsetIndexLength));
    java.util.Objects.requireNonNull((columnIndexOffset));
    java.util.Objects.requireNonNull((columnIndexLength));
    java.util.Objects.requireNonNull((cryptoMetadata));
    java.util.Objects.requireNonNull((encryptedColumnMetadata));
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
    java.util.Objects.requireNonNull((filePath));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withFileOffset(Long fileOffset) {
    java.util.Objects.requireNonNull((fileOffset));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withMetaData(hydra.util.Opt<hydra.langs.parquet.format.ColumnMetaData> metaData) {
    java.util.Objects.requireNonNull((metaData));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withOffsetIndexOffset(hydra.util.Opt<Long> offsetIndexOffset) {
    java.util.Objects.requireNonNull((offsetIndexOffset));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withOffsetIndexLength(hydra.util.Opt<Integer> offsetIndexLength) {
    java.util.Objects.requireNonNull((offsetIndexLength));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withColumnIndexOffset(hydra.util.Opt<Long> columnIndexOffset) {
    java.util.Objects.requireNonNull((columnIndexOffset));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withColumnIndexLength(hydra.util.Opt<Integer> columnIndexLength) {
    java.util.Objects.requireNonNull((columnIndexLength));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withCryptoMetadata(hydra.util.Opt<hydra.langs.parquet.format.ColumnCryptoMetaData> cryptoMetadata) {
    java.util.Objects.requireNonNull((cryptoMetadata));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
  
  public ColumnChunk withEncryptedColumnMetadata(hydra.util.Opt<String> encryptedColumnMetadata) {
    java.util.Objects.requireNonNull((encryptedColumnMetadata));
    return new ColumnChunk(filePath, fileOffset, metaData, offsetIndexOffset, offsetIndexLength, columnIndexOffset, columnIndexLength, cryptoMetadata, encryptedColumnMetadata);
  }
}