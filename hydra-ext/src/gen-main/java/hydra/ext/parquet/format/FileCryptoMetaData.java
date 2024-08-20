// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * Crypto metadata for files with encrypted footer
 */
public class FileCryptoMetaData implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.FileCryptoMetaData");
  
  public static final hydra.core.Name FIELD_NAME_ENCRYPTION_ALGORITHM = new hydra.core.Name("encryptionAlgorithm");
  
  public static final hydra.core.Name FIELD_NAME_KEY_METADATA = new hydra.core.Name("keyMetadata");
  
  /**
   * Encryption algorithm. This field is only used for files with encrypted footer. Files with plaintext footer store algorithm id inside footer (FileMetaData structure).
   */
  public final hydra.ext.parquet.format.EncryptionAlgorithm encryptionAlgorithm;
  
  /**
   * Retrieval metadata of key used for encryption of footer, and (possibly) columns
   */
  public final hydra.util.Opt<String> keyMetadata;
  
  public FileCryptoMetaData (hydra.ext.parquet.format.EncryptionAlgorithm encryptionAlgorithm, hydra.util.Opt<String> keyMetadata) {
    java.util.Objects.requireNonNull((encryptionAlgorithm));
    java.util.Objects.requireNonNull((keyMetadata));
    this.encryptionAlgorithm = encryptionAlgorithm;
    this.keyMetadata = keyMetadata;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FileCryptoMetaData)) {
      return false;
    }
    FileCryptoMetaData o = (FileCryptoMetaData) (other);
    return encryptionAlgorithm.equals(o.encryptionAlgorithm) && keyMetadata.equals(o.keyMetadata);
  }
  
  @Override
  public int hashCode() {
    return 2 * encryptionAlgorithm.hashCode() + 3 * keyMetadata.hashCode();
  }
  
  public FileCryptoMetaData withEncryptionAlgorithm(hydra.ext.parquet.format.EncryptionAlgorithm encryptionAlgorithm) {
    java.util.Objects.requireNonNull((encryptionAlgorithm));
    return new FileCryptoMetaData(encryptionAlgorithm, keyMetadata);
  }
  
  public FileCryptoMetaData withKeyMetadata(hydra.util.Opt<String> keyMetadata) {
    java.util.Objects.requireNonNull((keyMetadata));
    return new FileCryptoMetaData(encryptionAlgorithm, keyMetadata);
  }
}
