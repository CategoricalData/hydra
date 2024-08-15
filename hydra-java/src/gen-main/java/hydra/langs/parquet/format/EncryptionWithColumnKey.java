// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

public class EncryptionWithColumnKey implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/parquet/format.EncryptionWithColumnKey");
  
  public static final hydra.core.Name FIELD_NAME_PATH_IN_SCHEMA = new hydra.core.Name("pathInSchema");
  
  public static final hydra.core.Name FIELD_NAME_KEY_METADATA = new hydra.core.Name("keyMetadata");
  
  /**
   * Column path in schema
   */
  public final java.util.List<String> pathInSchema;
  
  /**
   * Retrieval metadata of column encryption key
   */
  public final hydra.util.Opt<String> keyMetadata;
  
  public EncryptionWithColumnKey (java.util.List<String> pathInSchema, hydra.util.Opt<String> keyMetadata) {
    java.util.Objects.requireNonNull((pathInSchema));
    java.util.Objects.requireNonNull((keyMetadata));
    this.pathInSchema = pathInSchema;
    this.keyMetadata = keyMetadata;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EncryptionWithColumnKey)) {
      return false;
    }
    EncryptionWithColumnKey o = (EncryptionWithColumnKey) (other);
    return pathInSchema.equals(o.pathInSchema) && keyMetadata.equals(o.keyMetadata);
  }
  
  @Override
  public int hashCode() {
    return 2 * pathInSchema.hashCode() + 3 * keyMetadata.hashCode();
  }
  
  public EncryptionWithColumnKey withPathInSchema(java.util.List<String> pathInSchema) {
    java.util.Objects.requireNonNull((pathInSchema));
    return new EncryptionWithColumnKey(pathInSchema, keyMetadata);
  }
  
  public EncryptionWithColumnKey withKeyMetadata(hydra.util.Opt<String> keyMetadata) {
    java.util.Objects.requireNonNull((keyMetadata));
    return new EncryptionWithColumnKey(pathInSchema, keyMetadata);
  }
}