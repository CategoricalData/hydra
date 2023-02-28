package hydra.langs.parquet.format;

public class EncryptionWithFooterKey {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.EncryptionWithFooterKey");
  
  public EncryptionWithFooterKey () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EncryptionWithFooterKey)) {
      return false;
    }
    EncryptionWithFooterKey o = (EncryptionWithFooterKey) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}