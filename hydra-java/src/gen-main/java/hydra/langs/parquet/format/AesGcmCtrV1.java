// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

public class AesGcmCtrV1 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.AesGcmCtrV1");
  
  public final hydra.util.Opt<String> aadPrefix;
  
  public final hydra.util.Opt<String> aadFileUnique;
  
  /**
   * In files encrypted with AAD prefix without storing it, readers must supply the prefix
   */
  public final hydra.util.Opt<Boolean> supplyAadPrefix;
  
  public AesGcmCtrV1 (hydra.util.Opt<String> aadPrefix, hydra.util.Opt<String> aadFileUnique, hydra.util.Opt<Boolean> supplyAadPrefix) {
    if (aadPrefix == null) {
      throw new IllegalArgumentException("null value for 'aadPrefix' argument");
    }
    if (aadFileUnique == null) {
      throw new IllegalArgumentException("null value for 'aadFileUnique' argument");
    }
    if (supplyAadPrefix == null) {
      throw new IllegalArgumentException("null value for 'supplyAadPrefix' argument");
    }
    this.aadPrefix = aadPrefix;
    this.aadFileUnique = aadFileUnique;
    this.supplyAadPrefix = supplyAadPrefix;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AesGcmCtrV1)) {
      return false;
    }
    AesGcmCtrV1 o = (AesGcmCtrV1) (other);
    return aadPrefix.equals(o.aadPrefix) && aadFileUnique.equals(o.aadFileUnique) && supplyAadPrefix.equals(o.supplyAadPrefix);
  }
  
  @Override
  public int hashCode() {
    return 2 * aadPrefix.hashCode() + 3 * aadFileUnique.hashCode() + 5 * supplyAadPrefix.hashCode();
  }
  
  public AesGcmCtrV1 withAadPrefix(hydra.util.Opt<String> aadPrefix) {
    if (aadPrefix == null) {
      throw new IllegalArgumentException("null value for 'aadPrefix' argument");
    }
    return new AesGcmCtrV1(aadPrefix, aadFileUnique, supplyAadPrefix);
  }
  
  public AesGcmCtrV1 withAadFileUnique(hydra.util.Opt<String> aadFileUnique) {
    if (aadFileUnique == null) {
      throw new IllegalArgumentException("null value for 'aadFileUnique' argument");
    }
    return new AesGcmCtrV1(aadPrefix, aadFileUnique, supplyAadPrefix);
  }
  
  public AesGcmCtrV1 withSupplyAadPrefix(hydra.util.Opt<Boolean> supplyAadPrefix) {
    if (supplyAadPrefix == null) {
      throw new IllegalArgumentException("null value for 'supplyAadPrefix' argument");
    }
    return new AesGcmCtrV1(aadPrefix, aadFileUnique, supplyAadPrefix);
  }
}