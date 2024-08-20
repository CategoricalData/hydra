// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

public class AesGcmCtrV1 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/parquet/format.AesGcmCtrV1");
  
  public static final hydra.core.Name FIELD_NAME_AAD_PREFIX = new hydra.core.Name("aadPrefix");
  
  public static final hydra.core.Name FIELD_NAME_AAD_FILE_UNIQUE = new hydra.core.Name("aadFileUnique");
  
  public static final hydra.core.Name FIELD_NAME_SUPPLY_AAD_PREFIX = new hydra.core.Name("supplyAadPrefix");
  
  /**
   * AAD prefix
   */
  public final hydra.util.Opt<String> aadPrefix;
  
  /**
   * Unique file identifier part of AAD suffix
   */
  public final hydra.util.Opt<String> aadFileUnique;
  
  /**
   * In files encrypted with AAD prefix without storing it, readers must supply the prefix
   */
  public final hydra.util.Opt<Boolean> supplyAadPrefix;
  
  public AesGcmCtrV1 (hydra.util.Opt<String> aadPrefix, hydra.util.Opt<String> aadFileUnique, hydra.util.Opt<Boolean> supplyAadPrefix) {
    java.util.Objects.requireNonNull((aadPrefix));
    java.util.Objects.requireNonNull((aadFileUnique));
    java.util.Objects.requireNonNull((supplyAadPrefix));
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
    java.util.Objects.requireNonNull((aadPrefix));
    return new AesGcmCtrV1(aadPrefix, aadFileUnique, supplyAadPrefix);
  }
  
  public AesGcmCtrV1 withAadFileUnique(hydra.util.Opt<String> aadFileUnique) {
    java.util.Objects.requireNonNull((aadFileUnique));
    return new AesGcmCtrV1(aadPrefix, aadFileUnique, supplyAadPrefix);
  }
  
  public AesGcmCtrV1 withSupplyAadPrefix(hydra.util.Opt<Boolean> supplyAadPrefix) {
    java.util.Objects.requireNonNull((supplyAadPrefix));
    return new AesGcmCtrV1(aadPrefix, aadFileUnique, supplyAadPrefix);
  }
}