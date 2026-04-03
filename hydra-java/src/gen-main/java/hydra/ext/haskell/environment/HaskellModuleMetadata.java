// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.environment;

import java.io.Serializable;

/**
 * Metadata used to determine which standard imports are needed in a generated Haskell module
 */
public class HaskellModuleMetadata implements Serializable, Comparable<HaskellModuleMetadata> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.environment.HaskellModuleMetadata");

  public static final hydra.core.Name USES_BYTE_STRING = new hydra.core.Name("usesByteString");

  public static final hydra.core.Name USES_INT = new hydra.core.Name("usesInt");

  public static final hydra.core.Name USES_MAP = new hydra.core.Name("usesMap");

  public static final hydra.core.Name USES_SET = new hydra.core.Name("usesSet");

  /**
   * Whether the module uses Data.ByteString (B.ByteString)
   */
  public final Boolean usesByteString;

  /**
   * Whether the module uses Data.Int (I.Int8, I.Int16, I.Int64)
   */
  public final Boolean usesInt;

  /**
   * Whether the module uses Data.Map (M.Map, M.fromList, M.empty)
   */
  public final Boolean usesMap;

  /**
   * Whether the module uses Data.Set (S.Set, S.fromList, S.empty)
   */
  public final Boolean usesSet;

  public HaskellModuleMetadata (Boolean usesByteString, Boolean usesInt, Boolean usesMap, Boolean usesSet) {
    this.usesByteString = usesByteString;
    this.usesInt = usesInt;
    this.usesMap = usesMap;
    this.usesSet = usesSet;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HaskellModuleMetadata)) {
      return false;
    }
    HaskellModuleMetadata o = (HaskellModuleMetadata) other;
    return java.util.Objects.equals(
      this.usesByteString,
      o.usesByteString) && java.util.Objects.equals(
      this.usesInt,
      o.usesInt) && java.util.Objects.equals(
      this.usesMap,
      o.usesMap) && java.util.Objects.equals(
      this.usesSet,
      o.usesSet);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(usesByteString) + 3 * java.util.Objects.hashCode(usesInt) + 5 * java.util.Objects.hashCode(usesMap) + 7 * java.util.Objects.hashCode(usesSet);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(HaskellModuleMetadata other) {
    int cmp = 0;
    cmp = ((Comparable) usesByteString).compareTo(other.usesByteString);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) usesInt).compareTo(other.usesInt);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) usesMap).compareTo(other.usesMap);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) usesSet).compareTo(other.usesSet);
  }

  public HaskellModuleMetadata withUsesByteString(Boolean usesByteString) {
    return new HaskellModuleMetadata(usesByteString, usesInt, usesMap, usesSet);
  }

  public HaskellModuleMetadata withUsesInt(Boolean usesInt) {
    return new HaskellModuleMetadata(usesByteString, usesInt, usesMap, usesSet);
  }

  public HaskellModuleMetadata withUsesMap(Boolean usesMap) {
    return new HaskellModuleMetadata(usesByteString, usesInt, usesMap, usesSet);
  }

  public HaskellModuleMetadata withUsesSet(Boolean usesSet) {
    return new HaskellModuleMetadata(usesByteString, usesInt, usesMap, usesSet);
  }
}
