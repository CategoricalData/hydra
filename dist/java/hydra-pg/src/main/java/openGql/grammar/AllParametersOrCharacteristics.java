// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AllParametersOrCharacteristics implements Serializable, Comparable<AllParametersOrCharacteristics> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AllParametersOrCharacteristics");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public final Boolean all;

  public final openGql.grammar.ParametersOrCharacteristics type;

  public AllParametersOrCharacteristics (Boolean all, openGql.grammar.ParametersOrCharacteristics type) {
    this.all = all;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AllParametersOrCharacteristics)) {
      return false;
    }
    AllParametersOrCharacteristics o = (AllParametersOrCharacteristics) other;
    return java.util.Objects.equals(
      this.all,
      o.all) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(all) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AllParametersOrCharacteristics other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      all,
      other.all);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public AllParametersOrCharacteristics withAll(Boolean all) {
    return new AllParametersOrCharacteristics(all, type);
  }

  public AllParametersOrCharacteristics withType(openGql.grammar.ParametersOrCharacteristics type) {
    return new AllParametersOrCharacteristics(all, type);
  }
}
