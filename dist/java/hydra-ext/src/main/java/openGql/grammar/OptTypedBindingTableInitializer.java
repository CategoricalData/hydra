// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OptTypedBindingTableInitializer implements Serializable, Comparable<OptTypedBindingTableInitializer> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OptTypedBindingTableInitializer");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name INITIALIZER = new hydra.core.Name("initializer");

  public final hydra.util.Maybe<openGql.grammar.TypedBindingTableReferenceValueType> type;

  public final java.lang.Void initializer;

  public OptTypedBindingTableInitializer (hydra.util.Maybe<openGql.grammar.TypedBindingTableReferenceValueType> type, java.lang.Void initializer) {
    this.type = type;
    this.initializer = initializer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptTypedBindingTableInitializer)) {
      return false;
    }
    OptTypedBindingTableInitializer o = (OptTypedBindingTableInitializer) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.initializer,
      o.initializer);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(initializer);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OptTypedBindingTableInitializer other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      initializer,
      other.initializer);
  }

  public OptTypedBindingTableInitializer withType(hydra.util.Maybe<openGql.grammar.TypedBindingTableReferenceValueType> type) {
    return new OptTypedBindingTableInitializer(type, initializer);
  }

  public OptTypedBindingTableInitializer withInitializer(java.lang.Void initializer) {
    return new OptTypedBindingTableInitializer(type, initializer);
  }
}
