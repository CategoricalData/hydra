// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Extract properties from path elements
 */
public class PropertyExtraction implements Serializable, Comparable<PropertyExtraction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.PropertyExtraction");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final hydra.util.Maybe<String> alias;

  public final com.gdblab.pathAlgebra.expressions.PropertySource source;

  public PropertyExtraction (hydra.util.Maybe<String> alias, com.gdblab.pathAlgebra.expressions.PropertySource source) {
    this.alias = alias;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyExtraction)) {
      return false;
    }
    PropertyExtraction o = (PropertyExtraction) other;
    return java.util.Objects.equals(
      this.alias,
      o.alias) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(alias) + 3 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyExtraction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      alias,
      other.alias);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public PropertyExtraction withAlias(hydra.util.Maybe<String> alias) {
    return new PropertyExtraction(alias, source);
  }

  public PropertyExtraction withSource(com.gdblab.pathAlgebra.expressions.PropertySource source) {
    return new PropertyExtraction(alias, source);
  }
}
