// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * String functions/keywords only found in OpenCypher
 */
public class StringFeatures implements Serializable, Comparable<StringFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.StringFeatures");

  public static final hydra.core.Name CONTAINS = new hydra.core.Name("contains");

  public static final hydra.core.Name ENDS_WITH = new hydra.core.Name("endsWith");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

  public static final hydra.core.Name STARTS_WITH = new hydra.core.Name("startsWith");

  /**
   * The contains() function / CONTAINS
   */
  public final Boolean contains;

  /**
   * The endsWith() function / ENDS WITH
   */
  public final Boolean endsWith;

  /**
   * The in() function / IN
   */
  public final Boolean in;

  /**
   * The startsWith() function / STARTS WITH
   */
  public final Boolean startsWith;

  public StringFeatures (Boolean contains, Boolean endsWith, Boolean in, Boolean startsWith) {
    this.contains = contains;
    this.endsWith = endsWith;
    this.in = in;
    this.startsWith = startsWith;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringFeatures)) {
      return false;
    }
    StringFeatures o = (StringFeatures) other;
    return java.util.Objects.equals(
      this.contains,
      o.contains) && java.util.Objects.equals(
      this.endsWith,
      o.endsWith) && java.util.Objects.equals(
      this.in,
      o.in) && java.util.Objects.equals(
      this.startsWith,
      o.startsWith);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(contains) + 3 * java.util.Objects.hashCode(endsWith) + 5 * java.util.Objects.hashCode(in) + 7 * java.util.Objects.hashCode(startsWith);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      contains,
      other.contains);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      endsWith,
      other.endsWith);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      in,
      other.in);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      startsWith,
      other.startsWith);
  }

  public StringFeatures withContains(Boolean contains) {
    return new StringFeatures(contains, endsWith, in, startsWith);
  }

  public StringFeatures withEndsWith(Boolean endsWith) {
    return new StringFeatures(contains, endsWith, in, startsWith);
  }

  public StringFeatures withIn(Boolean in) {
    return new StringFeatures(contains, endsWith, in, startsWith);
  }

  public StringFeatures withStartsWith(Boolean startsWith) {
    return new StringFeatures(contains, endsWith, in, startsWith);
  }
}
