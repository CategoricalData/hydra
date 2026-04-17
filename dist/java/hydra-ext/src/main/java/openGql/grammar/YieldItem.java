// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class YieldItem implements Serializable, Comparable<YieldItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.YieldItem");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public final String name;

  public final hydra.util.Maybe<String> alias;

  public YieldItem (String name, hydra.util.Maybe<String> alias) {
    this.name = name;
    this.alias = alias;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof YieldItem)) {
      return false;
    }
    YieldItem o = (YieldItem) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.alias,
      o.alias);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(alias);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(YieldItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      alias,
      other.alias);
  }

  public YieldItem withName(String name) {
    return new YieldItem(name, alias);
  }

  public YieldItem withAlias(hydra.util.Maybe<String> alias) {
    return new YieldItem(name, alias);
  }
}
