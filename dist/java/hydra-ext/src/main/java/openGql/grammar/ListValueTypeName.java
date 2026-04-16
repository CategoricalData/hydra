// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ListValueTypeName implements Serializable, Comparable<ListValueTypeName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ListValueTypeName");

  public static final hydra.core.Name GROUP = new hydra.core.Name("group");

  public static final hydra.core.Name SYNONYM = new hydra.core.Name("synonym");

  public final Boolean group;

  public final openGql.grammar.ListValueTypeNameSynonym synonym;

  public ListValueTypeName (Boolean group, openGql.grammar.ListValueTypeNameSynonym synonym) {
    this.group = group;
    this.synonym = synonym;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListValueTypeName)) {
      return false;
    }
    ListValueTypeName o = (ListValueTypeName) other;
    return java.util.Objects.equals(
      this.group,
      o.group) && java.util.Objects.equals(
      this.synonym,
      o.synonym);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(group) + 3 * java.util.Objects.hashCode(synonym);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ListValueTypeName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      group,
      other.group);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      synonym,
      other.synonym);
  }

  public ListValueTypeName withGroup(Boolean group) {
    return new ListValueTypeName(group, synonym);
  }

  public ListValueTypeName withSynonym(openGql.grammar.ListValueTypeNameSynonym synonym) {
    return new ListValueTypeName(group, synonym);
  }
}
