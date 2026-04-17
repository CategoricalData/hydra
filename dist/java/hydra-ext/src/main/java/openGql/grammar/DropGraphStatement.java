// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DropGraphStatement implements Serializable, Comparable<DropGraphStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DropGraphStatement");

  public static final hydra.core.Name IF_EXISTS = new hydra.core.Name("ifExists");

  public static final hydra.core.Name PARENT_AND_NAME = new hydra.core.Name("parentAndName");

  public final Boolean ifExists;

  public final openGql.grammar.CatalogGraphParentAndName parentAndName;

  public DropGraphStatement (Boolean ifExists, openGql.grammar.CatalogGraphParentAndName parentAndName) {
    this.ifExists = ifExists;
    this.parentAndName = parentAndName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DropGraphStatement)) {
      return false;
    }
    DropGraphStatement o = (DropGraphStatement) other;
    return java.util.Objects.equals(
      this.ifExists,
      o.ifExists) && java.util.Objects.equals(
      this.parentAndName,
      o.parentAndName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ifExists) + 3 * java.util.Objects.hashCode(parentAndName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DropGraphStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ifExists,
      other.ifExists);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      parentAndName,
      other.parentAndName);
  }

  public DropGraphStatement withIfExists(Boolean ifExists) {
    return new DropGraphStatement(ifExists, parentAndName);
  }

  public DropGraphStatement withParentAndName(openGql.grammar.CatalogGraphParentAndName parentAndName) {
    return new DropGraphStatement(ifExists, parentAndName);
  }
}
