// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DropSchemaStatement implements Serializable, Comparable<DropSchemaStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DropSchemaStatement");

  public static final hydra.core.Name IF_EXISTS = new hydra.core.Name("ifExists");

  public static final hydra.core.Name PARENT_AND_NAME = new hydra.core.Name("parentAndName");

  public final Boolean ifExists;

  public final openGql.grammar.AbsoluteDirectoryAndSchema parentAndName;

  public DropSchemaStatement (Boolean ifExists, openGql.grammar.AbsoluteDirectoryAndSchema parentAndName) {
    this.ifExists = ifExists;
    this.parentAndName = parentAndName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DropSchemaStatement)) {
      return false;
    }
    DropSchemaStatement o = (DropSchemaStatement) other;
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
  public int compareTo(DropSchemaStatement other) {
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

  public DropSchemaStatement withIfExists(Boolean ifExists) {
    return new DropSchemaStatement(ifExists, parentAndName);
  }

  public DropSchemaStatement withParentAndName(openGql.grammar.AbsoluteDirectoryAndSchema parentAndName) {
    return new DropSchemaStatement(ifExists, parentAndName);
  }
}
