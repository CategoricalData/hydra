// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CreateSchemaStatement implements Serializable, Comparable<CreateSchemaStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CreateSchemaStatement");

  public static final hydra.core.Name IF_NOT_EXISTS = new hydra.core.Name("ifNotExists");

  public static final hydra.core.Name PARENT_AND_NAME = new hydra.core.Name("parentAndName");

  public final Boolean ifNotExists;

  public final openGql.grammar.AbsoluteDirectoryAndSchema parentAndName;

  public CreateSchemaStatement (Boolean ifNotExists, openGql.grammar.AbsoluteDirectoryAndSchema parentAndName) {
    this.ifNotExists = ifNotExists;
    this.parentAndName = parentAndName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CreateSchemaStatement)) {
      return false;
    }
    CreateSchemaStatement o = (CreateSchemaStatement) other;
    return java.util.Objects.equals(
      this.ifNotExists,
      o.ifNotExists) && java.util.Objects.equals(
      this.parentAndName,
      o.parentAndName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ifNotExists) + 3 * java.util.Objects.hashCode(parentAndName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CreateSchemaStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ifNotExists,
      other.ifNotExists);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      parentAndName,
      other.parentAndName);
  }

  public CreateSchemaStatement withIfNotExists(Boolean ifNotExists) {
    return new CreateSchemaStatement(ifNotExists, parentAndName);
  }

  public CreateSchemaStatement withParentAndName(openGql.grammar.AbsoluteDirectoryAndSchema parentAndName) {
    return new CreateSchemaStatement(ifNotExists, parentAndName);
  }
}
