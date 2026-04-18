// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CreateGraphTypeStatement implements Serializable, Comparable<CreateGraphTypeStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CreateGraphTypeStatement");

  public static final hydra.core.Name CREATE_OPTION = new hydra.core.Name("createOption");

  public static final hydra.core.Name PARENT_AND_NAME = new hydra.core.Name("parentAndName");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final openGql.grammar.CreateGraphTypeOption createOption;

  public final openGql.grammar.CatalogGraphTypeParentAndName parentAndName;

  public final openGql.grammar.GraphTypeSource source;

  public CreateGraphTypeStatement (openGql.grammar.CreateGraphTypeOption createOption, openGql.grammar.CatalogGraphTypeParentAndName parentAndName, openGql.grammar.GraphTypeSource source) {
    this.createOption = createOption;
    this.parentAndName = parentAndName;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CreateGraphTypeStatement)) {
      return false;
    }
    CreateGraphTypeStatement o = (CreateGraphTypeStatement) other;
    return java.util.Objects.equals(
      this.createOption,
      o.createOption) && java.util.Objects.equals(
      this.parentAndName,
      o.parentAndName) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(createOption) + 3 * java.util.Objects.hashCode(parentAndName) + 5 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CreateGraphTypeStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      createOption,
      other.createOption);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      parentAndName,
      other.parentAndName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public CreateGraphTypeStatement withCreateOption(openGql.grammar.CreateGraphTypeOption createOption) {
    return new CreateGraphTypeStatement(createOption, parentAndName, source);
  }

  public CreateGraphTypeStatement withParentAndName(openGql.grammar.CatalogGraphTypeParentAndName parentAndName) {
    return new CreateGraphTypeStatement(createOption, parentAndName, source);
  }

  public CreateGraphTypeStatement withSource(openGql.grammar.GraphTypeSource source) {
    return new CreateGraphTypeStatement(createOption, parentAndName, source);
  }
}
