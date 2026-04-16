// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CreateGraphStatement implements Serializable, Comparable<CreateGraphStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CreateGraphStatement");

  public static final hydra.core.Name CREATE_OPTION = new hydra.core.Name("createOption");

  public static final hydra.core.Name PARENT_AND_NAME = new hydra.core.Name("parentAndName");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final openGql.grammar.CreateGraphOption createOption;

  public final openGql.grammar.CatalogGraphParentAndName parentAndName;

  public final openGql.grammar.GraphTypeOption type;

  public final hydra.util.Maybe<openGql.grammar.GraphExpression> source;

  public CreateGraphStatement (openGql.grammar.CreateGraphOption createOption, openGql.grammar.CatalogGraphParentAndName parentAndName, openGql.grammar.GraphTypeOption type, hydra.util.Maybe<openGql.grammar.GraphExpression> source) {
    this.createOption = createOption;
    this.parentAndName = parentAndName;
    this.type = type;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CreateGraphStatement)) {
      return false;
    }
    CreateGraphStatement o = (CreateGraphStatement) other;
    return java.util.Objects.equals(
      this.createOption,
      o.createOption) && java.util.Objects.equals(
      this.parentAndName,
      o.parentAndName) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(createOption) + 3 * java.util.Objects.hashCode(parentAndName) + 5 * java.util.Objects.hashCode(type) + 7 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CreateGraphStatement other) {
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
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public CreateGraphStatement withCreateOption(openGql.grammar.CreateGraphOption createOption) {
    return new CreateGraphStatement(createOption, parentAndName, type, source);
  }

  public CreateGraphStatement withParentAndName(openGql.grammar.CatalogGraphParentAndName parentAndName) {
    return new CreateGraphStatement(createOption, parentAndName, type, source);
  }

  public CreateGraphStatement withType(openGql.grammar.GraphTypeOption type) {
    return new CreateGraphStatement(createOption, parentAndName, type, source);
  }

  public CreateGraphStatement withSource(hydra.util.Maybe<openGql.grammar.GraphExpression> source) {
    return new CreateGraphStatement(createOption, parentAndName, type, source);
  }
}
