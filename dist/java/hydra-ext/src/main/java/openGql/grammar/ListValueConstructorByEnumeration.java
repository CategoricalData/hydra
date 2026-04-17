// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ListValueConstructorByEnumeration implements Serializable, Comparable<ListValueConstructorByEnumeration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ListValueConstructorByEnumeration");

  public static final hydra.core.Name LIST_VALUE_TYPE_NAME = new hydra.core.Name("listValueTypeName");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  public final hydra.util.Maybe<openGql.grammar.ListValueTypeName> listValueTypeName;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.ValueExpression>> elements;

  public ListValueConstructorByEnumeration (hydra.util.Maybe<openGql.grammar.ListValueTypeName> listValueTypeName, hydra.util.Maybe<java.util.List<openGql.grammar.ValueExpression>> elements) {
    this.listValueTypeName = listValueTypeName;
    this.elements = elements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListValueConstructorByEnumeration)) {
      return false;
    }
    ListValueConstructorByEnumeration o = (ListValueConstructorByEnumeration) other;
    return java.util.Objects.equals(
      this.listValueTypeName,
      o.listValueTypeName) && java.util.Objects.equals(
      this.elements,
      o.elements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listValueTypeName) + 3 * java.util.Objects.hashCode(elements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ListValueConstructorByEnumeration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      listValueTypeName,
      other.listValueTypeName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      elements,
      other.elements);
  }

  public ListValueConstructorByEnumeration withListValueTypeName(hydra.util.Maybe<openGql.grammar.ListValueTypeName> listValueTypeName) {
    return new ListValueConstructorByEnumeration(listValueTypeName, elements);
  }

  public ListValueConstructorByEnumeration withElements(hydra.util.Maybe<java.util.List<openGql.grammar.ValueExpression>> elements) {
    return new ListValueConstructorByEnumeration(listValueTypeName, elements);
  }
}
