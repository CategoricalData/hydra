// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Record or Structure definition
 */
public class RecordDefinition implements Serializable, Comparable<RecordDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.RecordDefinition");

  public static final hydra.core.Name LOCALITY = new hydra.core.Name("locality");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name SORT = new hydra.core.Name("sort");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.util.Maybe<hydra.coq.syntax.Locality> locality;

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.util.Maybe<hydra.coq.syntax.Sort> sort;

  public final hydra.coq.syntax.RecordBody body;

  public RecordDefinition (hydra.util.Maybe<hydra.coq.syntax.Locality> locality, hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Binder> binders, hydra.util.Maybe<hydra.coq.syntax.Sort> sort, hydra.coq.syntax.RecordBody body) {
    this.locality = locality;
    this.name = name;
    this.binders = binders;
    this.sort = sort;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordDefinition)) {
      return false;
    }
    RecordDefinition o = (RecordDefinition) other;
    return java.util.Objects.equals(
      this.locality,
      o.locality) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.sort,
      o.sort) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(locality) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(binders) + 7 * java.util.Objects.hashCode(sort) + 11 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RecordDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      locality,
      other.locality);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      binders,
      other.binders);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      sort,
      other.sort);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public RecordDefinition withLocality(hydra.util.Maybe<hydra.coq.syntax.Locality> locality) {
    return new RecordDefinition(locality, name, binders, sort, body);
  }

  public RecordDefinition withName(hydra.coq.syntax.Ident name) {
    return new RecordDefinition(locality, name, binders, sort, body);
  }

  public RecordDefinition withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new RecordDefinition(locality, name, binders, sort, body);
  }

  public RecordDefinition withSort(hydra.util.Maybe<hydra.coq.syntax.Sort> sort) {
    return new RecordDefinition(locality, name, binders, sort, body);
  }

  public RecordDefinition withBody(hydra.coq.syntax.RecordBody body) {
    return new RecordDefinition(locality, name, binders, sort, body);
  }
}
