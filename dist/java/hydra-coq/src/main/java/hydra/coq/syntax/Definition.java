// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Definition or Let command: Definition name binders : type := term.
 */
public class Definition implements Serializable, Comparable<Definition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Definition");

  public static final hydra.core.Name LOCALITY = new hydra.core.Name("locality");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.util.Maybe<hydra.coq.syntax.Locality> locality;

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.util.Maybe<hydra.coq.syntax.Type> type;

  public final hydra.coq.syntax.Term body;

  public Definition (hydra.util.Maybe<hydra.coq.syntax.Locality> locality, hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Binder> binders, hydra.util.Maybe<hydra.coq.syntax.Type> type, hydra.coq.syntax.Term body) {
    this.locality = locality;
    this.name = name;
    this.binders = binders;
    this.type = type;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Definition)) {
      return false;
    }
    Definition o = (Definition) other;
    return java.util.Objects.equals(
      this.locality,
      o.locality) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(locality) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(binders) + 7 * java.util.Objects.hashCode(type) + 11 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Definition other) {
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
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Definition withLocality(hydra.util.Maybe<hydra.coq.syntax.Locality> locality) {
    return new Definition(locality, name, binders, type, body);
  }

  public Definition withName(hydra.coq.syntax.Ident name) {
    return new Definition(locality, name, binders, type, body);
  }

  public Definition withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new Definition(locality, name, binders, type, body);
  }

  public Definition withType(hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    return new Definition(locality, name, binders, type, body);
  }

  public Definition withBody(hydra.coq.syntax.Term body) {
    return new Definition(locality, name, binders, type, body);
  }
}
