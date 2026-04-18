// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Fixpoint command for recursive definitions
 */
public class FixpointDefinition implements Serializable, Comparable<FixpointDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.FixpointDefinition");

  public static final hydra.core.Name LOCALITY = new hydra.core.Name("locality");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name ANNOT = new hydra.core.Name("annot");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

  public final hydra.util.Maybe<hydra.coq.syntax.Locality> locality;

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.util.Maybe<hydra.coq.syntax.FixAnnot> annot;

  public final hydra.util.Maybe<hydra.coq.syntax.Type> type;

  public final hydra.coq.syntax.Term body;

  public final java.util.List<hydra.coq.syntax.Fix_Decl> with;

  public FixpointDefinition (hydra.util.Maybe<hydra.coq.syntax.Locality> locality, hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Binder> binders, hydra.util.Maybe<hydra.coq.syntax.FixAnnot> annot, hydra.util.Maybe<hydra.coq.syntax.Type> type, hydra.coq.syntax.Term body, java.util.List<hydra.coq.syntax.Fix_Decl> with) {
    this.locality = locality;
    this.name = name;
    this.binders = binders;
    this.annot = annot;
    this.type = type;
    this.body = body;
    this.with = with;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixpointDefinition)) {
      return false;
    }
    FixpointDefinition o = (FixpointDefinition) other;
    return java.util.Objects.equals(
      this.locality,
      o.locality) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.annot,
      o.annot) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.with,
      o.with);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(locality) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(binders) + 7 * java.util.Objects.hashCode(annot) + 11 * java.util.Objects.hashCode(type) + 13 * java.util.Objects.hashCode(body) + 17 * java.util.Objects.hashCode(with);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FixpointDefinition other) {
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
      annot,
      other.annot);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      with,
      other.with);
  }

  public FixpointDefinition withLocality(hydra.util.Maybe<hydra.coq.syntax.Locality> locality) {
    return new FixpointDefinition(locality, name, binders, annot, type, body, with);
  }

  public FixpointDefinition withName(hydra.coq.syntax.Ident name) {
    return new FixpointDefinition(locality, name, binders, annot, type, body, with);
  }

  public FixpointDefinition withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new FixpointDefinition(locality, name, binders, annot, type, body, with);
  }

  public FixpointDefinition withAnnot(hydra.util.Maybe<hydra.coq.syntax.FixAnnot> annot) {
    return new FixpointDefinition(locality, name, binders, annot, type, body, with);
  }

  public FixpointDefinition withType(hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    return new FixpointDefinition(locality, name, binders, annot, type, body, with);
  }

  public FixpointDefinition withBody(hydra.coq.syntax.Term body) {
    return new FixpointDefinition(locality, name, binders, annot, type, body, with);
  }

  public FixpointDefinition withWith(java.util.List<hydra.coq.syntax.Fix_Decl> with) {
    return new FixpointDefinition(locality, name, binders, annot, type, body, with);
  }
}
