// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Theorem/Lemma/Proposition with a proof term
 */
public class TheoremBody implements Serializable, Comparable<TheoremBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.TheoremBody");

  public static final hydra.core.Name KIND = new hydra.core.Name("kind");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name PROOF = new hydra.core.Name("proof");

  public final hydra.coq.syntax.TheoremKind kind;

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.coq.syntax.Type type;

  public final hydra.coq.syntax.Term proof;

  public TheoremBody (hydra.coq.syntax.TheoremKind kind, hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Binder> binders, hydra.coq.syntax.Type type, hydra.coq.syntax.Term proof) {
    this.kind = kind;
    this.name = name;
    this.binders = binders;
    this.type = type;
    this.proof = proof;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TheoremBody)) {
      return false;
    }
    TheoremBody o = (TheoremBody) other;
    return java.util.Objects.equals(
      this.kind,
      o.kind) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.proof,
      o.proof);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(kind) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(binders) + 7 * java.util.Objects.hashCode(type) + 11 * java.util.Objects.hashCode(proof);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TheoremBody other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      kind,
      other.kind);
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
      proof,
      other.proof);
  }

  public TheoremBody withKind(hydra.coq.syntax.TheoremKind kind) {
    return new TheoremBody(kind, name, binders, type, proof);
  }

  public TheoremBody withName(hydra.coq.syntax.Ident name) {
    return new TheoremBody(kind, name, binders, type, proof);
  }

  public TheoremBody withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new TheoremBody(kind, name, binders, type, proof);
  }

  public TheoremBody withType(hydra.coq.syntax.Type type) {
    return new TheoremBody(kind, name, binders, type, proof);
  }

  public TheoremBody withProof(hydra.coq.syntax.Term proof) {
    return new TheoremBody(kind, name, binders, type, proof);
  }
}
