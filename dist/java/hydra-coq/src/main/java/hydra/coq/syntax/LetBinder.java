// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * Some constructions allow the binding of a variable to value. This is called a 'let-binder'.
 */
public class LetBinder implements Serializable, Comparable<LetBinder> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.LetBinder");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final hydra.coq.syntax.Name name;

  public final hydra.util.Maybe<hydra.coq.syntax.Type> type;

  public final hydra.coq.syntax.Term term;

  public LetBinder (hydra.coq.syntax.Name name, hydra.util.Maybe<hydra.coq.syntax.Type> type, hydra.coq.syntax.Term term) {
    this.name = name;
    this.type = type;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetBinder)) {
      return false;
    }
    LetBinder o = (LetBinder) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetBinder other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
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
      term,
      other.term);
  }

  public LetBinder withName(hydra.coq.syntax.Name name) {
    return new LetBinder(name, type, term);
  }

  public LetBinder withType(hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    return new LetBinder(name, type, term);
  }

  public LetBinder withTerm(hydra.coq.syntax.Term term) {
    return new LetBinder(name, type, term);
  }
}
