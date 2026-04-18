// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A constructor in an Inductive definition
 */
public class Constructor implements Serializable, Comparable<Constructor> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Constructor");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.util.Maybe<hydra.coq.syntax.Type> type;

  public Constructor (hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Binder> binders, hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    this.name = name;
    this.binders = binders;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Constructor)) {
      return false;
    }
    Constructor o = (Constructor) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(binders) + 5 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Constructor other) {
    int cmp = 0;
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
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public Constructor withName(hydra.coq.syntax.Ident name) {
    return new Constructor(name, binders, type);
  }

  public Constructor withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new Constructor(name, binders, type);
  }

  public Constructor withType(hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    return new Constructor(name, binders, type);
  }
}
