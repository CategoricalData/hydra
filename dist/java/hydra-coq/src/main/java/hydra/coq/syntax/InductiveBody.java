// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A single body in an Inductive definition (supports mutual induction via 'with')
 */
public class InductiveBody implements Serializable, Comparable<InductiveBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.InductiveBody");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name CONSTRUCTORS = new hydra.core.Name("constructors");

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.util.Maybe<hydra.coq.syntax.Type> type;

  public final java.util.List<hydra.coq.syntax.Constructor> constructors;

  public InductiveBody (hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Binder> binders, hydra.util.Maybe<hydra.coq.syntax.Type> type, java.util.List<hydra.coq.syntax.Constructor> constructors) {
    this.name = name;
    this.binders = binders;
    this.type = type;
    this.constructors = constructors;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InductiveBody)) {
      return false;
    }
    InductiveBody o = (InductiveBody) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.constructors,
      o.constructors);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(binders) + 5 * java.util.Objects.hashCode(type) + 7 * java.util.Objects.hashCode(constructors);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InductiveBody other) {
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
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      constructors,
      other.constructors);
  }

  public InductiveBody withName(hydra.coq.syntax.Ident name) {
    return new InductiveBody(name, binders, type, constructors);
  }

  public InductiveBody withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new InductiveBody(name, binders, type, constructors);
  }

  public InductiveBody withType(hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    return new InductiveBody(name, binders, type, constructors);
  }

  public InductiveBody withConstructors(java.util.List<hydra.coq.syntax.Constructor> constructors) {
    return new InductiveBody(name, binders, type, constructors);
  }
}
