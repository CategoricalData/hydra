// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class TypeBinders implements Serializable, Comparable<TypeBinders> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.TypeBinders");

  public static final hydra.core.Name NAMES = new hydra.core.Name("names");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public final java.util.List<hydra.coq.syntax.Name> names;

  public final hydra.coq.syntax.Type type;

  public TypeBinders (java.util.List<hydra.coq.syntax.Name> names, hydra.coq.syntax.Type type) {
    this.names = names;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeBinders)) {
      return false;
    }
    TypeBinders o = (TypeBinders) other;
    return java.util.Objects.equals(
      this.names,
      o.names) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(names) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeBinders other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      names,
      other.names);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public TypeBinders withNames(java.util.List<hydra.coq.syntax.Name> names) {
    return new TypeBinders(names, type);
  }

  public TypeBinders withType(hydra.coq.syntax.Type type) {
    return new TypeBinders(names, type);
  }
}
