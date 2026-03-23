// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Match implements Serializable, Comparable<Type_Match> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Match");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public static final hydra.core.Name CASES = new hydra.core.Name("cases");

  public final hydra.ext.scala.meta.Type tpe;

  public final hydra.util.ConsList<hydra.ext.scala.meta.TypeCase> cases;

  public Type_Match (hydra.ext.scala.meta.Type tpe, hydra.util.ConsList<hydra.ext.scala.meta.TypeCase> cases) {
    this.tpe = tpe;
    this.cases = cases;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Match)) {
      return false;
    }
    Type_Match o = (Type_Match) other;
    return java.util.Objects.equals(
      this.tpe,
      o.tpe) && java.util.Objects.equals(
      this.cases,
      o.cases);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tpe) + 3 * java.util.Objects.hashCode(cases);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Match other) {
    int cmp = 0;
    cmp = ((Comparable) tpe).compareTo(other.tpe);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) cases).compareTo(other.cases);
  }

  public Type_Match withTpe(hydra.ext.scala.meta.Type tpe) {
    return new Type_Match(tpe, cases);
  }

  public Type_Match withCases(hydra.util.ConsList<hydra.ext.scala.meta.TypeCase> cases) {
    return new Type_Match(tpe, cases);
  }
}
