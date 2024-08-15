// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_Match implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.Match");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  public final hydra.langs.scala.meta.Type tpe;
  
  public final java.util.List<hydra.langs.scala.meta.TypeCase> cases;
  
  public Type_Match (hydra.langs.scala.meta.Type tpe, java.util.List<hydra.langs.scala.meta.TypeCase> cases) {
    java.util.Objects.requireNonNull((tpe));
    java.util.Objects.requireNonNull((cases));
    this.tpe = tpe;
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Match)) {
      return false;
    }
    Type_Match o = (Type_Match) (other);
    return tpe.equals(o.tpe) && cases.equals(o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode() + 3 * cases.hashCode();
  }
  
  public Type_Match withTpe(hydra.langs.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    return new Type_Match(tpe, cases);
  }
  
  public Type_Match withCases(java.util.List<hydra.langs.scala.meta.TypeCase> cases) {
    java.util.Objects.requireNonNull((cases));
    return new Type_Match(tpe, cases);
  }
}