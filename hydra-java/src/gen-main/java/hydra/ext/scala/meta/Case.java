// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Case implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Case");
  
  public static final hydra.core.Name FIELD_NAME_PAT = new hydra.core.Name("pat");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.scala.meta.Pat pat;
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Data> cond;
  
  public final hydra.ext.scala.meta.Data body;
  
  public Case (hydra.ext.scala.meta.Pat pat, hydra.util.Opt<hydra.ext.scala.meta.Data> cond, hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((pat));
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((body));
    this.pat = pat;
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Case)) {
      return false;
    }
    Case o = (Case) (other);
    return pat.equals(o.pat) && cond.equals(o.cond) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * pat.hashCode() + 3 * cond.hashCode() + 5 * body.hashCode();
  }
  
  public Case withPat(hydra.ext.scala.meta.Pat pat) {
    java.util.Objects.requireNonNull((pat));
    return new Case(pat, cond, body);
  }
  
  public Case withCond(hydra.util.Opt<hydra.ext.scala.meta.Data> cond) {
    java.util.Objects.requireNonNull((cond));
    return new Case(pat, cond, body);
  }
  
  public Case withBody(hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Case(pat, cond, body);
  }
}