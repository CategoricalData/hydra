// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Case implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Case");
  
  public final hydra.langs.scala.meta.Pat pat;
  
  public final hydra.util.Opt<hydra.langs.scala.meta.Data> cond;
  
  public final hydra.langs.scala.meta.Data body;
  
  public Case (hydra.langs.scala.meta.Pat pat, hydra.util.Opt<hydra.langs.scala.meta.Data> cond, hydra.langs.scala.meta.Data body) {
    if (pat == null) {
      throw new IllegalArgumentException("null value for 'pat' argument");
    }
    if (cond == null) {
      throw new IllegalArgumentException("null value for 'cond' argument");
    }
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
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
  
  public Case withPat(hydra.langs.scala.meta.Pat pat) {
    if (pat == null) {
      throw new IllegalArgumentException("null value for 'pat' argument");
    }
    return new Case(pat, cond, body);
  }
  
  public Case withCond(hydra.util.Opt<hydra.langs.scala.meta.Data> cond) {
    if (cond == null) {
      throw new IllegalArgumentException("null value for 'cond' argument");
    }
    return new Case(pat, cond, body);
  }
  
  public Case withBody(hydra.langs.scala.meta.Data body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new Case(pat, cond, body);
  }
}