// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Cofix implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Cofix");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_QUAL = new hydra.core.Name("qual");
  
  public final hydra.ext.fr.inria.coq.syntax.CofixBody body;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.CofixQual> qual;
  
  public Cofix (hydra.ext.fr.inria.coq.syntax.CofixBody body, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.CofixQual> qual) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((qual));
    this.body = body;
    this.qual = qual;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Cofix)) {
      return false;
    }
    Cofix o = (Cofix) (other);
    return body.equals(o.body) && qual.equals(o.qual);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * qual.hashCode();
  }
  
  public Cofix withBody(hydra.ext.fr.inria.coq.syntax.CofixBody body) {
    java.util.Objects.requireNonNull((body));
    return new Cofix(body, qual);
  }
  
  public Cofix withQual(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.CofixQual> qual) {
    java.util.Objects.requireNonNull((qual));
    return new Cofix(body, qual);
  }
}