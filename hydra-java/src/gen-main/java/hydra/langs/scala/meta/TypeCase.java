// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class TypeCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.TypeCase");
  
  public static final hydra.core.Name FIELD_NAME_PAT = new hydra.core.Name("pat");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.langs.scala.meta.Type pat;
  
  public final hydra.langs.scala.meta.Type body;
  
  public TypeCase (hydra.langs.scala.meta.Type pat, hydra.langs.scala.meta.Type body) {
    java.util.Objects.requireNonNull((pat));
    java.util.Objects.requireNonNull((body));
    this.pat = pat;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeCase)) {
      return false;
    }
    TypeCase o = (TypeCase) (other);
    return pat.equals(o.pat) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * pat.hashCode() + 3 * body.hashCode();
  }
  
  public TypeCase withPat(hydra.langs.scala.meta.Type pat) {
    java.util.Objects.requireNonNull((pat));
    return new TypeCase(pat, body);
  }
  
  public TypeCase withBody(hydra.langs.scala.meta.Type body) {
    java.util.Objects.requireNonNull((body));
    return new TypeCase(pat, body);
  }
}