package hydra.ext.scala.meta;

public class TypeCase {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.TypeCase");
  
  public final hydra.ext.scala.meta.Type pat;
  
  public final hydra.ext.scala.meta.Type body;
  
  public TypeCase (hydra.ext.scala.meta.Type pat, hydra.ext.scala.meta.Type body) {
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
  
  public TypeCase withPat(hydra.ext.scala.meta.Type pat) {
    return new TypeCase(pat, body);
  }
  
  public TypeCase withBody(hydra.ext.scala.meta.Type body) {
    return new TypeCase(pat, body);
  }
}