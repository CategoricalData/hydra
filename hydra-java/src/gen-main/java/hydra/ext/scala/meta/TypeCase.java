package hydra.ext.scala.meta;

public class TypeCase {
  public final Type pat;
  
  public final Type body;
  
  public TypeCase (Type pat, Type body) {
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
  
  public TypeCase withPat(Type pat) {
    return new TypeCase(pat, body);
  }
  
  public TypeCase withBody(Type body) {
    return new TypeCase(pat, body);
  }
}