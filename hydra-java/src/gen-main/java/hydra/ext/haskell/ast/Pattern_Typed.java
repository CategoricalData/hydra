package hydra.ext.haskell.ast;

public class Pattern_Typed {
  public final Pattern inner;
  
  public final Type type;
  
  public Pattern_Typed (Pattern inner, Type type) {
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern_Typed)) {
      return false;
    }
    Pattern_Typed o = (Pattern_Typed) (other);
    return inner.equals(o.inner) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * type.hashCode();
  }
  
  public Pattern_Typed withInner(Pattern inner) {
    return new Pattern_Typed(inner, type);
  }
  
  public Pattern_Typed withType(Type type) {
    return new Pattern_Typed(inner, type);
  }
}