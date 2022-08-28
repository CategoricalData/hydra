package hydra.ext.scala.meta;

public class Enumerator_Val {
  public final hydra.ext.scala.meta.Pat pat;
  
  public final hydra.ext.scala.meta.Data rhs;
  
  public Enumerator_Val (hydra.ext.scala.meta.Pat pat, hydra.ext.scala.meta.Data rhs) {
    this.pat = pat;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Val)) {
      return false;
    }
    Enumerator_Val o = (Enumerator_Val) (other);
    return pat.equals(o.pat) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * pat.hashCode() + 3 * rhs.hashCode();
  }
  
  public Enumerator_Val withPat(hydra.ext.scala.meta.Pat pat) {
    return new Enumerator_Val(pat, rhs);
  }
  
  public Enumerator_Val withRhs(hydra.ext.scala.meta.Data rhs) {
    return new Enumerator_Val(pat, rhs);
  }
}