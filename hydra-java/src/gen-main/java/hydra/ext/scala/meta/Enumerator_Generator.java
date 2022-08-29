package hydra.ext.scala.meta;

public class Enumerator_Generator {
  public final hydra.ext.scala.meta.Pat pat;
  
  public final hydra.ext.scala.meta.Data rhs;
  
  public Enumerator_Generator (hydra.ext.scala.meta.Pat pat, hydra.ext.scala.meta.Data rhs) {
    this.pat = pat;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Generator)) {
      return false;
    }
    Enumerator_Generator o = (Enumerator_Generator) (other);
    return pat.equals(o.pat) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * pat.hashCode() + 3 * rhs.hashCode();
  }
  
  public Enumerator_Generator withPat(hydra.ext.scala.meta.Pat pat) {
    return new Enumerator_Generator(pat, rhs);
  }
  
  public Enumerator_Generator withRhs(hydra.ext.scala.meta.Data rhs) {
    return new Enumerator_Generator(pat, rhs);
  }
}