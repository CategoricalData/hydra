package hydra.ext.scala.meta;

public class Case {
  public final Pat pat;
  
  public final java.util.Optional<Data> cond;
  
  public final Data body;
  
  public Case (Pat pat, java.util.Optional<Data> cond, Data body) {
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
  
  public Case withPat(Pat pat) {
    return new Case(pat, cond, body);
  }
  
  public Case withCond(java.util.Optional<Data> cond) {
    return new Case(pat, cond, body);
  }
  
  public Case withBody(Data body) {
    return new Case(pat, cond, body);
  }
}