package hydra.ext.scala.meta;

public class Pat_Interpolate {
  public final Data_Name prefix;
  
  public final java.util.List<Lit> parts;
  
  public Pat_Interpolate (Data_Name prefix, java.util.List<Lit> parts) {
    this.prefix = prefix;
    this.parts = parts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Interpolate)) {
      return false;
    }
    Pat_Interpolate o = (Pat_Interpolate) (other);
    return prefix.equals(o.prefix) && parts.equals(o.parts);
  }
  
  @Override
  public int hashCode() {
    return 2 * prefix.hashCode() + 3 * parts.hashCode();
  }
  
  public Pat_Interpolate withPrefix(Data_Name prefix) {
    return new Pat_Interpolate(prefix, parts);
  }
  
  public Pat_Interpolate withParts(java.util.List<Lit> parts) {
    return new Pat_Interpolate(prefix, parts);
  }
}