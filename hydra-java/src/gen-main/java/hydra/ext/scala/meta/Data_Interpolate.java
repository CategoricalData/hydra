package hydra.ext.scala.meta;

public class Data_Interpolate {
  public final Data_Name prefix;
  
  public final java.util.List<Lit> parts;
  
  public final java.util.List<Data> args;
  
  public Data_Interpolate (Data_Name prefix, java.util.List<Lit> parts, java.util.List<Data> args) {
    this.prefix = prefix;
    this.parts = parts;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Interpolate)) {
      return false;
    }
    Data_Interpolate o = (Data_Interpolate) (other);
    return prefix.equals(o.prefix) && parts.equals(o.parts) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * prefix.hashCode() + 3 * parts.hashCode() + 5 * args.hashCode();
  }
  
  public Data_Interpolate withPrefix(Data_Name prefix) {
    return new Data_Interpolate(prefix, parts, args);
  }
  
  public Data_Interpolate withParts(java.util.List<Lit> parts) {
    return new Data_Interpolate(prefix, parts, args);
  }
  
  public Data_Interpolate withArgs(java.util.List<Data> args) {
    return new Data_Interpolate(prefix, parts, args);
  }
}