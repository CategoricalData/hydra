package hydra.ext.scala.meta;

public class Init {
  public final Type tpe;
  
  public final Name name;
  
  public final java.util.List<java.util.List<Data>> argss;
  
  public Init (Type tpe, Name name, java.util.List<java.util.List<Data>> argss) {
    this.tpe = tpe;
    this.name = name;
    this.argss = argss;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Init)) {
      return false;
    }
    Init o = (Init) (other);
    return tpe.equals(o.tpe) && name.equals(o.name) && argss.equals(o.argss);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode() + 3 * name.hashCode() + 5 * argss.hashCode();
  }
  
  public Init withTpe(Type tpe) {
    return new Init(tpe, name, argss);
  }
  
  public Init withName(Name name) {
    return new Init(tpe, name, argss);
  }
  
  public Init withArgss(java.util.List<java.util.List<Data>> argss) {
    return new Init(tpe, name, argss);
  }
}