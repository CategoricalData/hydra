package hydra.ext.scala.meta;

public class Type_Lambda {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Lambda");
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public Type_Lambda (java.util.List<hydra.ext.scala.meta.Type_Param> tparams, hydra.ext.scala.meta.Type tpe) {
    this.tparams = tparams;
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Lambda)) {
      return false;
    }
    Type_Lambda o = (Type_Lambda) (other);
    return tparams.equals(o.tparams) && tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tparams.hashCode() + 3 * tpe.hashCode();
  }
  
  public Type_Lambda withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    return new Type_Lambda(tparams, tpe);
  }
  
  public Type_Lambda withTpe(hydra.ext.scala.meta.Type tpe) {
    return new Type_Lambda(tparams, tpe);
  }
}