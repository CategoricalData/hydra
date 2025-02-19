// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Init implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Init");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGSS = new hydra.core.Name("argss");
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public final hydra.ext.scala.meta.Name name;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Data>> argss;
  
  public Init (hydra.ext.scala.meta.Type tpe, hydra.ext.scala.meta.Name name, java.util.List<java.util.List<hydra.ext.scala.meta.Data>> argss) {
    java.util.Objects.requireNonNull((tpe));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((argss));
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
  
  public Init withTpe(hydra.ext.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    return new Init(tpe, name, argss);
  }
  
  public Init withName(hydra.ext.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Init(tpe, name, argss);
  }
  
  public Init withArgss(java.util.List<java.util.List<hydra.ext.scala.meta.Data>> argss) {
    java.util.Objects.requireNonNull((argss));
    return new Init(tpe, name, argss);
  }
}