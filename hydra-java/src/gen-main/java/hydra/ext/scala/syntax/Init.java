// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Init implements Serializable, Comparable<Init> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Init");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name ARGSS = new hydra.core.Name("argss");

  public final hydra.ext.scala.syntax.Type tpe;

  public final hydra.ext.scala.syntax.Name name;

  public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.syntax.Data>> argss;

  public Init (hydra.ext.scala.syntax.Type tpe, hydra.ext.scala.syntax.Name name, hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.syntax.Data>> argss) {
    this.tpe = tpe;
    this.name = name;
    this.argss = argss;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Init)) {
      return false;
    }
    Init o = (Init) other;
    return java.util.Objects.equals(
      this.tpe,
      o.tpe) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.argss,
      o.argss);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tpe) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(argss);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Init other) {
    int cmp = 0;
    cmp = ((Comparable) tpe).compareTo(other.tpe);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) argss).compareTo(other.argss);
  }

  public Init withTpe(hydra.ext.scala.syntax.Type tpe) {
    return new Init(tpe, name, argss);
  }

  public Init withName(hydra.ext.scala.syntax.Name name) {
    return new Init(tpe, name, argss);
  }

  public Init withArgss(hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.syntax.Data>> argss) {
    return new Init(tpe, name, argss);
  }
}
