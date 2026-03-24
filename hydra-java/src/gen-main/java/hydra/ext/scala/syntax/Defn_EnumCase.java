// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Defn_EnumCase implements Serializable, Comparable<Defn_EnumCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Defn_EnumCase");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name CTOR = new hydra.core.Name("ctor");

  public static final hydra.core.Name INITS = new hydra.core.Name("inits");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.ext.scala.syntax.Data_Name name;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams;

  public final hydra.ext.scala.syntax.Ctor_Primary ctor;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Init> inits;

  public Defn_EnumCase (hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods, hydra.ext.scala.syntax.Data_Name name, hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams, hydra.ext.scala.syntax.Ctor_Primary ctor, hydra.util.ConsList<hydra.ext.scala.syntax.Init> inits) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.ctor = ctor;
    this.inits = inits;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_EnumCase)) {
      return false;
    }
    Defn_EnumCase o = (Defn_EnumCase) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.ctor,
      o.ctor) && java.util.Objects.equals(
      this.inits,
      o.inits);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(ctor) + 11 * java.util.Objects.hashCode(inits);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_EnumCase other) {
    int cmp = 0;
    cmp = ((Comparable) mods).compareTo(other.mods);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) tparams).compareTo(other.tparams);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) ctor).compareTo(other.ctor);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) inits).compareTo(other.inits);
  }

  public Defn_EnumCase withMods(hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods) {
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }

  public Defn_EnumCase withName(hydra.ext.scala.syntax.Data_Name name) {
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }

  public Defn_EnumCase withTparams(hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams) {
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }

  public Defn_EnumCase withCtor(hydra.ext.scala.syntax.Ctor_Primary ctor) {
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }

  public Defn_EnumCase withInits(hydra.util.ConsList<hydra.ext.scala.syntax.Init> inits) {
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }
}
