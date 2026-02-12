// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Args implements Serializable, Comparable<Args> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Args");
  
  public static final hydra.core.Name FIELD_NAME_POSITIONAL = new hydra.core.Name("positional");
  
  public static final hydra.core.Name FIELD_NAME_KWARG_OR_STARRED = new hydra.core.Name("kwargOrStarred");
  
  public static final hydra.core.Name FIELD_NAME_KWARG_OR_DOUBLE_STARRED = new hydra.core.Name("kwargOrDoubleStarred");
  
  public final java.util.List<hydra.ext.python.syntax.PosArg> positional;
  
  public final java.util.List<hydra.ext.python.syntax.KwargOrStarred> kwargOrStarred;
  
  public final java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred> kwargOrDoubleStarred;
  
  public Args (java.util.List<hydra.ext.python.syntax.PosArg> positional, java.util.List<hydra.ext.python.syntax.KwargOrStarred> kwargOrStarred, java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred> kwargOrDoubleStarred) {
    this.positional = positional;
    this.kwargOrStarred = kwargOrStarred;
    this.kwargOrDoubleStarred = kwargOrDoubleStarred;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Args)) {
      return false;
    }
    Args o = (Args) other;
    return java.util.Objects.equals(
      this.positional,
      o.positional) && java.util.Objects.equals(
      this.kwargOrStarred,
      o.kwargOrStarred) && java.util.Objects.equals(
      this.kwargOrDoubleStarred,
      o.kwargOrDoubleStarred);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(positional) + 3 * java.util.Objects.hashCode(kwargOrStarred) + 5 * java.util.Objects.hashCode(kwargOrDoubleStarred);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Args other) {
    int cmp = 0;
    cmp = Integer.compare(
      positional.hashCode(),
      other.positional.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      kwargOrStarred.hashCode(),
      other.kwargOrStarred.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      kwargOrDoubleStarred.hashCode(),
      other.kwargOrDoubleStarred.hashCode());
  }
  
  public Args withPositional(java.util.List<hydra.ext.python.syntax.PosArg> positional) {
    return new Args(positional, kwargOrStarred, kwargOrDoubleStarred);
  }
  
  public Args withKwargOrStarred(java.util.List<hydra.ext.python.syntax.KwargOrStarred> kwargOrStarred) {
    return new Args(positional, kwargOrStarred, kwargOrDoubleStarred);
  }
  
  public Args withKwargOrDoubleStarred(java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred> kwargOrDoubleStarred) {
    return new Args(positional, kwargOrStarred, kwargOrDoubleStarred);
  }
}
