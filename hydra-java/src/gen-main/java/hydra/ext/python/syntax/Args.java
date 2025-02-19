// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Args implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Args");
  
  public static final hydra.core.Name FIELD_NAME_POSITIONAL = new hydra.core.Name("positional");
  
  public static final hydra.core.Name FIELD_NAME_KWARG_OR_STARRED = new hydra.core.Name("kwargOrStarred");
  
  public static final hydra.core.Name FIELD_NAME_KWARG_OR_DOUBLE_STARRED = new hydra.core.Name("kwargOrDoubleStarred");
  
  public final java.util.List<hydra.ext.python.syntax.PosArg> positional;
  
  public final java.util.List<hydra.ext.python.syntax.KwargOrStarred> kwargOrStarred;
  
  public final java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred> kwargOrDoubleStarred;
  
  public Args (java.util.List<hydra.ext.python.syntax.PosArg> positional, java.util.List<hydra.ext.python.syntax.KwargOrStarred> kwargOrStarred, java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred> kwargOrDoubleStarred) {
    java.util.Objects.requireNonNull((positional));
    java.util.Objects.requireNonNull((kwargOrStarred));
    java.util.Objects.requireNonNull((kwargOrDoubleStarred));
    this.positional = positional;
    this.kwargOrStarred = kwargOrStarred;
    this.kwargOrDoubleStarred = kwargOrDoubleStarred;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Args)) {
      return false;
    }
    Args o = (Args) (other);
    return positional.equals(o.positional) && kwargOrStarred.equals(o.kwargOrStarred) && kwargOrDoubleStarred.equals(o.kwargOrDoubleStarred);
  }
  
  @Override
  public int hashCode() {
    return 2 * positional.hashCode() + 3 * kwargOrStarred.hashCode() + 5 * kwargOrDoubleStarred.hashCode();
  }
  
  public Args withPositional(java.util.List<hydra.ext.python.syntax.PosArg> positional) {
    java.util.Objects.requireNonNull((positional));
    return new Args(positional, kwargOrStarred, kwargOrDoubleStarred);
  }
  
  public Args withKwargOrStarred(java.util.List<hydra.ext.python.syntax.KwargOrStarred> kwargOrStarred) {
    java.util.Objects.requireNonNull((kwargOrStarred));
    return new Args(positional, kwargOrStarred, kwargOrDoubleStarred);
  }
  
  public Args withKwargOrDoubleStarred(java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred> kwargOrDoubleStarred) {
    java.util.Objects.requireNonNull((kwargOrDoubleStarred));
    return new Args(positional, kwargOrStarred, kwargOrDoubleStarred);
  }
}