package hydra.ext.sql.ansi;

public class PathResolvedUserDefinedTypeName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.PathResolvedUserDefinedTypeName");
  
  public final String value;
  
  public PathResolvedUserDefinedTypeName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathResolvedUserDefinedTypeName)) {
      return false;
    }
    PathResolvedUserDefinedTypeName o = (PathResolvedUserDefinedTypeName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}