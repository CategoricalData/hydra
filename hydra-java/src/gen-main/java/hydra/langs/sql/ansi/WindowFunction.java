package hydra.langs.sql.ansi;

public class WindowFunction {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.WindowFunction");
  
  public WindowFunction () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WindowFunction)) {
      return false;
    }
    WindowFunction o = (WindowFunction) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}