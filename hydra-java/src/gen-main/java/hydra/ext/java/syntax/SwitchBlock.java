package hydra.ext.java.syntax;

public class SwitchBlock {
  public final java.util.List<hydra.ext.java.syntax.SwitchBlock_Pair> value;
  
  public SwitchBlock (java.util.List<hydra.ext.java.syntax.SwitchBlock_Pair> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchBlock)) {
      return false;
    }
    SwitchBlock o = (SwitchBlock) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}