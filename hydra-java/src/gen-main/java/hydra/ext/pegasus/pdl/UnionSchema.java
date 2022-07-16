package hydra.ext.pegasus.pdl;

public class UnionSchema {
  public final java.util.List<UnionMember> value;
  
  public UnionSchema (java.util.List<UnionMember> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionSchema)) {
      return false;
    }
    UnionSchema o = (UnionSchema) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}