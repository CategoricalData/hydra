package hydra.ext.pegasus.pdl;

public class UnionSchema {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.UnionSchema");
  
  public final java.util.List<hydra.ext.pegasus.pdl.UnionMember> value;
  
  public UnionSchema (java.util.List<hydra.ext.pegasus.pdl.UnionMember> value) {
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