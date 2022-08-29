package hydra.ext.pegasus.pdl;

public class UnionMember {
  public final java.util.Optional<hydra.ext.pegasus.pdl.FieldName> alias;
  
  public final hydra.ext.pegasus.pdl.Schema value;
  
  public final hydra.ext.pegasus.pdl.Annotations annotations;
  
  public UnionMember (java.util.Optional<hydra.ext.pegasus.pdl.FieldName> alias, hydra.ext.pegasus.pdl.Schema value, hydra.ext.pegasus.pdl.Annotations annotations) {
    this.alias = alias;
    this.value = value;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionMember)) {
      return false;
    }
    UnionMember o = (UnionMember) (other);
    return alias.equals(o.alias) && value.equals(o.value) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * alias.hashCode() + 3 * value.hashCode() + 5 * annotations.hashCode();
  }
  
  public UnionMember withAlias(java.util.Optional<hydra.ext.pegasus.pdl.FieldName> alias) {
    return new UnionMember(alias, value, annotations);
  }
  
  public UnionMember withValue(hydra.ext.pegasus.pdl.Schema value) {
    return new UnionMember(alias, value, annotations);
  }
  
  public UnionMember withAnnotations(hydra.ext.pegasus.pdl.Annotations annotations) {
    return new UnionMember(alias, value, annotations);
  }
}