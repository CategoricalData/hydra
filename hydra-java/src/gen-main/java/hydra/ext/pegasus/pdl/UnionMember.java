package hydra.ext.pegasus.pdl;

public class UnionMember {
  public final java.util.Optional<FieldName> alias;
  
  public final Schema value;
  
  public final Annotations annotations;
  
  public UnionMember (java.util.Optional<FieldName> alias, Schema value, Annotations annotations) {
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
  
  public UnionMember withAlias(java.util.Optional<FieldName> alias) {
    return new UnionMember(alias, value, annotations);
  }
  
  public UnionMember withValue(Schema value) {
    return new UnionMember(alias, value, annotations);
  }
  
  public UnionMember withAnnotations(Annotations annotations) {
    return new UnionMember(alias, value, annotations);
  }
}