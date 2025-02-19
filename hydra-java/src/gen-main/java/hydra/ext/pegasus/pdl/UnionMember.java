// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class UnionMember implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.pegasus.pdl.UnionMember");
  
  public static final hydra.core.Name FIELD_NAME_ALIAS = new hydra.core.Name("alias");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public final hydra.util.Opt<hydra.ext.pegasus.pdl.FieldName> alias;
  
  public final hydra.ext.pegasus.pdl.Schema value;
  
  public final hydra.ext.pegasus.pdl.Annotations annotations;
  
  public UnionMember (hydra.util.Opt<hydra.ext.pegasus.pdl.FieldName> alias, hydra.ext.pegasus.pdl.Schema value, hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((alias));
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((annotations));
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
  
  public UnionMember withAlias(hydra.util.Opt<hydra.ext.pegasus.pdl.FieldName> alias) {
    java.util.Objects.requireNonNull((alias));
    return new UnionMember(alias, value, annotations);
  }
  
  public UnionMember withValue(hydra.ext.pegasus.pdl.Schema value) {
    java.util.Objects.requireNonNull((value));
    return new UnionMember(alias, value, annotations);
  }
  
  public UnionMember withAnnotations(hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new UnionMember(alias, value, annotations);
  }
}