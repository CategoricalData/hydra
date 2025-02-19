// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class MemberInitializer implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MemberInitializer");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.csharp.syntax.InitializerTarget target;
  
  public final hydra.ext.csharp.syntax.InitializerValue value;
  
  public MemberInitializer (hydra.ext.csharp.syntax.InitializerTarget target, hydra.ext.csharp.syntax.InitializerValue value) {
    java.util.Objects.requireNonNull((target));
    java.util.Objects.requireNonNull((value));
    this.target = target;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MemberInitializer)) {
      return false;
    }
    MemberInitializer o = (MemberInitializer) (other);
    return target.equals(o.target) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * target.hashCode() + 3 * value.hashCode();
  }
  
  public MemberInitializer withTarget(hydra.ext.csharp.syntax.InitializerTarget target) {
    java.util.Objects.requireNonNull((target));
    return new MemberInitializer(target, value);
  }
  
  public MemberInitializer withValue(hydra.ext.csharp.syntax.InitializerValue value) {
    java.util.Objects.requireNonNull((value));
    return new MemberInitializer(target, value);
  }
}