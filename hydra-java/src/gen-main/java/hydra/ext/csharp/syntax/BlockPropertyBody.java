// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BlockPropertyBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BlockPropertyBody");
  
  public static final hydra.core.Name FIELD_NAME_ACCESSORS = new hydra.core.Name("accessors");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.AccessorDeclarations accessors;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.VariableInitializer> initializer;
  
  public BlockPropertyBody (hydra.ext.csharp.syntax.AccessorDeclarations accessors, hydra.util.Opt<hydra.ext.csharp.syntax.VariableInitializer> initializer) {
    java.util.Objects.requireNonNull((accessors));
    java.util.Objects.requireNonNull((initializer));
    this.accessors = accessors;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BlockPropertyBody)) {
      return false;
    }
    BlockPropertyBody o = (BlockPropertyBody) (other);
    return accessors.equals(o.accessors) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * accessors.hashCode() + 3 * initializer.hashCode();
  }
  
  public BlockPropertyBody withAccessors(hydra.ext.csharp.syntax.AccessorDeclarations accessors) {
    java.util.Objects.requireNonNull((accessors));
    return new BlockPropertyBody(accessors, initializer);
  }
  
  public BlockPropertyBody withInitializer(hydra.util.Opt<hydra.ext.csharp.syntax.VariableInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new BlockPropertyBody(accessors, initializer);
  }
}