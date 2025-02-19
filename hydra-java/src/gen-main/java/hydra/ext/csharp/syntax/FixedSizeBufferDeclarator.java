// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FixedSizeBufferDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FixedSizeBufferDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SIZE = new hydra.core.Name("size");
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.ext.csharp.syntax.ConstantExpression size;
  
  public FixedSizeBufferDeclarator (hydra.ext.csharp.syntax.Identifier name, hydra.ext.csharp.syntax.ConstantExpression size) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((size));
    this.name = name;
    this.size = size;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixedSizeBufferDeclarator)) {
      return false;
    }
    FixedSizeBufferDeclarator o = (FixedSizeBufferDeclarator) (other);
    return name.equals(o.name) && size.equals(o.size);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * size.hashCode();
  }
  
  public FixedSizeBufferDeclarator withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new FixedSizeBufferDeclarator(name, size);
  }
  
  public FixedSizeBufferDeclarator withSize(hydra.ext.csharp.syntax.ConstantExpression size) {
    java.util.Objects.requireNonNull((size));
    return new FixedSizeBufferDeclarator(name, size);
  }
}