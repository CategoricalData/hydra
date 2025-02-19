// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FinalizerDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FinalizerDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_EXTERN = new hydra.core.Name("extern");
  
  public static final hydra.core.Name FIELD_NAME_UNSAFE = new hydra.core.Name("unsafe");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final Boolean extern;
  
  public final Boolean unsafe;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.ext.csharp.syntax.FinalizerBody body;
  
  public FinalizerDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, Boolean extern, Boolean unsafe, hydra.ext.csharp.syntax.Identifier name, hydra.ext.csharp.syntax.FinalizerBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((extern));
    java.util.Objects.requireNonNull((unsafe));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.extern = extern;
    this.unsafe = unsafe;
    this.name = name;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FinalizerDeclaration)) {
      return false;
    }
    FinalizerDeclaration o = (FinalizerDeclaration) (other);
    return attributes.equals(o.attributes) && extern.equals(o.extern) && unsafe.equals(o.unsafe) && name.equals(o.name) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * extern.hashCode() + 5 * unsafe.hashCode() + 7 * name.hashCode() + 11 * body.hashCode();
  }
  
  public FinalizerDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new FinalizerDeclaration(attributes, extern, unsafe, name, body);
  }
  
  public FinalizerDeclaration withExtern(Boolean extern) {
    java.util.Objects.requireNonNull((extern));
    return new FinalizerDeclaration(attributes, extern, unsafe, name, body);
  }
  
  public FinalizerDeclaration withUnsafe(Boolean unsafe) {
    java.util.Objects.requireNonNull((unsafe));
    return new FinalizerDeclaration(attributes, extern, unsafe, name, body);
  }
  
  public FinalizerDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new FinalizerDeclaration(attributes, extern, unsafe, name, body);
  }
  
  public FinalizerDeclaration withBody(hydra.ext.csharp.syntax.FinalizerBody body) {
    java.util.Objects.requireNonNull((body));
    return new FinalizerDeclaration(attributes, extern, unsafe, name, body);
  }
}