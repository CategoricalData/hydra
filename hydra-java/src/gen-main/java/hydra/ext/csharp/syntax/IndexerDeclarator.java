// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class IndexerDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.IndexerDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE = new hydra.core.Name("interface");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.InterfaceType> interface_;
  
  public final hydra.ext.csharp.syntax.FormalParameterList parameters;
  
  public IndexerDeclarator (hydra.ext.csharp.syntax.Type type, hydra.util.Opt<hydra.ext.csharp.syntax.InterfaceType> interface_, hydra.ext.csharp.syntax.FormalParameterList parameters) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((interface_));
    java.util.Objects.requireNonNull((parameters));
    this.type = type;
    this.interface_ = interface_;
    this.parameters = parameters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IndexerDeclarator)) {
      return false;
    }
    IndexerDeclarator o = (IndexerDeclarator) (other);
    return type.equals(o.type) && interface_.equals(o.interface_) && parameters.equals(o.parameters);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * interface_.hashCode() + 5 * parameters.hashCode();
  }
  
  public IndexerDeclarator withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new IndexerDeclarator(type, interface_, parameters);
  }
  
  public IndexerDeclarator withInterface(hydra.util.Opt<hydra.ext.csharp.syntax.InterfaceType> interface_) {
    java.util.Objects.requireNonNull((interface_));
    return new IndexerDeclarator(type, interface_, parameters);
  }
  
  public IndexerDeclarator withParameters(hydra.ext.csharp.syntax.FormalParameterList parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new IndexerDeclarator(type, interface_, parameters);
  }
}