// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class UsingStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.UsingStatement");
  
  public static final hydra.core.Name FIELD_NAME_ACQUISITION = new hydra.core.Name("acquisition");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.csharp.syntax.ResourceAcquisition acquisition;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement body;
  
  public UsingStatement (hydra.ext.csharp.syntax.ResourceAcquisition acquisition, hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((acquisition));
    java.util.Objects.requireNonNull((body));
    this.acquisition = acquisition;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UsingStatement)) {
      return false;
    }
    UsingStatement o = (UsingStatement) (other);
    return acquisition.equals(o.acquisition) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * acquisition.hashCode() + 3 * body.hashCode();
  }
  
  public UsingStatement withAcquisition(hydra.ext.csharp.syntax.ResourceAcquisition acquisition) {
    java.util.Objects.requireNonNull((acquisition));
    return new UsingStatement(acquisition, body);
  }
  
  public UsingStatement withBody(hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((body));
    return new UsingStatement(acquisition, body);
  }
}