// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.sourceContext;

import java.io.Serializable;

/**
 * `SourceContext` represents information about the source of a protobuf element, like the file in which it is defined.
 */
public class SourceContext implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.sourceContext.SourceContext");
  
  public static final hydra.core.Name FIELD_NAME_FILE_NAME = new hydra.core.Name("fileName");
  
  /**
   * The path-qualified name of the .proto file that contained the associated protobuf element.  For example: `"google/protobuf/source_context.proto"`.
   */
  public final String fileName;
  
  public SourceContext (String fileName) {
    java.util.Objects.requireNonNull((fileName));
    this.fileName = fileName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SourceContext)) {
      return false;
    }
    SourceContext o = (SourceContext) (other);
    return fileName.equals(o.fileName);
  }
  
  @Override
  public int hashCode() {
    return 2 * fileName.hashCode();
  }
}