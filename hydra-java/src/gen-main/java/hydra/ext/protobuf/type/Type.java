package hydra.ext.protobuf.type;

/**
 * A protocol buffer message type.
 */
public class Type {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/protobuf/type.Type");
  
  /**
   * The fully qualified message name.
   */
  public final String name;
  
  /**
   * The list of fields.
   */
  public final java.util.List<hydra.ext.protobuf.type.Field> fields;
  
  /**
   * The list of types appearing in `oneof` definitions in this type.
   */
  public final java.util.List<String> oneofs;
  
  /**
   * The protocol buffer options.
   */
  public final java.util.List<hydra.ext.protobuf.type.Option> options;
  
  /**
   * The source context.
   */
  public final hydra.ext.protobuf.sourceContext.SourceContext sourceContext;
  
  /**
   * The source syntax.
   */
  public final hydra.ext.protobuf.type.Syntax syntax;
  
  public Type (String name, java.util.List<hydra.ext.protobuf.type.Field> fields, java.util.List<String> oneofs, java.util.List<hydra.ext.protobuf.type.Option> options, hydra.ext.protobuf.sourceContext.SourceContext sourceContext, hydra.ext.protobuf.type.Syntax syntax) {
    this.name = name;
    this.fields = fields;
    this.oneofs = oneofs;
    this.options = options;
    this.sourceContext = sourceContext;
    this.syntax = syntax;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type)) {
      return false;
    }
    Type o = (Type) (other);
    return name.equals(o.name) && fields.equals(o.fields) && oneofs.equals(o.oneofs) && options.equals(o.options) && sourceContext.equals(o.sourceContext) && syntax.equals(o.syntax);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode() + 5 * oneofs.hashCode() + 7 * options.hashCode() + 11 * sourceContext.hashCode() + 13 * syntax.hashCode();
  }
  
  public Type withName(String name) {
    return new Type(name, fields, oneofs, options, sourceContext, syntax);
  }
  
  public Type withFields(java.util.List<hydra.ext.protobuf.type.Field> fields) {
    return new Type(name, fields, oneofs, options, sourceContext, syntax);
  }
  
  public Type withOneofs(java.util.List<String> oneofs) {
    return new Type(name, fields, oneofs, options, sourceContext, syntax);
  }
  
  public Type withOptions(java.util.List<hydra.ext.protobuf.type.Option> options) {
    return new Type(name, fields, oneofs, options, sourceContext, syntax);
  }
  
  public Type withSourceContext(hydra.ext.protobuf.sourceContext.SourceContext sourceContext) {
    return new Type(name, fields, oneofs, options, sourceContext, syntax);
  }
  
  public Type withSyntax(hydra.ext.protobuf.type.Syntax syntax) {
    return new Type(name, fields, oneofs, options, sourceContext, syntax);
  }
}
