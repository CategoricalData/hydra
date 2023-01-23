package hydra.ext.protobuf.type;

/**
 * Enum type definition.
 */
public class Enum_ {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/protobuf/type.Enum");
  
  /**
   * Enum type name.
   */
  public final String name;
  
  /**
   * Enum value definitions.
   */
  public final java.util.List<hydra.ext.protobuf.type.EnumValue> enumvalue;
  
  /**
   * Protocol buffer options.
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
  
  public Enum_ (String name, java.util.List<hydra.ext.protobuf.type.EnumValue> enumvalue, java.util.List<hydra.ext.protobuf.type.Option> options, hydra.ext.protobuf.sourceContext.SourceContext sourceContext, hydra.ext.protobuf.type.Syntax syntax) {
    this.name = name;
    this.enumvalue = enumvalue;
    this.options = options;
    this.sourceContext = sourceContext;
    this.syntax = syntax;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enum_)) {
      return false;
    }
    Enum_ o = (Enum_) (other);
    return name.equals(o.name) && enumvalue.equals(o.enumvalue) && options.equals(o.options) && sourceContext.equals(o.sourceContext) && syntax.equals(o.syntax);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * enumvalue.hashCode() + 5 * options.hashCode() + 7 * sourceContext.hashCode() + 11 * syntax.hashCode();
  }
  
  public Enum_ withName(String name) {
    return new Enum_(name, enumvalue, options, sourceContext, syntax);
  }
  
  public Enum_ withEnumvalue(java.util.List<hydra.ext.protobuf.type.EnumValue> enumvalue) {
    return new Enum_(name, enumvalue, options, sourceContext, syntax);
  }
  
  public Enum_ withOptions(java.util.List<hydra.ext.protobuf.type.Option> options) {
    return new Enum_(name, enumvalue, options, sourceContext, syntax);
  }
  
  public Enum_ withSourceContext(hydra.ext.protobuf.sourceContext.SourceContext sourceContext) {
    return new Enum_(name, enumvalue, options, sourceContext, syntax);
  }
  
  public Enum_ withSyntax(hydra.ext.protobuf.type.Syntax syntax) {
    return new Enum_(name, enumvalue, options, sourceContext, syntax);
  }
}