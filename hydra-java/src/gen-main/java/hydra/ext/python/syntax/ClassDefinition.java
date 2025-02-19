// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ClassDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ClassDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DECORATORS = new hydra.core.Name("decorators");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMS = new hydra.core.Name("typeParams");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Decorators> decorators;
  
  public final hydra.ext.python.syntax.Name name;
  
  public final java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Args> arguments;
  
  public final hydra.ext.python.syntax.Block body;
  
  public ClassDefinition (hydra.util.Opt<hydra.ext.python.syntax.Decorators> decorators, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams, hydra.util.Opt<hydra.ext.python.syntax.Args> arguments, hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((decorators));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((typeParams));
    java.util.Objects.requireNonNull((arguments));
    java.util.Objects.requireNonNull((body));
    this.decorators = decorators;
    this.name = name;
    this.typeParams = typeParams;
    this.arguments = arguments;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassDefinition)) {
      return false;
    }
    ClassDefinition o = (ClassDefinition) (other);
    return decorators.equals(o.decorators) && name.equals(o.name) && typeParams.equals(o.typeParams) && arguments.equals(o.arguments) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * decorators.hashCode() + 3 * name.hashCode() + 5 * typeParams.hashCode() + 7 * arguments.hashCode() + 11 * body.hashCode();
  }
  
  public ClassDefinition withDecorators(hydra.util.Opt<hydra.ext.python.syntax.Decorators> decorators) {
    java.util.Objects.requireNonNull((decorators));
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withTypeParams(java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams) {
    java.util.Objects.requireNonNull((typeParams));
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withArguments(hydra.util.Opt<hydra.ext.python.syntax.Args> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
}