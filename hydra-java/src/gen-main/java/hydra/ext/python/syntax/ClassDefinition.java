// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ClassDefinition implements Serializable, Comparable<ClassDefinition> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ClassDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DECORATORS = new hydra.core.Name("decorators");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMS = new hydra.core.Name("typeParams");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decorators;
  
  public final hydra.ext.python.syntax.Name name;
  
  public final java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Args> arguments;
  
  public final hydra.ext.python.syntax.Block body;
  
  public ClassDefinition (hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decorators, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams, hydra.util.Maybe<hydra.ext.python.syntax.Args> arguments, hydra.ext.python.syntax.Block body) {
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
    ClassDefinition o = (ClassDefinition) other;
    return java.util.Objects.equals(
      this.decorators,
      o.decorators) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.typeParams,
      o.typeParams) && java.util.Objects.equals(
      this.arguments,
      o.arguments) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(decorators) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(typeParams) + 7 * java.util.Objects.hashCode(arguments) + 11 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassDefinition other) {
    int cmp = 0;
    cmp = Integer.compare(
      decorators.hashCode(),
      other.decorators.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeParams.hashCode(),
      other.typeParams.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      arguments.hashCode(),
      other.arguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public ClassDefinition withDecorators(hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decorators) {
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withName(hydra.ext.python.syntax.Name name) {
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withTypeParams(java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams) {
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withArguments(hydra.util.Maybe<hydra.ext.python.syntax.Args> arguments) {
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
  
  public ClassDefinition withBody(hydra.ext.python.syntax.Block body) {
    return new ClassDefinition(decorators, name, typeParams, arguments, body);
  }
}
