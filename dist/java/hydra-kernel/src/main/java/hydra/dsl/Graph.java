// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.graph
 */
public interface Graph {
  static hydra.phantoms.TTerm<hydra.graph.Graph> graph(hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> boundTerms, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> boundTypes, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> classConstraints, hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> lambdaVariables, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> metadata, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.graph.Primitive>> primitives, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes, hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> typeVariables) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), (boundTerms).value),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), (boundTypes).value),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), (classConstraints).value),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), (lambdaVariables).value),
      new hydra.core.Field(new hydra.core.Name("metadata"), (metadata).value),
      new hydra.core.Field(new hydra.core.Name("primitives"), (primitives).value),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), (schemaTypes).value),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), (typeVariables).value)))));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> graphBoundTerms(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> graphBoundTypes(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> graphClassConstraints(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> graphLambdaVariables(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> graphMetadata(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.graph.Primitive>> graphPrimitives(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> graphSchemaTypes(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> graphTypeVariables(hydra.phantoms.TTerm<hydra.graph.Graph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithBoundTerms(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithBoundTypes(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithClassConstraints(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithLambdaVariables(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithMetadata(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("metadata"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithPrimitives(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.graph.Primitive>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithSchemaTypes(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("typeVariables"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> graphWithTypeVariables(hydra.phantoms.TTerm<hydra.graph.Graph> original, hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("boundTerms"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTerms"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("boundTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("boundTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("classConstraints"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("lambdaVariables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("metadata"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("primitives"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Graph"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Primitive> primitive(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.TypeScheme> type, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>>> implementation) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Primitive"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("implementation"), (implementation).value)))));
  }

  static hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>>> primitiveImplementation(hydra.phantoms.TTerm<hydra.graph.Primitive> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("implementation"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> primitiveName(hydra.phantoms.TTerm<hydra.graph.Primitive> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeScheme> primitiveType(hydra.phantoms.TTerm<hydra.graph.Primitive> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.graph.Primitive> primitiveWithImplementation(hydra.phantoms.TTerm<hydra.graph.Primitive> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Primitive"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("implementation"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Primitive> primitiveWithName(hydra.phantoms.TTerm<hydra.graph.Primitive> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Primitive"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("implementation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("implementation"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.graph.Primitive> primitiveWithType(hydra.phantoms.TTerm<hydra.graph.Primitive> original, hydra.phantoms.TTerm<hydra.core.TypeScheme> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.Primitive"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("implementation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.Primitive"), new hydra.core.Name("implementation"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> termCoder(hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, A>>>>> encode, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<A, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>> decode) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.TermCoder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("encode"), (encode).value),
      new hydra.core.Field(new hydra.core.Name("decode"), (decode).value)))));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<A, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>> termCoderDecode(hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("decode"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, A>>>>> termCoderEncode(hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("encode"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.core.Type> termCoderType(hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("type"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> termCoderWithDecode(hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<A, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.TermCoder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("encode"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("decode"), (newVal).value)))));
  }

  static <A> hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> termCoderWithEncode(hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, A>>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.TermCoder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encode"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("decode"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> termCoderWithType(hydra.phantoms.TTerm<hydra.graph.TermCoder<A>> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.graph.TermCoder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("encode"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.graph.TermCoder"), new hydra.core.Name("decode"))))), (original).value)))))));
  }
}
