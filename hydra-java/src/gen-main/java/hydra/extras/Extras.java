// Note: this is an automatically generated file. Do not edit.

package hydra.extras;

/**
 * Basic functions which depend on primitive functions
 */
public interface Extras {
  static Integer functionArity(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.Visitor<>() {
      @Override
      public Integer visit(hydra.core.Function.Elimination instance) {
        return 1;
      }
      
      @Override
      public Integer visit(hydra.core.Function.Lambda instance) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.extras.Extras.termArity(((instance.value)).body));
      }
      
      @Override
      public Integer visit(hydra.core.Function.Primitive instance) {
        return 42;
      }
    });
  }
  
  static java.util.function.Function<hydra.core.Name, hydra.util.Opt<hydra.graph.Primitive>> lookupPrimitive(hydra.graph.Graph g) {
    return (java.util.function.Function<hydra.core.Name, hydra.util.Opt<hydra.graph.Primitive>>) (name -> hydra.lib.maps.Lookup.apply(
      (name),
      ((g)).primitives));
  }
  
  static Integer primitiveArity(hydra.graph.Primitive x) {
    return hydra.extras.Extras.typeArity((((x)).type).type);
  }
  
  static java.util.function.Function<String, hydra.core.Name> qname(hydra.module.Namespace ns) {
    return (java.util.function.Function<String, hydra.core.Name>) (name -> new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      ((ns)).value,
      ".",
      (name)))));
  }
  
  static Integer termArity(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Term.Application instance) {
        return hydra.lib.math.Sub.apply(
          hydra.extras.Extras.termArity(((instance.value)).function),
          1);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Function instance) {
        return hydra.extras.Extras.functionArity((instance.value));
      }
    });
  }
  
  static Integer typeArity(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Type instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Type.Annotated instance) {
        return hydra.extras.Extras.typeArity(((instance.value)).subject);
      }
      
      @Override
      public Integer visit(hydra.core.Type.Application instance) {
        return hydra.extras.Extras.typeArity(((instance.value)).function);
      }
      
      @Override
      public Integer visit(hydra.core.Type.Lambda instance) {
        return hydra.extras.Extras.typeArity(((instance.value)).body);
      }
      
      @Override
      public Integer visit(hydra.core.Type.Function instance) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.extras.Extras.typeArity(((instance.value)).codomain));
      }
    });
  }
  
  static java.util.List<hydra.core.Type> uncurryType(hydra.core.Type t) {
    return ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return java.util.Arrays.asList((t));
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Annotated instance) {
        return hydra.extras.Extras.uncurryType(((instance.value)).subject);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Application instance) {
        return hydra.extras.Extras.uncurryType(((instance.value)).function);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Lambda instance) {
        return hydra.extras.Extras.uncurryType(((instance.value)).body);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Function instance) {
        return hydra.lib.lists.Cons.apply(
          ((instance.value)).domain,
          hydra.extras.Extras.uncurryType(((instance.value)).codomain));
      }
    });
  }
  
  static java.util.function.Function<java.util.Map<String, hydra.core.Term>, hydra.util.Opt<hydra.core.Term>> getAnnotation(String key) {
    return (java.util.function.Function<java.util.Map<String, hydra.core.Term>, hydra.util.Opt<hydra.core.Term>>) (ann -> hydra.lib.maps.Lookup.apply(
      (key),
      (ann)));
  }
}