// Note: this is an automatically generated file. Do not edit.

package hydra.strip;

/**
 * Several functions for stripping annotations from types and terms.
 */
public interface Strip {
  static hydra.core.Term fullyStripTerm(hydra.core.Term t) {
    return ((t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (t);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated instance) {
        return hydra.strip.Strip.fullyStripTerm(((instance.value)).subject);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Typed instance) {
        return hydra.strip.Strip.fullyStripTerm(((instance.value)).term);
      }
    });
  }
  
  static hydra.core.Term stripTerm(hydra.core.Term t) {
    return ((t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (t);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated instance) {
        return hydra.strip.Strip.stripTerm(((instance.value)).subject);
      }
    });
  }
  
  static hydra.core.Type stripType(hydra.core.Type t) {
    return ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (t);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated instance) {
        return hydra.strip.Strip.stripType(((instance.value)).subject);
      }
    });
  }
  
  static hydra.core.Type stripTypeParameters(hydra.core.Type t) {
    return (hydra.strip.Strip.stripType((t))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (t);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Lambda instance) {
        return hydra.strip.Strip.stripTypeParameters(((instance.value)).body);
      }
    });
  }
}