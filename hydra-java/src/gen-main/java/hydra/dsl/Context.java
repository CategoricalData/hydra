// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.context
 */
public interface Context {
  static hydra.phantoms.TTerm<hydra.context.Context> context(hydra.phantoms.TTerm<hydra.util.ConsList<String>> trace, hydra.phantoms.TTerm<hydra.util.ConsList<String>> messages, hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> other) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.Context"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("trace"), (trace).value),
      new hydra.core.Field(new hydra.core.Name("messages"), (messages).value),
      new hydra.core.Field(new hydra.core.Name("other"), (other).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<String>> contextTrace(hydra.phantoms.TTerm<hydra.context.Context> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("trace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<String>> contextMessages(hydra.phantoms.TTerm<hydra.context.Context> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("messages"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> contextOther(hydra.phantoms.TTerm<hydra.context.Context> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("other"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.context.Context> contextWithTrace(hydra.phantoms.TTerm<hydra.context.Context> original, hydra.phantoms.TTerm<hydra.util.ConsList<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.Context"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("trace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("messages"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("messages"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("other"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.context.Context> contextWithMessages(hydra.phantoms.TTerm<hydra.context.Context> original, hydra.phantoms.TTerm<hydra.util.ConsList<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.Context"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("trace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("trace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("messages"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("other"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.context.Context> contextWithOther(hydra.phantoms.TTerm<hydra.context.Context> original, hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.Context"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("trace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("trace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("messages"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.Context"), new hydra.core.Name("messages"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("other"), (newVal).value)))));
  }

  static <E> hydra.phantoms.TTerm<hydra.context.InContext<E>> inContext(hydra.phantoms.TTerm<E> object, hydra.phantoms.TTerm<hydra.context.Context> context) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.InContext"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("object"), (object).value),
      new hydra.core.Field(new hydra.core.Name("context"), (context).value)))));
  }

  static <E> hydra.phantoms.TTerm<E> inContextObject(hydra.phantoms.TTerm<hydra.context.InContext<E>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.InContext"), new hydra.core.Name("object"))))), (x).value)));
  }

  static <E> hydra.phantoms.TTerm<hydra.context.Context> inContextContext(hydra.phantoms.TTerm<hydra.context.InContext<E>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.InContext"), new hydra.core.Name("context"))))), (x).value)));
  }

  static <E> hydra.phantoms.TTerm<hydra.context.InContext<E>> inContextWithObject(hydra.phantoms.TTerm<hydra.context.InContext<E>> original, hydra.phantoms.TTerm<E> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.InContext"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("object"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.InContext"), new hydra.core.Name("context"))))), (original).value)))))));
  }

  static <E> hydra.phantoms.TTerm<hydra.context.InContext<E>> inContextWithContext(hydra.phantoms.TTerm<hydra.context.InContext<E>> original, hydra.phantoms.TTerm<hydra.context.Context> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.InContext"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("object"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.context.InContext"), new hydra.core.Name("object"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), (newVal).value)))));
  }
}
