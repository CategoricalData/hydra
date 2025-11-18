package hydra.dsl;

import hydra.compute.Flow;
import hydra.util.Maybe;
import hydra.util.Unit;
import hydra.tools.Function3;
import hydra.tools.Function4;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;


/**
 * A convenient wrapper for a stateless Flow, providing fluent-style methods.
 * @param <A> the result type of the Possible, e.g. <code>Possible&lt;String&gt;</code> provides a <code>String</code>,
 *           unless there is an evaluation error.
 */
public class Possible<A> {
  /**
   * The wrapped Flow of the Possible.
   */
  public final Flow<Unit, A> flow;

  /**
   * Construct a Possible which wraps the given Flow.
   * @param flow the flow to wrap
   */
  public Possible(Flow<Unit, A> flow) {
    this.flow = flow;
  }

  /**
   * Construct a pure Possible for a given object.
   * @param obj the object to wrap in a pure Possible
   */
  public Possible(A obj) {
    flow = Flows.pure(obj);
  }

  /**
   * Evaluate a Possible and consume the result.
   * @param consumer the consumer to apply to the result
   * @return a Possible containing Unit
   */
  public Possible<Unit> consume(Consumer<A> consumer) {
    return consume(this, consumer);
  }

  /**
   * Evaluate a Possible and consume the result.
   * @param <A> the type of the Possible's value
   * @param possible the Possible to evaluate
   * @param consumer the consumer to apply to the result
   * @return a Possible containing Unit
   */
  public static <A> Possible<Unit> consume(Possible<A> possible, Consumer<A> consumer) {
    return new Possible<>(Flows.consume(possible.flow, consumer));
  }

  /**
   * Extract the value from a Possible, throwing an exception if the Possible failed.
   * @return the extracted value
   */
  public A eval() {
    return eval(this);
  }

  /**
   * Extract the value from a Possible, returning a default value instead if the Possible failed.
   * @param dflt the default value to return if the Possible failed
   * @return the extracted value or the default value
   */
  public A eval(A dflt) {
    return eval(this, dflt);
  }

  /**
   * Extract the value from a Possible, throwing an exception if the Possible failed.
   * @param <A> the type of the Possible's value
   * @param possible the Possible to evaluate
   * @return the extracted value
   */
  public static <A> A eval(Possible<A> possible) {
    return Flows.fromFlow(new Unit(), possible.flow);
  }

  /**
   * Extract the value from a Possible, returning a default value instead if the Possible failed.
   * @param <A> the type of the Possible's value
   * @param possible the Possible to evaluate
   * @param dflt the default value to return if the Possible failed
   * @return the extracted value or the default value
   */
  public static <A> A eval(Possible<A> possible, A dflt) {
    return Flows.fromFlow(dflt, new Unit(), possible.flow);
  }

  /**
   * Monadic bind function for Possibles.
   * @param <B> the result type
   * @param f the function to apply to the Possible's value
   * @return a Possible containing the result of the bind operation
   */
  public <B> Possible<B> flatMap(Function<A, Possible<B>> f) {
    return flatMap(this, f);
  }

  /**
   * Monadic bind function for Possibles.
   * @param <A> the input type
   * @param <B> the result type
   * @param a the input Possible
   * @param f the function to apply to the Possible's value
   * @return a Possible containing the result of the bind operation
   */
  public static <A, B> Possible<B> flatMap(Possible<A> a, Function<A, Possible<B>> f) {
    return new Possible<>(Flows.bind(a.flow, x -> f.apply(x).flow));
  }

  /**
   * Variant of monadic bind which takes two monadic arguments and a binary function.
   * @param <B> the second input type
   * @param <C> the result type
   * @param other the second Possible
   * @param f the binary function to apply
   * @return a Possible containing the result of the bind operation
   */
  public <B, C> Possible<C> flatMap2(Possible<B> other, BiFunction<A, B, Possible<C>> f) {
    return flatMap2(this, other, f);
  }

  /**
   * Variant of monadic bind which takes two monadic arguments and a binary function.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the result type
   * @param possibleA the first Possible
   * @param possibleB the second Possible
   * @param f the binary function to apply
   * @return a Possible containing the result of the bind operation
   */
  public static <A, B, C> Possible<C> flatMap2(
      Possible<A> possibleA, Possible<B> possibleB, BiFunction<A, B, Possible<C>> f) {
    return new Possible<>(Flows.bind2(possibleA.flow, possibleB.flow, (a, b) -> f.apply(a, b).flow));
  }

  /**
   * Variant of monadic bind which takes three monadic arguments and an arity-3 function.
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param other the second Possible
   * @param other2 the third Possible
   * @param f the arity-3 function to apply
   * @return a Possible containing the result of the bind operation
   */
  public <B, C, D> Possible<D> flatMap3(
      Possible<B> other,
      Possible<C> other2,
      Function3<A, B, C, Possible<D>> f) {
    return flatMap3(this, other, other2, f);
  }

  /**
   * Variant of monadic bind which takes three monadic arguments and an arity-3 function.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param possibleA the first Possible
   * @param possibleB the second Possible
   * @param possibleC the third Possible
   * @param f the arity-3 function to apply
   * @return a Possible containing the result of the bind operation
   */
  public static <A, B, C, D> Possible<D> flatMap3(
      Possible<A> possibleA,
      Possible<B> possibleB,
      Possible<C> possibleC,
      Function3<A, B, C, Possible<D>> f) {
    return new Possible<>(Flows.bind3(possibleA.flow, possibleB.flow, possibleC.flow,
            (a, b, c) -> f.apply(a, b, c).flow));
  }

  /**
   * Map a function over a Possible, producing another Possible.
   * @param <B> the result type
   * @param f the function to apply
   * @return a Possible containing the result of applying the function
   */
  public <B> Possible<B> map(Function<A, B> f) {
    return map(this, f);
  }

  /**
   * Map a function over a Possible, producing another Possible.
   * @param <A> the input type
   * @param <B> the result type
   * @param possible the input Possible
   * @param f the function to apply
   * @return a Possible containing the result of applying the function
   */
  public static <A, B> Possible<B> map(Possible<A> possible, Function<A, B> f) {
    return new Possible<>(Flows.map(possible.flow, f));
  }

  /**
   * Map a bifunction over two Possibles, producing a Possible.
   * @param <B> the second input type
   * @param <C> the result type
   * @param other the second Possible
   * @param f the bifunction to apply
   * @return a Possible containing the result of applying the bifunction
   */
  public <B, C> Possible<C> map2(Possible<B> other, BiFunction<A, B, C> f) {
    return map2(this, other, f);
  }

  /**
   * Map a bifunction over two Possibles, producing a Possible.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the result type
   * @param possibleA the first Possible
   * @param possibleB the second Possible
   * @param f the bifunction to apply
   * @return a Possible containing the result of applying the bifunction
   */
  public static <A, B, C> Possible<C> map2(Possible<A> possibleA, Possible<B> possibleB, BiFunction<A, B, C> f) {
    return new Possible<>(Flows.map2(possibleA.flow, possibleB.flow, f));
  }

  /**
   * Map an arity-3 function over three Possibles, producing a Possible.
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param other the second Possible
   * @param other2 the third Possible
   * @param f the arity-3 function to apply
   * @return a Possible containing the result of applying the function
   */
  public <B, C, D> Possible<D> map3(Possible<B> other, Possible<C> other2, Function3<A, B, C, D> f) {
    return map3(this, other, other2, f);
  }

  /**
   * Map an arity-3 function over three Possibles, producing a Possible.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param possibleA the first Possible
   * @param possibleB the second Possible
   * @param possibleC the third Possible
   * @param f the arity-3 function to apply
   * @return a Possible containing the result of applying the function
   */
  public static <A, B, C, D> Possible<D> map3(
          Possible<A> possibleA, Possible<B> possibleB, Possible<C> possibleC, Function3<A, B, C, D> f) {
    return new Possible<>(Flows.map3(possibleA.flow, possibleB.flow, possibleC.flow, f));
  }

  /**
   * Map an arity-4 function over four Possibles, producing a Possible.
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the fourth input type
   * @param <E> the result type
   * @param other the second Possible
   * @param other2 the third Possible
   * @param other3 the fourth Possible
   * @param f the arity-4 function to apply
   * @return a Possible containing the result of applying the function
   */
  public <B, C, D, E> Possible<E> map4(
      Possible<B> other,
      Possible<C> other2,
      Possible<D> other3,
      Function4<A, B, C, D, E> f) {
    return map4(this, other, other2, other3, f);
  }

  /**
   * Map an arity-4 function over four Possibles, producing a Possible.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the fourth input type
   * @param <E> the result type
   * @param possibleA the first Possible
   * @param possibleB the second Possible
   * @param possibleC the third Possible
   * @param possibleD the fourth Possible
   * @param f the arity-4 function to apply
   * @return a Possible containing the result of applying the function
   */
  public static <A, B, C, D, E> Possible<E> map4(
      Possible<A> possibleA,
      Possible<B> possibleB,
      Possible<C> possibleC,
      Possible<D> possibleD,
      Function4<A, B, C, D, E> f) {
    return new Possible<>(Flows.map4(possibleA.flow, possibleB.flow, possibleC.flow, possibleD.flow, f));
  }

  /**
   * Continue a Possible after adding a warning message.
   * @param message the warning message to add
   * @return a Possible with the warning message added
   */
  public Possible<A> warn(String message) {
    return warn(message, this);
  }

  /**
   * Continue a Possible after adding a warning message.
   * @param <A> the type of the Possible's value
   * @param message the warning message to add
   * @param possible the Possible to add the warning to
   * @return a Possible with the warning message added
   */
  public static <A> Possible<A> warn(String message, Possible<A> possible) {
    return new Possible<>(Flows.warn(message, possible.flow));
  }

  /**
   * Check whether a given value satisfies a list of predicates, returning the value itself if all checks are
   * successful, or a failure Possible for the first predicate that fails.
   * @param <A> the type of the input value
   * @param input the value to check
   * @param predicates the predicates to check against the input value
   * @return a Possible containing the input value if all predicates pass, or a failure Possible otherwise
   */
  public static <A> Possible<A> check(A input, Function<A, Maybe<String>>... predicates) {
    return new Possible<>(Flows.check(input, predicates));
  }

  /**
   * Compose two monadic functions, feeding the output of the first into the second.
   * @param <A> the input type
   * @param <B> the intermediate type
   * @param <C> the result type
   * @param f the first monadic function
   * @param g the second monadic function
   * @return the composed monadic function
   */
  public static <A, B, C> Function<A, Possible<C>> compose(Function<A, Possible<B>> f, Function<B, Possible<C>> g) {
    Function<A, Flow<Unit, C>> fg = Flows.compose(a -> f.apply(a).flow, b -> g.apply(b).flow);
    return a -> new Possible<>(fg.apply(a));
  }

  /**
   * Produce a failure Possible with the provided message.
   * @param <A> the type of the Possible's value
   * @param msg the error message
   * @return a failure Possible with the provided message
   */
  public static <A> Possible<A> fail(String msg) {
    return new Possible<>(Flows.fail(msg));
  }

  /**
   * Produce a failure Possible with the provided message and additional information from a Throwable.
   * @param <A> the type of the Possible's value
   * @param msg the error message
   * @param cause the throwable that caused the failure
   * @return a failure Possible with the provided message and cause
   */
  public static <A> Possible<A> fail(String msg, Throwable cause) {
    return new Possible<>(Flows.fail(msg, cause));
  }

  /**
   * Map a monadic function over a list, producing a Possible list.
   * @param <A> the input type
   * @param <B> the result type
   * @param as the input list
   * @param f the monadic function to apply
   * @return a Possible containing a list of results
   */
  public static <A, B> Possible<List<B>> mapM(List<A> as, Function<A, Possible<B>> f) {
    return new Possible<>(Flows.mapM(as, a -> f.apply(a).flow));
  }

  /**
   * Map a monadic function over an array, producing a Possible list.
   * @param <A> the input type
   * @param <B> the result type
   * @param xs the input array
   * @param f the monadic function to apply
   * @return a Possible containing a list of results
   */
  public static <A, B> Possible<List<B>> mapM(A[] xs, Function<A, Possible<B>> f) {
    return new Possible<>(Flows.mapM(xs, a -> f.apply(a).flow));
  }

  /**
   * Map a monadic function over the keys of a map, and another monadic function over the values of a map,
   * producing a Possible map.
   * @param <K1> the input key type
   * @param <V1> the input value type
   * @param <K2> the result key type
   * @param <V2> the result value type
   * @param xs the input map
   * @param kf the monadic function to apply to keys
   * @param vf the monadic function to apply to values
   * @return a Possible containing a map of results
   */
  public static <K1, V1, K2, V2> Possible<Map<K2, V2>> mapM(Map<K1, V1> xs,
      Function<K1, Possible<K2>> kf,
      Function<V1, Possible<V2>> vf) {
    return new Possible<>(Flows.mapM(xs, x -> kf.apply(x).flow, x -> vf.apply(x).flow));
  }

  /**
   * Map a monadic function over an optional value, producing a Possible optional.
   * @param <A> the input type
   * @param <B> the result type
   * @param xs the input optional
   * @param f the monadic function to apply
   * @return a Possible containing an optional result
   */
  public static <A, B> Possible<Maybe<B>> mapM(Maybe<A> xs, Function<A, Possible<B>> f) {
    return new Possible<>(Flows.mapM(xs, x -> f.apply(x).flow));
  }

  /**
   * Map a monadic function over a set, producing a Possible set.
   * @param <A> the input type
   * @param <B> the result type
   * @param as the input set
   * @param f the monadic function to apply
   * @return a Possible containing a set of results
   */
  public static <A, B> Possible<Set<B>> mapM(Set<A> as, Function<A, Possible<B>> f) {
    return new Possible<>(Flows.mapM(as, x -> f.apply(x).flow));
  }

  /**
   * Produce a given object as a pure Possible; the value is guaranteed to be present,
   * and neither state nor trace are modified.
   * @param <A> the type of the value
   * @param obj the value to wrap in a pure Possible
   * @return a pure Possible containing the value
   */
  public static <A> Possible<A> pure(A obj) {
    return new Possible<>(Flows.pure(obj));
  }

  /**
   * Evaluate each Possible from left to right, and produce a Possible of the resulting list.
   * Analogous to the sequence function in Haskell.
   * @param <A> the type of the elements
   * @param elements the list of Possibles to sequence
   * @return a Possible containing a list of the results
   */
  public static <A> Possible<List<A>> sequence(List<Possible<A>> elements) {
    List<Flow<Unit, A>> flows = new ArrayList<>(elements.size());
    for (Possible<A> p : elements) {
      flows.add(p.flow);
    }
    return new Possible<>(Flows.sequence(flows));
  }

  /**
   * Produce an error Possible indicating an unexpected value.
   * For example, if you expect a string but find an integer, use unexpected("string", myInt).
   * @param <A> the expected type
   * @param cat the category of the expected value
   * @param obj the unexpected object
   * @return an error Possible indicating the unexpected value
   */
  public static <A> Possible<A> unexpected(String cat, Object obj) {
    return new Possible<>(Flows.unexpected(cat, obj));
  }

  /**
   * Produce an error Possible indicating an unexpected class of value.
   * @param <A> the expected type
   * @param cat the category of the expected value
   * @param obj the object with an unexpected class
   * @return an error Possible indicating the unexpected class
   */
  public static <A> Possible<A> unexpectedClass(String cat, Object obj) {
    return new Possible<>(Flows.unexpectedClass(cat, obj));
  }
}
