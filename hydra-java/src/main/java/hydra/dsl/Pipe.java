package hydra.dsl;

import hydra.compute.Flow;
import hydra.util.Unit;
import hydra.tools.Function3;
import hydra.tools.Function4;
import hydra.util.Opt;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;


/**
 * A convenient wrapper for a stateless Flow, providing fluent-style methods.
 * @param <A> the result type of the Flow
 */
public class Pipe<A> {
  /**
   * The wrapped Flow of the pipe.
   */
  public final Flow<Unit, A> flow;

  /**
   * Construct a pipe which wraps the given Flow.
   * @param flow the flow to wrap
   */
  public Pipe(Flow<Unit, A> flow) {
    this.flow = flow;
  }

  /**
   * Construct a pure pipe for a given object.
   * @param obj the object to wrap in a pure pipe
   */
  public Pipe(A obj) {
    flow = Flows.pure(obj);
  }

  /**
   * Evaluate a pipe and consume the result.
   * @param consumer the consumer to apply to the result
   * @return a pipe containing Unit
   */
  public Pipe<Unit> consume(Consumer<A> consumer) {
    return consume(this, consumer);
  }

  /**
   * Evaluate a pipe and consume the result.
   * @param <A> the type of the pipe's value
   * @param pipe the pipe to evaluate
   * @param consumer the consumer to apply to the result
   * @return a pipe containing Unit
   */
  public static <A> Pipe<Unit> consume(Pipe<A> pipe, Consumer<A> consumer) {
    return new Pipe<>(Flows.consume(pipe.flow, consumer));
  }

  /**
   * Extract the value from a pipe, throwing an exception if the pipe failed.
   * @return the extracted value
   */
  public A eval() {
    return eval(this);
  }

  /**
   * Extract the value from a pipe, returning a default value instead if the pipe failed.
   * @param dflt the default value to return if the pipe failed
   * @return the extracted value or the default value
   */
  public A eval(A dflt) {
    return eval(this, dflt);
  }

  /**
   * Extract the value from a pipe, throwing an exception if the pipe failed.
   * @param <A> the type of the pipe's value
   * @param pipe the pipe to evaluate
   * @return the extracted value
   */
  public static <A> A eval(Pipe<A> pipe) {
    return Flows.fromFlow(new Unit(), pipe.flow);
  }

  /**
   * Extract the value from a pipe, returning a default value instead if the pipe failed.
   * @param <A> the type of the pipe's value
   * @param pipe the pipe to evaluate
   * @param dflt the default value to return if the pipe failed
   * @return the extracted value or the default value
   */
  public static <A> A eval(Pipe<A> pipe, A dflt) {
    return Flows.fromFlow(dflt, new Unit(), pipe.flow);
  }

  /**
   * Monadic bind function for pipes.
   * @param <B> the result type
   * @param f the function to apply to the pipe's value
   * @return a pipe containing the result of the bind operation
   */
  public <B> Pipe<B> flatMap(Function<A, Pipe<B>> f) {
    return flatMap(this, f);
  }

  /**
   * Monadic bind function for pipes.
   * @param <A> the input type
   * @param <B> the result type
   * @param a the input pipe
   * @param f the function to apply to the pipe's value
   * @return a pipe containing the result of the bind operation
   */
  public static <A, B> Pipe<B> flatMap(Pipe<A> a, Function<A, Pipe<B>> f) {
    return new Pipe<>(Flows.bind(a.flow, x -> f.apply(x).flow));
  }

  /**
   * Variant of monadic bind which takes two monadic arguments and a binary function.
   * @param <B> the second input type
   * @param <C> the result type
   * @param other the second pipe
   * @param f the binary function to apply
   * @return a pipe containing the result of the bind operation
   */
  public <B, C> Pipe<C> flatMap2(Pipe<B> other, BiFunction<A, B, Pipe<C>> f) {
    return flatMap2(this, other, f);
  }

  /**
   * Variant of monadic bind which takes two monadic arguments and a binary function.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the result type
   * @param pipeA the first pipe
   * @param pipeB the second pipe
   * @param f the binary function to apply
   * @return a pipe containing the result of the bind operation
   */
  public static <A, B, C> Pipe<C> flatMap2(Pipe<A> pipeA, Pipe<B> pipeB, BiFunction<A, B, Pipe<C>> f) {
    return new Pipe<>(Flows.bind2(pipeA.flow, pipeB.flow, (a, b) -> f.apply(a, b).flow));
  }

  /**
   * Variant of monadic bind which takes three monadic arguments and an arity-3 function.
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param other the second pipe
   * @param other2 the third pipe
   * @param f the arity-3 function to apply
   * @return a pipe containing the result of the bind operation
   */
  public <B, C, D> Pipe<D> flatMap3(
      Pipe<B> other,
      Pipe<C> other2,
      Function3<A, B, C, Pipe<D>> f) {
    return flatMap3(this, other, other2, f);
  }

  /**
   * Variant of monadic bind which takes three monadic arguments and an arity-3 function.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param pipeA the first pipe
   * @param pipeB the second pipe
   * @param pipeC the third pipe
   * @param f the arity-3 function to apply
   * @return a pipe containing the result of the bind operation
   */
  public static <A, B, C, D> Pipe<D> flatMap3(
      Pipe<A> pipeA,
      Pipe<B> pipeB,
      Pipe<C> pipeC,
      Function3<A, B, C, Pipe<D>> f) {
    return new Pipe<>(Flows.bind3(pipeA.flow, pipeB.flow, pipeC.flow, (a, b, c) -> f.apply(a, b, c).flow));
  }

  /**
   * Map a function over a pipe, producing another pipe.
   * @param <B> the result type
   * @param f the function to apply
   * @return a pipe containing the result of applying the function
   */
  public <B> Pipe<B> map(Function<A, B> f) {
    return map(this, f);
  }

  /**
   * Map a function over a pipe, producing another pipe.
   * @param <A> the input type
   * @param <B> the result type
   * @param pipe the input pipe
   * @param f the function to apply
   * @return a pipe containing the result of applying the function
   */
  public static <A, B> Pipe<B> map(Pipe<A> pipe, Function<A, B> f) {
    return new Pipe<>(Flows.map(pipe.flow, f));
  }

  /**
   * Map a bifunction over two pipes, producing a pipe.
   * @param <B> the second input type
   * @param <C> the result type
   * @param other the second pipe
   * @param f the bifunction to apply
   * @return a pipe containing the result of applying the bifunction
   */
  public <B, C> Pipe<C> map2(Pipe<B> other, BiFunction<A, B, C> f) {
    return map2(this, other, f);
  }

  /**
   * Map a bifunction over two pipes, producing a pipe.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the result type
   * @param pipeA the first pipe
   * @param pipeB the second pipe
   * @param f the bifunction to apply
   * @return a pipe containing the result of applying the bifunction
   */
  public static <A, B, C> Pipe<C> map2(Pipe<A> pipeA, Pipe<B> pipeB, BiFunction<A, B, C> f) {
    return new Pipe<>(Flows.map2(pipeA.flow, pipeB.flow, f));
  }

  /**
   * Map an arity-3 function over three pipes, producing a pipe.
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param other the second pipe
   * @param other2 the third pipe
   * @param f the arity-3 function to apply
   * @return a pipe containing the result of applying the function
   */
  public <B, C, D> Pipe<D> map3(Pipe<B> other, Pipe<C> other2, Function3<A, B, C, D> f) {
    return map3(this, other, other2, f);
  }

  /**
   * Map an arity-3 function over three pipes, producing a pipe.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the result type
   * @param pipeA the first pipe
   * @param pipeB the second pipe
   * @param pipeC the third pipe
   * @param f the arity-3 function to apply
   * @return a pipe containing the result of applying the function
   */
  public static <A, B, C, D> Pipe<D> map3(Pipe<A> pipeA, Pipe<B> pipeB, Pipe<C> pipeC, Function3<A, B, C, D> f) {
    return new Pipe<>(Flows.map3(pipeA.flow, pipeB.flow, pipeC.flow, f));
  }

  /**
   * Map an arity-4 function over four pipes, producing a pipe.
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the fourth input type
   * @param <E> the result type
   * @param other the second pipe
   * @param other2 the third pipe
   * @param other3 the fourth pipe
   * @param f the arity-4 function to apply
   * @return a pipe containing the result of applying the function
   */
  public <B, C, D, E> Pipe<E> map4(
      Pipe<B> other,
      Pipe<C> other2,
      Pipe<D> other3,
      Function4<A, B, C, D, E> f) {
    return map4(this, other, other2, other3, f);
  }

  /**
   * Map an arity-4 function over four pipes, producing a pipe.
   * @param <A> the first input type
   * @param <B> the second input type
   * @param <C> the third input type
   * @param <D> the fourth input type
   * @param <E> the result type
   * @param pipeA the first pipe
   * @param pipeB the second pipe
   * @param pipeC the third pipe
   * @param pipeD the fourth pipe
   * @param f the arity-4 function to apply
   * @return a pipe containing the result of applying the function
   */
  public static <A, B, C, D, E> Pipe<E> map4(
      Pipe<A> pipeA,
      Pipe<B> pipeB,
      Pipe<C> pipeC,
      Pipe<D> pipeD,
      Function4<A, B, C, D, E> f) {
    return new Pipe<>(Flows.map4(pipeA.flow, pipeB.flow, pipeC.flow, pipeD.flow, f));
  }

  /**
   * Map a monadic function over a list, producing a pipe of lists.
   * @param <A> the input type
   * @param <B> the result type
   * @param as the input list
   * @param f the monadic function to apply
   * @return a pipe containing a list of results
   */
  public static <A, B> Pipe<List<B>> mapM(List<A> as, Function<A, Pipe<B>> f) {
    return new Pipe<>(Flows.mapM(as, a -> f.apply(a).flow));
  }

  /**
   * Map a monadic function over an array, producing a pipe of lists.
   * @param <A> the input type
   * @param <B> the result type
   * @param xs the input array
   * @param f the monadic function to apply
   * @return a pipe containing a list of results
   */
  public static <A, B> Pipe<List<B>> mapM(A[] xs, Function<A, Pipe<B>> f) {
    return new Pipe<>(Flows.mapM(xs, a -> f.apply(a).flow));
  }

  /**
   * Map a monadic function over the keys of a map, and another monadic function over the values of a map,
   * producing a pipe of maps.
   * @param <K1> the input key type
   * @param <V1> the input value type
   * @param <K2> the result key type
   * @param <V2> the result value type
   * @param xs the input map
   * @param kf the monadic function to apply to keys
   * @param vf the monadic function to apply to values
   * @return a pipe containing a map of results
   */
  public static <K1, V1, K2, V2> Pipe<Map<K2, V2>> mapM(Map<K1, V1> xs,
      Function<K1, Pipe<K2>> kf,
      Function<V1, Pipe<V2>> vf) {
    return new Pipe<>(Flows.mapM(xs, x -> kf.apply(x).flow, x -> vf.apply(x).flow));
  }

  /**
   * Map a monadic function over an optional value, producing a pipe of optionals.
   * @param <A> the input type
   * @param <B> the result type
   * @param xs the input optional
   * @param f the monadic function to apply
   * @return a pipe containing an optional result
   */
  public static <A, B> Pipe<Opt<B>> mapM(Opt<A> xs, Function<A, Pipe<B>> f) {
    return new Pipe<>(Flows.mapM(xs, x -> f.apply(x).flow));
  }

  /**
   * Map a monadic function over a set, producing a pipe of sets.
   * @param <A> the input type
   * @param <B> the result type
   * @param as the input set
   * @param f the monadic function to apply
   * @return a pipe containing a set of results
   */
  public static <A, B> Pipe<Set<B>> mapM(Set<A> as, Function<A, Pipe<B>> f) {
    return new Pipe<>(Flows.mapM(as, x -> f.apply(x).flow));
  }

  /**
   * Continue a pipe after adding a warning message.
   * @param message the warning message to add
   * @return a pipe with the warning message added
   */
  public Pipe<A> warn(String message) {
    return warn(message, this);
  }

  /**
   * Continue a pipe after adding a warning message.
   * @param <A> the type of the pipe's value
   * @param message the warning message to add
   * @param pipe the pipe to add the warning to
   * @return a pipe with the warning message added
   */
  public static <A> Pipe<A> warn(String message, Pipe<A> pipe) {
    return new Pipe<>(Flows.warn(message, pipe.flow));
  }

  /**
   * Check whether a given value satisfies a list of predicates, returning the value itself if all checks are
   * successful, or a failure pipe for the first predicate that fails.
   * @param <A> the type of the input value
   * @param input the value to check
   * @param predicates the predicates to check against the input value
   * @return a pipe containing the input value if all predicates pass, or a failure pipe otherwise
   */
  public static <A> Pipe<A> check(A input, Function<A, Opt<String>>... predicates) {
    return new Pipe<>(Flows.check(input, predicates));
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
  public static <A, B, C> Function<A, Pipe<C>> compose(Function<A, Pipe<B>> f, Function<B, Pipe<C>> g) {
    Function<A, Flow<Unit, C>> fg = Flows.compose(a -> f.apply(a).flow, b -> g.apply(b).flow);
    return a -> new Pipe<>(fg.apply(a));
  }

  /**
   * Produce a failure pipe with the provided message.
   * @param <A> the type of the pipe's value
   * @param msg the error message
   * @return a failure pipe with the provided message
   */
  public static <A> Pipe<A> fail(String msg) {
    return new Pipe<>(Flows.fail(msg));
  }

  /**
   * Produce a failure pipe with the provided message and additional information from a Throwable.
   * @param <A> the type of the pipe's value
   * @param msg the error message
   * @param cause the throwable that caused the failure
   * @return a failure pipe with the provided message and cause
   */
  public static <A> Pipe<A> fail(String msg, Throwable cause) {
    return new Pipe<>(Flows.fail(msg, cause));
  }

  /**
   * Produce a given object as a pure pipe; the value is guaranteed to be present,
   * and neither state nor trace are modified.
   * @param <A> the type of the value
   * @param obj the value to wrap in a pure pipe
   * @return a pure pipe containing the value
   */
  public static <A> Pipe<A> pure(A obj) {
    return new Pipe<>(Flows.pure(obj));
  }

  /**
   * Evaluate each pipe from left to right, and produce a pipe of the resulting list.
   * Analogous to the sequence function in Haskell.
   * @param <A> the type of the elements
   * @param elements the list of pipes to sequence
   * @return a pipe containing a list of the results
   */
  public static <A> Pipe<List<A>> sequence(List<Pipe<A>> elements) {
    List<Flow<Unit, A>> flows = new ArrayList<>(elements.size());
    for (Pipe<A> p : elements) {
      flows.add(p.flow);
    }
    return new Pipe<>(Flows.sequence(flows));
  }

  /**
   * Produce an error pipe indicating an unexpected value.
   * For example, if you expect a string but find an integer, use unexpected("string", myInt).
   * @param <A> the expected type
   * @param cat the category of the expected value
   * @param obj the unexpected object
   * @return an error pipe indicating the unexpected value
   */
  public static <A> Pipe<A> unexpected(String cat, Object obj) {
    return new Pipe<>(Flows.unexpected(cat, obj));
  }

  /**
   * Produce an error pipe indicating an unexpected class of value.
   * @param <A> the expected type
   * @param cat the category of the expected value
   * @param obj the object with an unexpected class
   * @return an error pipe indicating the unexpected class
   */
  public static <A> Pipe<A> unexpectedClass(String cat, Object obj) {
    return new Pipe<>(Flows.unexpectedClass(cat, obj));
  }
}
