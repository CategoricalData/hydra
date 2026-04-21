// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as Environment from "./environment.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as Lexical from "./lexical.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLiterals from "./lib/literals.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMath from "./lib/math.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Resolution from "./resolution.js";
import * as Rewriting from "./rewriting.js";
import * as Scoping from "./scoping.js";
import * as Sorting from "./sorting.js";
import * as Strip from "./strip.js";
import * as Substitution from "./substitution.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function augmentBindingsWithNewFreeVars(cx: Graph.Graph): ((x: ReadonlySet<Core.Name>) => ((x: ReadonlyArray<Core.Binding>) => readonly [ReadonlyArray<Core.Binding>, Typing.TermSubst])) {
  return ((boundVars: ReadonlySet<Core.Name>) => ((bindings: ReadonlyArray<Core.Binding>) => (() => {
  const types = LibMaps.map(Scoping.typeSchemeToFType)(((_x) => _x.boundTypes)(cx));
  return (() => {
  const wrapAfterTypeLambdas = ((vars: ReadonlyArray<readonly [Core.Name, Core.Type | null]>) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "typeLambda": return ((tl: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(tl),
    body: wrapAfterTypeLambdas(vars)(((_x) => _x.body)(tl))
  }) }))((_m as any).value);
    default: return LibLists.foldl(((t: Core.Term) => ((p: readonly [Core.Name, Core.Type | null]) => ({ tag: "lambda", value: ({
    parameter: LibPairs.first(p),
    domain: LibPairs.second(p),
    body: t
  }) }))))(term)(LibLists.reverse(vars))(_m);
  }
})()));
  return (() => {
  const augment = ((b: Core.Binding) => (() => {
  const freeVars = LibSets.toList(LibSets.intersection(boundVars)(Variables.freeVariablesInTerm(((_x) => _x.term)(b))));
  return (() => {
  const varTypePairs = LibLists.map(((v: Core.Name) => [v, LibMaps.lookup(v)(types)]))(freeVars);
  return (() => {
  const varTypes = LibMaybes.cat(LibLists.map(LibPairs.second)(varTypePairs));
  return LibLogic.ifElse(LibLogic.or(LibLists.null_(freeVars))(LibLogic.not(LibEquality.equal(LibLists.length(varTypes))(LibLists.length(varTypePairs)))))([b, null])([({
    name: ((_x) => _x.name)(b),
    term: wrapAfterTypeLambdas(varTypePairs)(((_x) => _x.term)(b)),
    type: LibMaybes.map(((ts: Core.TypeScheme) => ({
    variables: ((_x) => _x.variables)(ts),
    type: LibLists.foldl(((acc: Core.Type) => ((t: Core.Type) => ({ tag: "function", value: ({
    domain: t,
    codomain: acc
  }) }))))(((_x) => _x.type)(ts))(LibLists.reverse(varTypes)),
    constraints: ((_x) => _x.constraints)(ts)
  })))(((_x) => _x.type)(b))
  }), [((_x) => _x.name)(b), LibLists.foldl(((t: Core.Term) => ((v: Core.Name) => ({ tag: "application", value: ({
    function: t,
    argument: ({ tag: "variable", value: v })
  }) }))))(({ tag: "variable", value: ((_x) => _x.name)(b) }))(freeVars)]]);
})();
})();
})());
  return (() => {
  const results = LibLists.map(augment)(bindings);
  return [LibLists.map(LibPairs.first)(results), LibMaps.fromList(LibMaybes.cat(LibLists.map(LibPairs.second)(results)))];
})();
})();
})();
})()));
}

export function bindingIsPolymorphic(binding: Core.Binding): boolean {
  return LibMaybes.maybe(false)(((ts: Core.TypeScheme) => LibLogic.not(LibLists.null_(((_x) => _x.variables)(ts)))))(((_x) => _x.type)(binding));
}

export function bindingUsesContextTypeVars(cx: Graph.Graph): ((x: Core.Binding) => boolean) {
  return ((binding: Core.Binding) => LibMaybes.maybe(false)(((ts: Core.TypeScheme) => (() => {
  const freeInType = Variables.freeVariablesInType(((_x) => _x.type)(ts));
  return (() => {
  const contextTypeVars = ((_x) => _x.typeVariables)(cx);
  return LibLogic.not(LibSets.null_(LibSets.intersection(freeInType)(contextTypeVars)));
})();
})()))(((_x) => _x.type)(binding)));
}

export function countVarOccurrences(name: Core.Name): ((x: Core.Term) => number) {
  return ((term: Core.Term) => (() => {
  const childCount = LibLists.foldl(((acc: number) => ((t: Core.Term) => LibMath.add(acc)(countVarOccurrences(name)(t)))))(0)(Rewriting.subterms(term));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(LibEquality.equal(v)(name))(LibMath.add(1)(childCount))(childCount))((_m as any).value);
    default: return childCount(_m);
  }
})();
})());
}

export function hoistAllLetBindings(let0: Core.Let): Core.Let {
  return (() => {
  const emptyCx = ({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  });
  return hoistLetBindingsWithPredicate(((_: Core.Binding) => true))(shouldHoistAll)(emptyCx)(let0);
})();
}

export function hoistCaseStatements(v1: Graph.Graph): ((x: Core.Term) => Core.Term) {
  return ((v2: Core.Term) => hoistSubterms(shouldHoistCaseStatement)(v1)(v2));
}

export function hoistCaseStatementsInGraph(bindings: ReadonlyArray<Core.Binding>): ReadonlyArray<Core.Binding> {
  return (() => {
  const emptyTx = ({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  });
  return (() => {
  const term0 = ({ tag: "let", value: ({
    bindings: bindings,
    body: ({ tag: "unit" })
  }) });
  return (() => {
  const term1 = hoistCaseStatements(emptyTx)(term0);
  return Environment.termAsBindings(term1);
})();
})();
})();
}

export function hoistLetBindingsWithContext(isParentBinding: ((x: Core.Binding) => boolean)): ((x: Graph.Graph) => ((x: Core.Let) => Core.Let)) {
  return ((cx: Graph.Graph) => ((let0: Core.Let) => hoistLetBindingsWithPredicate(isParentBinding)(shouldHoistPolymorphic)(cx)(let0)));
}

export function hoistLetBindingsWithPredicate(isParentBinding: ((x: Core.Binding) => boolean)): ((x: ((x: Graph.Graph) => ((x: Core.Binding) => boolean))) => ((x: Graph.Graph) => ((x: Core.Let) => Core.Let))) {
  return ((shouldHoistBinding: ((x: Graph.Graph) => ((x: Core.Binding) => boolean))) => ((cx0: Graph.Graph) => ((let0: Core.Let) => (() => {
  const hoistOne = ((prefix: string) => ((cx: Graph.Graph) => ((pair: readonly [ReadonlyArray<readonly [Core.Binding, Core.Term]>, ReadonlySet<Core.Name>]) => ((bindingWithCapturedVars: readonly [Core.Binding, ReadonlyArray<Core.Name>]) => (() => {
  const bindingAndReplacementPairs = LibPairs.first(pair);
  return (() => {
  const alreadyUsedNames = LibPairs.second(pair);
  return (() => {
  const b = LibPairs.first(bindingWithCapturedVars);
  return (() => {
  const capturedTermVars = LibPairs.second(bindingWithCapturedVars);
  return (() => {
  const types = LibMaps.map(Scoping.typeSchemeToFType)(((_x) => _x.boundTypes)(cx));
  return (() => {
  const capturedTermVarTypePairs = LibLists.map(((v: Core.Name) => [v, LibMaps.lookup(v)(types)]))(capturedTermVars);
  return (() => {
  const capturedTermVarTypes = LibLists.map(((typ: Core.Type) => Strip.deannotateTypeParameters(typ)))(LibMaybes.cat(LibLists.map(LibPairs.second)(capturedTermVarTypePairs)));
  return (() => {
  const freeInBindingType = LibMaybes.maybe(LibSets.empty)(((ts: Core.TypeScheme) => Variables.freeVariablesInType(((_x) => _x.type)(ts))))(((_x) => _x.type)(b));
  return (() => {
  const freeInCapturedVarTypes = LibSets.unions(LibLists.map(((t: Core.Type) => Variables.freeVariablesInType(t)))(capturedTermVarTypes));
  return (() => {
  const capturedTypeVars = LibSets.toList(LibSets.intersection(((_x) => _x.typeVariables)(cx))(LibSets.union(freeInBindingType)(freeInCapturedVarTypes)));
  return (() => {
  const globalBindingName = Lexical.chooseUniqueName(alreadyUsedNames)(LibStrings.cat2(prefix)(((_x) => _x)(((_x) => _x.name)(b))));
  return (() => {
  const newUsedNames = LibSets.insert(globalBindingName)(alreadyUsedNames);
  return (() => {
  const newTypeScheme = LibLogic.ifElse(LibEquality.equal(LibLists.length(capturedTermVarTypes))(LibLists.length(capturedTermVarTypePairs)))(LibMaybes.map(((ts: Core.TypeScheme) => ({
    variables: LibLists.nub(LibLists.concat2(capturedTypeVars)(((_x) => _x.variables)(ts))),
    type: LibLists.foldl(((t: Core.Type) => ((a: Core.Type) => ({ tag: "function", value: ({
    domain: a,
    codomain: t
  }) }))))(((_x) => _x.type)(ts))(LibLists.reverse(capturedTermVarTypes)),
    constraints: ((_x) => _x.constraints)(ts)
  })))(((_x) => _x.type)(b)))(null);
  return (() => {
  const strippedTerm = Strip.stripTypeLambdas(((_x) => _x.term)(b));
  return (() => {
  const termWithLambdas = LibLists.foldl(((t: Core.Term) => ((p: readonly [Core.Name, Core.Type | null]) => ({ tag: "lambda", value: ({
    parameter: LibPairs.first(p),
    domain: LibMaybes.map(((dom: Core.Type) => Strip.deannotateTypeParameters(dom)))(LibPairs.second(p)),
    body: t
  }) }))))(strippedTerm)(LibLists.reverse(capturedTermVarTypePairs));
  return (() => {
  const termWithTypeLambdas = LibLists.foldl(((t: Core.Term) => ((v: Core.Name) => ({ tag: "typeLambda", value: ({
    parameter: v,
    body: t
  }) }))))(termWithLambdas)(LibLists.reverse(LibMaybes.maybe([])(((_x) => _x.variables))(newTypeScheme)));
  return (() => {
  const withTypeApps = LibLists.foldl(((t: Core.Term) => ((v: Core.Name) => ({ tag: "typeApplication", value: ({
    body: t,
    type: ({ tag: "variable", value: v })
  }) }))))(({ tag: "variable", value: globalBindingName }))(capturedTypeVars);
  return (() => {
  const replacement = LibLists.foldl(((t: Core.Term) => ((v: Core.Name) => ({ tag: "application", value: ({
    function: t,
    argument: ({ tag: "variable", value: v })
  }) }))))(withTypeApps)(capturedTermVars);
  return (() => {
  const newBindingAndReplacement = [({
    name: globalBindingName,
    term: termWithTypeLambdas,
    type: newTypeScheme
  }), replacement];
  return (() => {
  const newPairs = LibLists.cons(newBindingAndReplacement)(bindingAndReplacementPairs);
  return [newPairs, newUsedNames];
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()))));
  return (() => {
  const rewrite = ((prefix: string) => ((recurse: ((x: readonly [ReadonlyArray<t0>, t1]) => ((x: t2) => readonly [readonly [ReadonlyArray<Core.Binding>, ReadonlySet<Core.Name>], Core.Term]))) => ((cx: Graph.Graph) => ((bindingsAndNames: readonly [ReadonlyArray<Core.Binding>, t1]) => ((term: t2) => (() => {
  const previouslyFinishedBindings = LibPairs.first(bindingsAndNames);
  return (() => {
  const emptyBindingsAndNames = [[], LibPairs.second(bindingsAndNames)];
  return (() => {
  const result = recurse(emptyBindingsAndNames)(term);
  return (() => {
  const newBindingsAndNames = LibPairs.first(result);
  return (() => {
  const bindingsSoFar = LibPairs.first(newBindingsAndNames);
  return (() => {
  const alreadyUsedNames = LibPairs.second(newBindingsAndNames);
  return (() => {
  const newTerm = LibPairs.second(result);
  return (() => {
  const _m = newTerm;
  switch (_m.tag) {
    case "let": return ((l: Core.Let) => (() => {
  const body = ((_x) => _x.body)(l);
  return (() => {
  const partitionPair = LibLists.partition(((v1: Core.Binding) => shouldHoistBinding(cx)(v1)))(((_x) => _x.bindings)(l));
  return (() => {
  const hoistUs = LibPairs.first(partitionPair);
  return (() => {
  const keepUs = LibPairs.second(partitionPair);
  return (() => {
  const hoistedBindingNames = LibLists.map(((_x) => _x.name))(hoistUs);
  return (() => {
  const polyLetVariables = LibSets.fromList(LibLists.filter(((v: Core.Name) => LibMaybes.maybe(false)(Resolution.fTypeIsPolymorphic)(LibMaybes.map(Scoping.typeSchemeToFType)(LibMaps.lookup(v)(((_x) => _x.boundTypes)(cx))))))(LibSets.toList(LibSets.difference(LibSets.fromList(LibMaps.keys(((_x) => _x.boundTerms)(cx))))(((_x) => _x.lambdaVariables)(cx)))));
  return (() => {
  const boundTermVariables = LibSets.union(((_x) => _x.lambdaVariables)(cx))(LibSets.difference(LibSets.fromList(LibMaps.keys(((_x) => _x.boundTerms)(cx))))(((_x) => _x.lambdaVariables)(cx)));
  return (() => {
  const freeVariablesInEachBinding = LibLists.map(((b: Core.Binding) => LibSets.toList(LibSets.intersection(boundTermVariables)(Variables.freeVariablesInTerm(((_x) => _x.term)(b))))))(hoistUs);
  return (() => {
  const bindingDependencies = LibLists.map(((vars: ReadonlyArray<Core.Name>) => LibLists.partition(((v: Core.Name) => LibSets.member(v)(LibSets.fromList(hoistedBindingNames))))(vars)))(freeVariablesInEachBinding);
  return (() => {
  const bindingEdges = LibLists.zip(hoistedBindingNames)(LibLists.map(LibPairs.first)(bindingDependencies));
  return (() => {
  const bindingImmediateCapturedVars = LibLists.zip(hoistedBindingNames)(LibLists.map(LibPairs.second)(bindingDependencies));
  return (() => {
  const capturedVarsMap = LibMaps.fromList(Sorting.propagateTags(bindingEdges)(bindingImmediateCapturedVars));
  return (() => {
  const bindingsWithCapturedVars = LibLists.map(((b: Core.Binding) => [b, LibMaybes.maybe([])(((vars: ReadonlySet<Core.Name>) => LibSets.toList(LibSets.difference(vars)(polyLetVariables))))(LibMaps.lookup(((_x) => _x.name)(b))(capturedVarsMap))]))(hoistUs);
  return (() => {
  const hoistPairsAndNames = LibLists.foldl(((v1: readonly [ReadonlyArray<readonly [Core.Binding, Core.Term]>, ReadonlySet<Core.Name>]) => ((v2: readonly [Core.Binding, ReadonlyArray<Core.Name>]) => hoistOne(prefix)(cx)(v1)(v2))))([[], alreadyUsedNames])(bindingsWithCapturedVars);
  return (() => {
  const hoistPairs = LibLists.reverse(LibPairs.first(hoistPairsAndNames));
  return (() => {
  const hoistedBindings = LibLists.map(LibPairs.first)(hoistPairs);
  return (() => {
  const replacements = LibLists.map(LibPairs.second)(hoistPairs);
  return (() => {
  const finalUsedNames = LibPairs.second(hoistPairsAndNames);
  return (() => {
  const hoistNameReplacementPairs = LibLists.zip(LibLists.map(((_x) => _x.name))(hoistUs))(replacements);
  return (() => {
  const hoistBindingMap = LibMaps.fromList(LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), b]))(hoistUs));
  return (() => {
  const isCacheable = ((name: Core.Name) => (() => {
  const multiRef = LibEquality.gte(countVarOccurrences(name)(body))(2);
  return (() => {
  const isPoly = LibMaybes.maybe(false)(((b: Core.Binding) => bindingIsPolymorphic(b)))(LibMaps.lookup(name)(hoistBindingMap));
  return LibLogic.and(multiRef)(LibLogic.not(isPoly));
})();
})());
  return (() => {
  const singleRefPairs = LibLists.filter(((p: readonly [Core.Name, Core.Term]) => LibLogic.not(isCacheable(LibPairs.first(p)))))(hoistNameReplacementPairs);
  return (() => {
  const multiRefPairs = LibLists.filter(((p: readonly [Core.Name, Core.Term]) => isCacheable(LibPairs.first(p))))(hoistNameReplacementPairs);
  return (() => {
  const fullSubst = LibMaps.fromList(hoistNameReplacementPairs);
  return (() => {
  const bodyOnlySubst = LibMaps.fromList(singleRefPairs);
  return (() => {
  const bodySubst = Substitution.substituteInTerm(bodyOnlySubst)(body);
  return (() => {
  const cacheBindings = LibLists.map(((p: readonly [Core.Name, Core.Term]) => (() => {
  const origType = LibMaybes.maybe(null)(((b: Core.Binding) => ((_x) => _x.type)(b)))(LibMaps.lookup(LibPairs.first(p))(hoistBindingMap));
  return ({
    name: LibPairs.first(p),
    term: LibPairs.second(p),
    type: origType
  });
})()))(multiRefPairs);
  return (() => {
  const bodyWithCache = LibLogic.ifElse(LibLists.null_(cacheBindings))(bodySubst)(({ tag: "let", value: ({
    bindings: cacheBindings,
    body: bodySubst
  }) }));
  return (() => {
  const keepUsSubst = LibLists.map(((v1: Core.Binding) => Substitution.substituteInBinding(fullSubst)(v1)))(keepUs);
  return (() => {
  const hoistedBindingsSubst = LibLists.map(((v1: Core.Binding) => Substitution.substituteInBinding(fullSubst)(v1)))(hoistedBindings);
  return (() => {
  const bindingsSoFarSubst = LibLists.map(((v1: Core.Binding) => Substitution.substituteInBinding(fullSubst)(v1)))(bindingsSoFar);
  return (() => {
  const augmentResult = augmentBindingsWithNewFreeVars(cx)(LibSets.difference(boundTermVariables)(polyLetVariables))(bindingsSoFarSubst);
  return (() => {
  const bindingsSoFarAugmented = LibPairs.first(augmentResult);
  return (() => {
  const augmentSubst = LibPairs.second(augmentResult);
  return (() => {
  const hoistedBindingsFinal = LibLists.map(((v1: Core.Binding) => Substitution.substituteInBinding(augmentSubst)(v1)))(hoistedBindingsSubst);
  return (() => {
  const bindingsSoFarFinal = LibLists.map(((v1: Core.Binding) => Substitution.substituteInBinding(augmentSubst)(v1)))(bindingsSoFarAugmented);
  return (() => {
  const bodyFinal = Substitution.substituteInTerm(augmentSubst)(bodyWithCache);
  return (() => {
  const keepUsFinal = LibLists.map(((v1: Core.Binding) => Substitution.substituteInBinding(augmentSubst)(v1)))(keepUsSubst);
  return (() => {
  const finalTerm = LibLogic.ifElse(LibLists.null_(keepUsFinal))(bodyFinal)(({ tag: "let", value: ({
    bindings: keepUsFinal,
    body: bodyFinal
  }) }));
  return [[LibLists.concat([previouslyFinishedBindings, hoistedBindingsFinal, bindingsSoFarFinal]), finalUsedNames], finalTerm];
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
    default: return [[LibLists.concat2(previouslyFinishedBindings)(bindingsSoFar), alreadyUsedNames], newTerm](_m);
  }
})();
})();
})();
})();
})();
})();
})();
})())))));
  return (() => {
  const cx1 = Scoping.extendGraphForLet(((c: Graph.Graph) => ((b: Core.Binding) => null)))(cx0)(let0);
  return (() => {
  const forActiveBinding = ((b: Core.Binding) => (() => {
  const prefix = LibStrings.cat2(((_x) => _x)(((_x) => _x.name)(b)))("_");
  return (() => {
  const init = [[], LibSets.singleton(((_x) => _x.name)(b))];
  return (() => {
  const resultPair = Rewriting.rewriteAndFoldTermWithGraph(((v1: ((x: readonly [ReadonlyArray<Core.Binding>, ReadonlySet<Core.Name>]) => ((x: Core.Term) => readonly [readonly [ReadonlyArray<Core.Binding>, ReadonlySet<Core.Name>], Core.Term]))) => ((v2: Graph.Graph) => ((v3: readonly [ReadonlyArray<Core.Binding>, ReadonlySet<Core.Name>]) => ((v4: Core.Term) => rewrite(prefix)(v1)(v2)(v3)(v4))))))(cx1)(init)(((_x) => _x.term)(b));
  return (() => {
  const resultBindings = LibPairs.first(LibPairs.first(resultPair));
  return (() => {
  const resultTerm = LibPairs.second(resultPair);
  return LibLists.cons(({
    name: ((_x) => _x.name)(b),
    term: resultTerm,
    type: ((_x) => _x.type)(b)
  }))(resultBindings);
})();
})();
})();
})();
})());
  return (() => {
  const forBinding = ((b: Core.Binding) => LibLogic.ifElse(isParentBinding(b))(forActiveBinding(b))([b]));
  return ({
    bindings: LibLists.concat(LibLists.map(forBinding)(((_x) => _x.bindings)(let0))),
    body: ((_x) => _x.body)(let0)
  });
})();
})();
})();
})();
})())));
}

export function hoistPolymorphicLetBindings(isParentBinding: ((x: Core.Binding) => boolean)): ((x: Core.Let) => Core.Let) {
  return ((let0: Core.Let) => (() => {
  const emptyCx = ({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  });
  return hoistLetBindingsWithPredicate(isParentBinding)(shouldHoistPolymorphic)(emptyCx)(let0);
})());
}

export function hoistSubterms(shouldHoist: ((x: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]) => boolean)): ((x: Graph.Graph) => ((x: Core.Term) => Core.Term)) {
  return ((cx0: Graph.Graph) => ((term0: Core.Term) => (() => {
  const processImmediateSubterm = ((cx: Graph.Graph) => ((counter: number) => ((namePrefix: string) => ((pathPrefix: ReadonlyArray<Paths.SubtermStep>) => ((subterm: Core.Term) => (() => {
  const baselineLambdaVars = ((_x) => _x.lambdaVariables)(cx);
  return (() => {
  const collectAndReplace = ((recurse: ((x: readonly [number, ReadonlyArray<Core.Binding>]) => ((x: Core.Term) => readonly [readonly [number, ReadonlyArray<Core.Binding>], Core.Term]))) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((cxInner: Graph.Graph) => ((acc: readonly [number, ReadonlyArray<Core.Binding>]) => ((term: Core.Term) => (() => {
  const currentCounter = LibPairs.first(acc);
  return (() => {
  const collectedBindings = LibPairs.second(acc);
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "let": return ((_: Core.Let) => [acc, term])((_m as any).value);
    case "typeLambda": return ((_: Core.TypeLambda) => [acc, term])((_m as any).value);
    default: return (() => {
  const result = recurse(acc)(term);
  return (() => {
  const newAcc = LibPairs.first(result);
  return (() => {
  const processedTerm = LibPairs.second(result);
  return (() => {
  const newCounter = LibPairs.first(newAcc);
  return (() => {
  const newBindings = LibPairs.second(newAcc);
  return (() => {
  const fullPath = LibLists.concat2(pathPrefix)(path);
  return LibLogic.ifElse(shouldHoist([fullPath, processedTerm]))((() => {
  const proposedName = LibStrings.cat(["_hoist_", namePrefix, "_", LibLiterals.showInt32(newCounter)]);
  return (() => {
  const existingNames = LibSets.fromList(LibLists.map(((b: Core.Binding) => ((_x) => _x.name)(b)))(newBindings));
  return (() => {
  const freeVarsInSubterm = Variables.freeVariablesInTerm(subterm);
  return (() => {
  const allReserved = LibSets.union(existingNames)(freeVarsInSubterm);
  return (() => {
  const bindingName = Lexical.chooseUniqueName(allReserved)(proposedName);
  return (() => {
  const allLambdaVars = ((_x) => _x.lambdaVariables)(cxInner);
  return (() => {
  const newLambdaVars = LibSets.difference(allLambdaVars)(baselineLambdaVars);
  return (() => {
  const freeVars = Variables.freeVariablesInTerm(processedTerm);
  return (() => {
  const capturedVars = LibSets.toList(LibSets.intersection(newLambdaVars)(freeVars));
  return (() => {
  const typeMap = LibMaps.map(Scoping.typeSchemeToFType)(((_x) => _x.boundTypes)(cxInner));
  return (() => {
  const wrappedTerm = LibLists.foldl(((body: Core.Term) => ((varName: Core.Name) => ({ tag: "lambda", value: ({
    parameter: varName,
    domain: LibMaps.lookup(varName)(typeMap),
    body: body
  }) }))))(processedTerm)(LibLists.reverse(capturedVars));
  return (() => {
  const reference = LibLists.foldl(((fn: Core.Term) => ((varName: Core.Name) => ({ tag: "application", value: ({
    function: fn,
    argument: ({ tag: "variable", value: varName })
  }) }))))(({ tag: "variable", value: bindingName }))(capturedVars);
  return (() => {
  const newBinding = ({
    name: bindingName,
    term: wrappedTerm,
    type: null
  });
  return [[LibMath.add(newCounter)(1), LibLists.cons(newBinding)(newBindings)], reference];
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})())([newAcc, processedTerm]);
})();
})();
})();
})();
})();
})()(_m);
  }
})();
})();
})())))));
  return (() => {
  const result = Rewriting.rewriteAndFoldTermWithGraphAndPath(collectAndReplace)(cx)([counter, []])(subterm);
  return (() => {
  const finalAcc = LibPairs.first(result);
  return (() => {
  const transformedSubterm = LibPairs.second(result);
  return (() => {
  const finalCounter = LibPairs.first(finalAcc);
  return (() => {
  const bindings = LibPairs.second(finalAcc);
  return LibLogic.ifElse(LibLists.null_(bindings))([finalCounter, transformedSubterm])((() => {
  const localLet = ({ tag: "let", value: ({
    bindings: LibLists.reverse(bindings),
    body: transformedSubterm
  }) });
  return [finalCounter, localLet];
})());
})();
})();
})();
})();
})();
})();
})())))));
  return (() => {
  const processLetTerm = ((cx: Graph.Graph) => ((counter: t0) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return (() => {
  const processBinding = ((acc: ReadonlyArray<Core.Binding>) => ((binding: Core.Binding) => (() => {
  const namePrefix = LibStrings.intercalate("_")(LibStrings.splitOn(".")(((_x) => _x)(((_x) => _x.name)(binding))));
  return (() => {
  const bindingPathPrefix = LibLists.concat2(path)([({ tag: "letBinding", value: ((_x) => _x.name)(binding) })]);
  return (() => {
  const result = processImmediateSubterm(cx)(1)(namePrefix)(bindingPathPrefix)(((_x) => _x.term)(binding));
  return (() => {
  const newValue = LibPairs.second(result);
  return (() => {
  const newBinding = ({
    name: ((_x) => _x.name)(binding),
    term: newValue,
    type: ((_x) => _x.type)(binding)
  });
  return LibLists.cons(newBinding)(acc);
})();
})();
})();
})();
})()));
  return (() => {
  const newBindingsReversed = LibLists.foldl(processBinding)([])(bindings);
  return (() => {
  const newBindings = LibLists.reverse(newBindingsReversed);
  return (() => {
  const bodyPathPrefix = LibLists.concat2(path)([({ tag: "letBody" })]);
  return (() => {
  const firstBindingName = LibMaybes.maybe("body")(((b: Core.Binding) => LibStrings.intercalate("_")(LibStrings.splitOn(".")(((_x) => _x)(((_x) => _x.name)(b))))))(LibLists.safeHead(bindings));
  return (() => {
  const bodyPrefix = LibStrings.cat2(firstBindingName)("_body");
  return (() => {
  const bodyResult = processImmediateSubterm(cx)(1)(bodyPrefix)(bodyPathPrefix)(body);
  return (() => {
  const newBody = LibPairs.second(bodyResult);
  return [counter, ({ tag: "let", value: ({
    bindings: newBindings,
    body: newBody
  }) })];
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()))));
  return (() => {
  const rewrite = ((recurse: ((x: t0) => ((x: Core.Term) => readonly [t1, Core.Term]))) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((cx: Graph.Graph) => ((counter: t0) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => (() => {
  const recursed = recurse(counter)(term);
  return (() => {
  const newCounter = LibPairs.first(recursed);
  return (() => {
  const recursedTerm = LibPairs.second(recursed);
  return (() => {
  const _m = recursedTerm;
  switch (_m.tag) {
    case "let": return ((lt2: Core.Let) => processLetTerm(cx)(newCounter)(path)(lt2))((_m as any).value);
    default: return [newCounter, recursedTerm](_m);
  }
})();
})();
})();
})())((_m as any).value);
    default: return recurse(counter)(term)(_m);
  }
})())))));
  return LibPairs.second(Rewriting.rewriteAndFoldTermWithGraphAndPath(rewrite)(cx0)(1)(term0));
})();
})();
})()));
}

export function isApplicationFunction(acc: Paths.SubtermStep): boolean {
  return (() => {
  const _m = acc;
  switch (_m.tag) {
    case "applicationFunction": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isLambdaBody(acc: Paths.SubtermStep): boolean {
  return (() => {
  const _m = acc;
  switch (_m.tag) {
    case "lambdaBody": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isUnionElimination(term: Core.Term): boolean {
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "cases": return ((_: Core.CaseStatement) => true)((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isUnionEliminationApplication(term: Core.Term): boolean {
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => isUnionElimination(Strip.deannotateAndDetypeTerm(((_x) => _x.function)(app))))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function normalizePathForHoisting(path: ReadonlyArray<Paths.SubtermStep>): ReadonlyArray<Paths.SubtermStep> {
  return (() => {
  const go = ((remaining: ReadonlyArray<Paths.SubtermStep>) => LibLogic.ifElse(LibLogic.or(LibLists.null_(remaining))(LibLists.null_(LibLists.tail(remaining))))(remaining)((() => {
  const first = LibLists.head(remaining);
  return (() => {
  const second = LibLists.head(LibLists.tail(remaining));
  return (() => {
  const rest = LibLists.tail(LibLists.tail(remaining));
  return LibLogic.ifElse(LibLogic.and(isApplicationFunction(first))(isLambdaBody(second)))(LibLists.cons(({ tag: "letBody" }))(go(rest)))(LibLists.cons(first)(go(LibLists.tail(remaining))));
})();
})();
})()));
  return go(path);
})();
}

export function shouldHoistAll<t0, t1>(_: t0): ((x: t1) => boolean) {
  return ((_2: t1) => true);
}

export function shouldHoistCaseStatement(pathAndTerm: readonly [ReadonlyArray<Paths.SubtermStep>, Core.Term]): boolean {
  return (() => {
  const path = LibPairs.first(pathAndTerm);
  return (() => {
  const term = LibPairs.second(pathAndTerm);
  return LibLogic.ifElse(LibLogic.not(LibLogic.or(isUnionElimination(term))(isUnionEliminationApplication(term))))(false)((() => {
  const finalState = LibLists.foldl(((st: readonly [boolean, boolean]) => ((acc: Paths.SubtermStep) => updateHoistState(acc)(st))))([true, false])(path);
  return LibLogic.not(LibPairs.first(finalState));
})());
})();
})();
}

export function shouldHoistPolymorphic(cx: Graph.Graph): ((x: Core.Binding) => boolean) {
  return ((binding: Core.Binding) => LibLogic.or(bindingIsPolymorphic(binding))(bindingUsesContextTypeVars(cx)(binding)));
}

export function updateHoistState(accessor: Paths.SubtermStep): ((x: readonly [boolean, boolean]) => readonly [boolean, boolean]) {
  return ((state: readonly [boolean, boolean]) => (() => {
  const atTop = LibPairs.first(state);
  return (() => {
  const usedApp = LibPairs.second(state);
  return LibLogic.ifElse(LibLogic.not(atTop))([false, usedApp])((() => {
  const _m = accessor;
  switch (_m.tag) {
    case "annotatedBody": return ((_: void) => [true, usedApp])((_m as any).value);
    case "letBody": return ((_: void) => [true, usedApp])((_m as any).value);
    case "letBinding": return ((_: Core.Name) => [true, usedApp])((_m as any).value);
    case "lambdaBody": return ((_: void) => LibLogic.ifElse(usedApp)([false, true])([true, false]))((_m as any).value);
    case "unionCasesBranch": return ((_: Core.Name) => LibLogic.ifElse(usedApp)([false, true])([true, false]))((_m as any).value);
    case "unionCasesDefault": return ((_: void) => LibLogic.ifElse(usedApp)([false, true])([true, false]))((_m as any).value);
    case "applicationFunction": return ((_: void) => LibLogic.ifElse(usedApp)([false, true])([true, true]))((_m as any).value);
    case "applicationArgument": return ((_: void) => [false, usedApp])((_m as any).value);
    default: return [false, usedApp](_m);
  }
})());
})();
})());
}
