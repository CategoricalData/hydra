// Note: this is an automatically generated file. Do not edit.

package hydra.python;

/**
 * Python naming utilities: encoding Hydra names as Python names
 */
public interface Names {
  static <T0, T1> hydra.python.syntax.Name encodeConstantForFieldName(T0 env, T1 tname, hydra.core.Name fname) {
    return new hydra.python.syntax.Name(hydra.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(),
      new hydra.util.CaseConvention.UpperSnake(),
      (fname).value));
  }

  static <T0, T1> hydra.python.syntax.Name encodeConstantForTypeName(T0 env, T1 tname) {
    return new hydra.python.syntax.Name("TYPE_");
  }

  static hydra.python.syntax.Name encodeEnumValue(hydra.python.environment.PythonEnvironment v1, hydra.core.Name v2) {
    return hydra.python.Names.encodeName(
      false,
      new hydra.util.CaseConvention.UpperSnake(),
      v1,
      v2);
  }

  static hydra.python.syntax.Name encodeFieldName(hydra.python.environment.PythonEnvironment env, hydra.core.Name fname) {
    return hydra.python.Names.encodeName(
      false,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      fname);
  }

  static hydra.python.syntax.Name encodeName(Boolean isQualified, hydra.util.CaseConvention conv, hydra.python.environment.PythonEnvironment env, hydra.core.Name name) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.python.syntax.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((env).boundTypeVariables));
    hydra.packaging.Namespaces<hydra.python.syntax.DottedName> namespaces = (env).namespaces;
    hydra.util.Lazy<hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>> focusPair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.packaging.Namespaces<hydra.python.syntax.DottedName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<hydra.packaging.Namespace> focusNs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(focusPair.get()));
    hydra.packaging.QualifiedName qualName = hydra.Names.qualifyName(name);
    String local = (qualName).local;
    hydra.util.Maybe<hydra.packaging.Namespace> mns = (qualName).namespace;
    String pyLocal = hydra.python.Names.sanitizePythonName(hydra.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(),
      conv,
      local));
    java.util.function.Function<hydra.packaging.Namespace, String> pyNs = (java.util.function.Function<hydra.packaging.Namespace, String>) (nsVal -> hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, String>) (v1 -> hydra.Formatting.convertCase(
          new hydra.util.CaseConvention.Camel(),
          new hydra.util.CaseConvention.LowerSnake(),
          v1)),
        hydra.lib.strings.SplitOn.apply(
          ".",
          (nsVal).value))));
    return hydra.lib.logic.IfElse.lazy(
      isQualified,
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            mns,
            hydra.util.Maybe.just(focusNs.get())),
          () -> new hydra.python.syntax.Name(hydra.lib.logic.IfElse.lazy(
            hydra.python.Names.useFutureAnnotations(),
            () -> pyLocal,
            () -> hydra.python.Serde.escapePythonString(
              true,
              pyLocal))),
          () -> hydra.lib.maybes.Maybe.applyLazy(
            () -> new hydra.python.syntax.Name(pyLocal),
            (java.util.function.Function<hydra.packaging.Namespace, hydra.python.syntax.Name>) (nsVal -> new hydra.python.syntax.Name(hydra.lib.strings.Cat2.apply(
              (pyNs).apply(nsVal),
              hydra.lib.strings.Cat2.apply(
                ".",
                pyLocal)))),
            mns)),
        (java.util.function.Function<hydra.python.syntax.Name, hydra.python.syntax.Name>) (n -> n),
        hydra.lib.maps.Lookup.apply(
          name,
          boundVars.get())),
      () -> new hydra.python.syntax.Name(pyLocal));
  }

  static hydra.python.syntax.Name encodeNameQualified(hydra.python.environment.PythonEnvironment env, hydra.core.Name name) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.python.syntax.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((env).boundTypeVariables));
    hydra.packaging.Namespaces<hydra.python.syntax.DottedName> namespaces = (env).namespaces;
    hydra.util.Lazy<hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>> focusPair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.packaging.Namespaces<hydra.python.syntax.DottedName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<hydra.packaging.Namespace> focusNs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(focusPair.get()));
    hydra.packaging.QualifiedName qualName = hydra.Names.qualifyName(name);
    String local = (qualName).local;
    hydra.util.Maybe<hydra.packaging.Namespace> mns = (qualName).namespace;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          mns,
          hydra.util.Maybe.just(focusNs.get())),
        () -> new hydra.python.syntax.Name(hydra.lib.logic.IfElse.lazy(
          hydra.python.Names.useFutureAnnotations(),
          () -> local,
          () -> hydra.python.Serde.escapePythonString(
            true,
            local))),
        () -> new hydra.python.syntax.Name(hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Map.apply(
            hydra.python.Names::sanitizePythonName,
            hydra.lib.strings.SplitOn.apply(
              ".",
              (name).value))))),
      (java.util.function.Function<hydra.python.syntax.Name, hydra.python.syntax.Name>) (n -> n),
      hydra.lib.maps.Lookup.apply(
        name,
        boundVars.get()));
  }

  static hydra.python.syntax.DottedName encodeNamespace(hydra.packaging.Namespace nsVal) {
    return new hydra.python.syntax.DottedName(hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.python.syntax.Name>) (part -> new hydra.python.syntax.Name(hydra.Formatting.convertCase(
        new hydra.util.CaseConvention.Camel(),
        new hydra.util.CaseConvention.LowerSnake(),
        part))),
      hydra.lib.strings.SplitOn.apply(
        ".",
        (nsVal).value)));
  }

  static hydra.python.syntax.Name encodeTypeVariable(hydra.core.Name name) {
    return new hydra.python.syntax.Name(hydra.Formatting.capitalize((name).value));
  }

  static String sanitizePythonName(String v1) {
    return hydra.Formatting.sanitizeWithUnderscores(
      hydra.python.Language.pythonReservedWords(),
      v1);
  }

  static hydra.python.syntax.Expression termVariableReference(hydra.python.environment.PythonEnvironment v1, hydra.core.Name v2) {
    return hydra.python.Names.variableReference(
      new hydra.util.CaseConvention.LowerSnake(),
      false,
      v1,
      v2);
  }

  static hydra.python.syntax.Expression typeVariableReference(hydra.python.environment.PythonEnvironment v1, hydra.core.Name v2) {
    return hydra.python.Names.variableReference(
      new hydra.util.CaseConvention.Pascal(),
      false,
      v1,
      v2);
  }

  static Boolean useFutureAnnotations() {
    return true;
  }

  static hydra.python.syntax.Expression variableReference(hydra.util.CaseConvention conv, Boolean quoted, hydra.python.environment.PythonEnvironment env, hydra.core.Name name) {
    hydra.packaging.Namespaces<hydra.python.syntax.DottedName> namespaces = (env).namespaces;
    hydra.util.Lazy<hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>> focusPair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.packaging.Namespaces<hydra.python.syntax.DottedName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<hydra.packaging.Namespace> focusNs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(focusPair.get()));
    hydra.util.Maybe<hydra.packaging.Namespace> mns = hydra.Names.namespaceOf(name);
    hydra.python.syntax.Name pyName = hydra.python.Names.encodeName(
      true,
      conv,
      env,
      name);
    hydra.util.Lazy<Boolean> sameNamespace = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> false,
      (java.util.function.Function<hydra.packaging.Namespace, Boolean>) (ns -> hydra.lib.equality.Equal.apply(
        ns,
        focusNs.get())),
      mns));
    hydra.util.Lazy<hydra.python.syntax.Expression> unquoted = new hydra.util.Lazy<>(() -> new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(pyName))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        quoted,
        sameNamespace.get()),
      () -> new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.String_(new hydra.python.syntax.String_((pyName).value, new hydra.python.syntax.QuoteStyle.Double_())))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))),
      () -> unquoted.get());
  }

  static hydra.python.syntax.Name variantName(Boolean isQualified, hydra.python.environment.PythonEnvironment env, hydra.core.Name tname, hydra.core.Name fname) {
    return hydra.python.Names.encodeName(
      isQualified,
      new hydra.util.CaseConvention.Pascal(),
      env,
      new hydra.core.Name(hydra.lib.strings.Cat2.apply(
        (tname).value,
        hydra.Formatting.capitalize((fname).value))));
  }
}
