// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.names;

/**
 * Python naming utilities: encoding Hydra names as Python names
 */
public interface Names {
  static Boolean useFutureAnnotations() {
    return true;
  }
  
  static <T0> hydra.ext.python.syntax.Name encodeConstantForFieldName(T0 env, hydra.core.Name tname, hydra.core.Name fname) {
    return new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat.apply(java.util.List.of(
      hydra.formatting.Formatting.convertCase(
        new hydra.util.CaseConvention.Pascal(),
        new hydra.util.CaseConvention.UpperSnake(),
        hydra.names.Names.localNameOf(tname)),
      "__",
      hydra.formatting.Formatting.convertCase(
        new hydra.util.CaseConvention.Camel(),
        new hydra.util.CaseConvention.UpperSnake(),
        (fname).value),
      "__NAME")));
  }
  
  static <T0> hydra.ext.python.syntax.Name encodeConstantForTypeName(T0 env, hydra.core.Name tname) {
    return new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
      hydra.formatting.Formatting.convertCase(
        new hydra.util.CaseConvention.Pascal(),
        new hydra.util.CaseConvention.UpperSnake(),
        hydra.names.Names.localNameOf(tname)),
      "__NAME"));
  }
  
  static hydra.ext.python.syntax.Name encodeEnumValue(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.Name v2) {
    return hydra.ext.python.names.Names.encodeName(
      false,
      new hydra.util.CaseConvention.UpperSnake(),
      v1,
      v2);
  }
  
  static hydra.ext.python.syntax.Name encodeFieldName(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name fname) {
    return hydra.ext.python.names.Names.encodeName(
      false,
      new hydra.util.CaseConvention.LowerSnake(),
      env,
      fname);
  }
  
  static hydra.ext.python.syntax.Name encodeName(Boolean isQualified, hydra.util.CaseConvention conv, hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((env).boundTypeVariables));
    hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces = (env).namespaces;
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>> focusPair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<hydra.module.Namespace> focusNs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(focusPair.get()));
    hydra.module.QualifiedName qualName = hydra.names.Names.qualifyName(name);
    String local = (qualName).local;
    hydra.util.Maybe<hydra.module.Namespace> mns = (qualName).namespace;
    String pyLocal = hydra.ext.python.names.Names.sanitizePythonName(hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(),
      conv,
      local));
    java.util.function.Function<hydra.module.Namespace, String> pyNs = (java.util.function.Function<hydra.module.Namespace, String>) (nsVal -> hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, String>) (v1 -> hydra.formatting.Formatting.convertCase(
          new hydra.util.CaseConvention.Camel(),
          new hydra.util.CaseConvention.LowerSnake(),
          v1)),
        hydra.lib.strings.SplitOn.apply(
          ".",
          (nsVal).value))));
    return hydra.lib.logic.IfElse.lazy(
      isQualified,
      () -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            mns,
            hydra.util.Maybe.just(focusNs.get())),
          () -> new hydra.ext.python.syntax.Name(hydra.lib.logic.IfElse.lazy(
            hydra.ext.python.names.Names.useFutureAnnotations(),
            () -> pyLocal,
            () -> hydra.ext.python.serde.Serde.escapePythonString(
              true,
              pyLocal))),
          () -> hydra.lib.maybes.Maybe.apply(
            new hydra.ext.python.syntax.Name(pyLocal),
            (java.util.function.Function<hydra.module.Namespace, hydra.ext.python.syntax.Name>) (nsVal -> new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
              (pyNs).apply(nsVal),
              hydra.lib.strings.Cat2.apply(
                ".",
                pyLocal)))),
            mns)),
        (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Name>) (n -> n),
        hydra.lib.maps.Lookup.apply(
          name,
          boundVars.get())),
      () -> new hydra.ext.python.syntax.Name(pyLocal));
  }
  
  static hydra.ext.python.syntax.Name encodeNameQualified(hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((env).boundTypeVariables));
    hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces = (env).namespaces;
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>> focusPair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<hydra.module.Namespace> focusNs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(focusPair.get()));
    hydra.module.QualifiedName qualName = hydra.names.Names.qualifyName(name);
    String local = (qualName).local;
    hydra.util.Maybe<hydra.module.Namespace> mns = (qualName).namespace;
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          mns,
          hydra.util.Maybe.just(focusNs.get())),
        () -> new hydra.ext.python.syntax.Name(hydra.lib.logic.IfElse.lazy(
          hydra.ext.python.names.Names.useFutureAnnotations(),
          () -> local,
          () -> hydra.ext.python.serde.Serde.escapePythonString(
            true,
            local))),
        () -> new hydra.ext.python.syntax.Name(hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Map.apply(
            hydra.ext.python.names.Names::sanitizePythonName,
            hydra.lib.strings.SplitOn.apply(
              ".",
              (name).value))))),
      (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Name>) (n -> n),
      hydra.lib.maps.Lookup.apply(
        name,
        boundVars.get()));
  }
  
  static hydra.ext.python.syntax.DottedName encodeNamespace(hydra.module.Namespace nsVal) {
    return new hydra.ext.python.syntax.DottedName(hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.ext.python.syntax.Name>) (part -> new hydra.ext.python.syntax.Name(hydra.formatting.Formatting.convertCase(
        new hydra.util.CaseConvention.Camel(),
        new hydra.util.CaseConvention.LowerSnake(),
        part))),
      hydra.lib.strings.SplitOn.apply(
        ".",
        (nsVal).value)));
  }
  
  static hydra.ext.python.syntax.Name encodeTypeVariable(hydra.core.Name name) {
    return new hydra.ext.python.syntax.Name(hydra.formatting.Formatting.capitalize((name).value));
  }
  
  static String sanitizePythonName(String v1) {
    return hydra.formatting.Formatting.sanitizeWithUnderscores(
      hydra.ext.python.language.Language.pythonReservedWords(),
      v1);
  }
  
  static hydra.ext.python.syntax.Expression termVariableReference(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.Name v2) {
    return hydra.ext.python.names.Names.variableReference(
      new hydra.util.CaseConvention.LowerSnake(),
      false,
      v1,
      v2);
  }
  
  static hydra.ext.python.syntax.Expression typeVariableReference(hydra.ext.python.helpers.PythonEnvironment v1, hydra.core.Name v2) {
    return hydra.ext.python.names.Names.variableReference(
      new hydra.util.CaseConvention.Pascal(),
      false,
      v1,
      v2);
  }
  
  static hydra.ext.python.syntax.Name variantName(Boolean isQualified, hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name tname, hydra.core.Name fname) {
    return hydra.ext.python.names.Names.encodeName(
      isQualified,
      new hydra.util.CaseConvention.Pascal(),
      env,
      new hydra.core.Name(hydra.lib.strings.Cat2.apply(
        (tname).value,
        hydra.formatting.Formatting.capitalize((fname).value))));
  }
  
  static hydra.ext.python.syntax.Expression variableReference(hydra.util.CaseConvention conv, Boolean quoted, hydra.ext.python.helpers.PythonEnvironment env, hydra.core.Name name) {
    hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces = (env).namespaces;
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>> focusPair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<hydra.module.Namespace> focusNs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(focusPair.get()));
    hydra.util.Maybe<hydra.module.Namespace> mns = hydra.names.Names.namespaceOf(name);
    hydra.ext.python.syntax.Name pyName = hydra.ext.python.names.Names.encodeName(
      true,
      conv,
      env,
      name);
    hydra.util.Lazy<Boolean> sameNamespace = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
      false,
      (java.util.function.Function<hydra.module.Namespace, Boolean>) (ns -> hydra.lib.equality.Equal.apply(
        ns,
        focusNs.get())),
      mns));
    hydra.util.Lazy<hydra.ext.python.syntax.Expression> unquoted = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(pyName))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        quoted,
        sameNamespace.get()),
      () -> new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.String_(new hydra.ext.python.syntax.String_((pyName).value, new hydra.ext.python.syntax.QuoteStyle.Double_())))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))),
      () -> unquoted.get());
  }
}
