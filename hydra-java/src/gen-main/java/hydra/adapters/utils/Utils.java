package hydra.adapters.utils;

/**
 * Utilities for use in transformations
 */
public interface Utils {
  static String describeFloatType(hydra.core.FloatType t) {
    return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
      hydra.adapters.utils.Utils.describePrecision(hydra.basics.Basics.floatTypePrecision((t))),
      " floating-point numbers"));
  }

  static String describeIntegerType(hydra.core.IntegerType t) {
    return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
      hydra.adapters.utils.Utils.describePrecision(hydra.basics.Basics.integerTypePrecision((t))),
      " integers"));
  }

  static String describeLiteralType(hydra.core.LiteralType v1) {
    return ((v1)).accept(new hydra.core.LiteralType.Visitor<String>() {
      @Override
      public String visit(hydra.core.LiteralType.Binary instance) {
        return "binary strings";
      }

      @Override
      public String visit(hydra.core.LiteralType.Boolean_ instance) {
        return "boolean values";
      }

      @Override
      public String visit(hydra.core.LiteralType.Float_ instance) {
        return hydra.adapters.utils.Utils.describeFloatType((instance.value));
      }

      @Override
      public String visit(hydra.core.LiteralType.Integer_ instance) {
        return hydra.adapters.utils.Utils.describeIntegerType((instance.value));
      }

      @Override
      public String visit(hydra.core.LiteralType.String_ instance) {
        return "character strings";
      }
    });
  }

  static String describePrecision(hydra.mantle.Precision v1) {
    return ((v1)).accept(new hydra.mantle.Precision.Visitor<String>() {
      @Override
      public String visit(hydra.mantle.Precision.Arbitrary instance) {
        return "arbitrary-precision";
      }

      @Override
      public String visit(hydra.mantle.Precision.Bits instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          hydra.lib.literals.Literals.showInt32((instance.value)),
          "-bit"));
      }
    });
  }

  static <M> String describeType(hydra.core.Type<M> typ) {
    return ((typ)).accept(new hydra.core.Type.Visitor<String, M>() {
      @Override
      public String visit(hydra.core.Type.Annotated<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          "annotated ",
          hydra.adapters.utils.Utils.describeType(((instance.value)).subject)));
      }

      @Override
      public String visit(hydra.core.Type.Application<M> instance) {
        return "instances of an application type";
      }

      @Override
      public String visit(hydra.core.Type.Literal<M> instance) {
        return hydra.adapters.utils.Utils.describeLiteralType((instance.value));
      }

      @Override
      public String visit(hydra.core.Type.Element<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          "elements containing ",
          hydra.adapters.utils.Utils.describeType((instance.value))));
      }

      @Override
      public String visit(hydra.core.Type.Function<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
            hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
              "functions from ",
              hydra.adapters.utils.Utils.describeType(((instance.value)).domain))),
            " to ")),
          hydra.adapters.utils.Utils.describeType(((instance.value)).codomain)));
      }

      @Override
      public String visit(hydra.core.Type.Lambda<M> instance) {
        return "polymorphic terms";
      }

      @Override
      public String visit(hydra.core.Type.List<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          "lists of ",
          hydra.adapters.utils.Utils.describeType((instance.value))));
      }

      @Override
      public String visit(hydra.core.Type.Map<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
            hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
              "maps from ",
              hydra.adapters.utils.Utils.describeType(((instance.value)).keys))),
            " to ")),
          hydra.adapters.utils.Utils.describeType(((instance.value)).values)));
      }

      @Override
      public String visit(hydra.core.Type.Nominal<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          "alias for ",
          ((instance.value)).value));
      }

      @Override
      public String visit(hydra.core.Type.Optional<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          "optional ",
          hydra.adapters.utils.Utils.describeType((instance.value))));
      }

      @Override
      public String visit(hydra.core.Type.Product<M> instance) {
        return "tuples";
      }

      @Override
      public String visit(hydra.core.Type.Record<M> instance) {
        return "records";
      }

      @Override
      public String visit(hydra.core.Type.Set<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          "sets of ",
          hydra.adapters.utils.Utils.describeType((instance.value))));
      }

      @Override
      public String visit(hydra.core.Type.Stream<M> instance) {
        return hydra.lib.strings.Strings.cat(java.util.Arrays.asList(
          "streams of ",
          hydra.adapters.utils.Utils.describeType((instance.value))));
      }

      @Override
      public String visit(hydra.core.Type.Sum<M> instance) {
        return "variant tuples";
      }

      @Override
      public String visit(hydra.core.Type.Union<M> instance) {
        return "unions";
      }

      @Override
      public String visit(hydra.core.Type.Variable<M> instance) {
        return "unspecified/parametric terms";
      }
    });
  }
}