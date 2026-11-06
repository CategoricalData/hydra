{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.CppCoderSpec.spec
-}

module Hydra.CppCoderSpec where

import Hydra.Kernel
import qualified Hydra.Ext.Cpp.Coder as Coder

import Hydra.ExtCoderTestSupport
import Hydra.TestUtils

import qualified Data.Map as M
import qualified Test.Hspec as H


-- | Golden-output tests for the Cpp generation coder (#816), rendered through the real
-- `moduleToCpp` entry point (encode definitions -> Serde -> printExpr).
-- NOTE: output is pinned as-is. It uses std::string / std::optional without including <string> / <optional>;
-- see the #816 plan findings.
spec :: H.Spec
spec = H.describe "Cpp.Coder.moduleToCpp golden output (#816)" $ do

  H.it "generates a record type" $ do
    generated [personRecord] `H.shouldBe` M.fromList [("example/shapes/person.h", expected [
      "#pragma once",
      "",
      "#include <memory>",
      "#include <stdexcept>",
      "#include \"example/shapes/fwd.h\"",
      "",
      "namespace example::shapes {",
      "  class Person {",
      "    public:",
      "",
      "    std::string name;",
      "",
      "    int age;",
      "",
      "    const std::optional<bool> active;",
      "",
      "    Person(std::string name, int age, const std::optional<bool> active) : name(name), age(age), active(active) {}",
      "  };",
      "",
      "  inline bool operator<(Person /*lhs*/, Person /*rhs*/) {",
      "    return false;",
      "  }",
      "}"])]

  H.it "generates a union type" $ do
    generated [shapeUnion] `H.shouldBe` M.fromList [("example/shapes/shape.h", expected [
      "#pragma once",
      "",
      "#include <memory>",
      "#include <stdexcept>",
      "#include \"example/shapes/fwd.h\"",
      "",
      "namespace example::shapes {",
      "  class ShapeLabel;",
      "",
      "  class ShapeSize;",
      "",
      "  template<typename R>",
      "  class ShapeVisitor {",
      "    public:",
      "",
      "    virtual R visit(const ShapeLabel& value) = 0;",
      "",
      "    virtual R visit(const ShapeSize& value) = 0;",
      "",
      "    virtual ~ShapeVisitor() = default;",
      "  };",
      "",
      "  class Shape {",
      "    protected:",
      "",
      "    Shape() = default;",
      "",
      "    public:",
      "",
      "    virtual ~Shape() = default;",
      "",
      "    template<typename R>",
      "    R accept(ShapeVisitor<R>& visitor) const ;",
      "  };",
      "",
      "  class ShapeLabel : public Shape {",
      "    public:",
      "",
      "    std::string value;",
      "",
      "    ShapeLabel(std::string value) : value(value) {}",
      "  };",
      "",
      "  class ShapeSize : public Shape {",
      "    public:",
      "",
      "    int value;",
      "",
      "    ShapeSize(int value) : value(value) {}",
      "  };",
      "",
      "  template<typename R>",
      "  class ShapePartialVisitor : public ShapeVisitor<R> {",
      "    public:",
      "",
      "    virtual R otherwise(const Shape& value) const = 0;",
      "",
      "    R visit(const ShapeLabel& value) override {",
      "      return otherwise(value);",
      "    }",
      "",
      "    R visit(const ShapeSize& value) override {",
      "      return otherwise(value);",
      "    }",
      "  };",
      "",
      "  template<typename R>",
      "  R Shape::accept(ShapeVisitor<R>& visitor) const {",
      "    if (auto ptr = dynamic_cast<const ShapeLabel*>(this))",
      "    {",
      "      return visitor.visit(* ptr);",
      "    }",
      "    else",
      "    throw std::runtime_error(typeid((*this)).name());",
      "",
      "    if (auto ptr = dynamic_cast<const ShapeSize*>(this))",
      "    {",
      "      return visitor.visit(* ptr);",
      "    }",
      "    else",
      "    throw std::runtime_error(typeid((*this)).name());",
      "  }",
      "}"])]

  where
    generated defs = case mapError (Coder.moduleToCpp (fixtureModule defs) defs testContext testGraph) of
      Left e -> error e
      Right m -> m
