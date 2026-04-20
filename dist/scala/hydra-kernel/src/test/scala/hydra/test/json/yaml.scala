package hydra.test.json.yaml

import hydra.json.model.*

import hydra.testing.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("JSON<->YAML bridge",
   None, Seq(hydra.testing.TestGroup("decimal round-trip", None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("zero",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.number(BigDecimal("0.0"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.number(BigDecimal("0.0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("positive whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.number(BigDecimal("42.0"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.number(BigDecimal("42.0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative whole", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.number(BigDecimal("-17.0"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.number(BigDecimal("-17.0"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("fraction", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.number(BigDecimal("3.14"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.number(BigDecimal("3.14"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("negative fraction", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.number(BigDecimal("-2.5"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.number(BigDecimal("-2.5"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("tiny exponent", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.number(BigDecimal("1.0e-20"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.number(BigDecimal("1.0e-20"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("huge exponent", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.number(BigDecimal("1.0e20"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.number(BigDecimal("1.0e20"))))),
   None, Seq()), hydra.testing.TestCaseWithMetadata("null", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.`null`))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.`null`))), None, Seq()), hydra.testing.TestCaseWithMetadata("true",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.boolean(true)))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.boolean(true)))), None, Seq()),
   hydra.testing.TestCaseWithMetadata("false", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.boolean(false)))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.boolean(false)))), None, Seq()),
   hydra.testing.TestCaseWithMetadata("simple string", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.string("hello")))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.writer.printJson(hydra.json.model.Value.string("hello")))), None, Seq()),
   hydra.testing.TestCaseWithMetadata("mixed array", hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.lib.eithers.either[scala.Predef.String,
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value, scala.Predef.String]((e: scala.Predef.String) => e)((back: hydra.json.model.Value) => hydra.json.writer.printJson(back))(hydra.json.yaml.decode.yamlToJson(hydra.json.yaml.encode.jsonToYaml(hydra.json.model.Value.array(Seq(hydra.json.model.Value.number(BigDecimal("1.0")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.json.model.Value.number(BigDecimal("1.0e-20")), hydra.json.model.Value.string("note")))))),
   hydra.json.writer.printJson(hydra.json.model.Value.array(Seq(hydra.json.model.Value.number(BigDecimal("1.0")),
   hydra.json.model.Value.number(BigDecimal("1.0e-20")), hydra.json.model.Value.string("note")))))),
   None, Seq())))), Seq())
