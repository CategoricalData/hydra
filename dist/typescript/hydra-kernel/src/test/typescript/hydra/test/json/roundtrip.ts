// Note: this is an automatically generated file. Do not edit.

/**
 * Round-trip test cases for JSON encoding and decoding
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as Graph from "../../graph.js";
import * as JsonDecode from "../../json/decode.js";
import * as JsonEncode from "../../json/encode.js";
import * as JsonModel from "../../json/model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibMaps from "../../lib/maps.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "JSON round-trip",
    description: null,
    subgroups: [({
    name: "literal types",
    description: null,
    subgroups: [],
    cases: [({
    name: "boolean true",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "boolean" }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "boolean" }) }))(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "boolean false",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "boolean" }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "boolean" }) }))(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int8 positive",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 42 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: 42 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int8 negative",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -17 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8", value: -17 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int16",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 1000n }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16", value: 1000n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100000 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 100000 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "uint8",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 200n }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8", value: 200n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "uint16",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 50000 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16", value: 50000 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "int64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 1000000000000n }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64", value: 1000000000000n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "uint32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32" }) }) }))(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 4000000000n }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32", value: 4000000000n }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float32",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 1.5 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: 1.5 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float64",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159 }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: 3.14159 }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float32 NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: NaN }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float32 positive infinity",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: Infinity }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float32 negative infinity",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -Infinity }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float64 NaN",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: NaN }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float64 positive infinity",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "float64 negative infinity",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }))(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64", value: -Infinity }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string simple",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "string" }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "string" }) }))(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string empty",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "string" }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "string" }) }))(({ tag: "literal", value: ({ tag: "string", value: "" }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "" }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "string with spaces",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "string" }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "literal", value: ({ tag: "string" }) }))(({ tag: "literal", value: ({ tag: "string", value: "hello world" }) }))),
    expected: ShowCore.term(({ tag: "literal", value: ({ tag: "string", value: "hello world" }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "collection types",
    description: null,
    subgroups: [],
    cases: [({
    name: "list of integers",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "list of strings",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(({ tag: "list", value: [] }))),
    expected: ShowCore.term(({ tag: "list", value: [] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested list",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "list", value: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) }))(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))),
    expected: ShowCore.term(({ tag: "list", value: [({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 1 }) }) }), ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 2 }) }) })] }), ({ tag: "list", value: [({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 3 }) }) })] })] }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "set of strings",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })]) }))),
    expected: ShowCore.term(({ tag: "set", value: new Set([({ tag: "literal", value: ({ tag: "string", value: "a" }) }), ({ tag: "literal", value: ({ tag: "string", value: "b" }) })]) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "empty set",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "set", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(({ tag: "set", value: new Set([]) }))),
    expected: ShowCore.term(({ tag: "set", value: new Set([]) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "optional types",
    description: null,
    subgroups: [],
    cases: [({
    name: "optional string with value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "optional string nothing",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }))(({ tag: "maybe", value: null }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "optional int with value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }))(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested optional: nothing",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))(({ tag: "maybe", value: null }))),
    expected: ShowCore.term(({ tag: "maybe", value: null }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested optional: just nothing",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))(({ tag: "maybe", value: ({ tag: "maybe", value: null }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "maybe", value: null }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "nested optional: just just value",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }) }))(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }) }))),
    expected: ShowCore.term(({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "hello" }) }) }) }))
  }) }),
    description: null,
    tags: []
  })]
  }), ({
    name: "record types",
    description: null,
    subgroups: [],
    cases: [({
    name: "record with required fields",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "age",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  })] }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "age",
    type: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) })
  })] }))(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 30 }) }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "record with optional field present",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "email",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  })] }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "email",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  })] }))(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "email",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "alice@example.com" }) }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "email",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string", value: "alice@example.com" }) }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "record with optional field absent",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "email",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  })] }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "email",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  })] }))(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "email",
    term: ({ tag: "maybe", value: null })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Alice" }) })
  }), ({
    name: "email",
    term: ({ tag: "maybe", value: null })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "record with mixed optional fields",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "email",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }), ({
    name: "age",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  })] }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "email",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) })
  }), ({
    name: "age",
    type: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) })
  })] }))(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Bob" }) })
  }), ({
    name: "email",
    term: ({ tag: "maybe", value: null })
  }), ({
    name: "age",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Bob" }) })
  }), ({
    name: "email",
    term: ({ tag: "maybe", value: null })
  }), ({
    name: "age",
    term: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 25 }) }) }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  }), ({
    name: "record with nested optional field",
    case: ({ tag: "universal", value: ({
    actual: LibEithers.either(((e: string) => e))(((json: JsonModel.Value) => LibEithers.either(((e: string) => e))(((decoded: Core.Term) => ShowCore.term(decoded)))(JsonDecode.fromJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "value",
    type: ({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) })
  })] }))(json))))(JsonEncode.toJson(LibMaps.empty)("test")(({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "literal", value: ({ tag: "string" }) })
  }), ({
    name: "value",
    type: ({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }) }) })
  })] }))(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  }), ({
    name: "value",
    term: ({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })
  })]
  }) }))),
    expected: ShowCore.term(({ tag: "record", value: ({
    typeName: "test",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: "test" }) })
  }), ({
    name: "value",
    term: ({ tag: "maybe", value: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) }) }) })
  })]
  }) }))
  }) }),
    description: null,
    tags: []
  })]
  })],
    cases: []
  });
