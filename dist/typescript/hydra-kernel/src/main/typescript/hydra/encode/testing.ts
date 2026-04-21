// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.testing
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaybes from "../lib/maybes.js";
import * as Testing from "../testing.js";

export function tag(x: Testing.Tag): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.testing.Tag",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function testCase(v1: Testing.TestCase): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "universal": return ((y: Testing.UniversalTestCase) => ({ tag: "inject", value: ({
    typeName: "hydra.testing.TestCase",
    field: ({
    name: "universal",
    term: universalTestCase(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function testCaseWithMetadata(x: Testing.TestCaseWithMetadata): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.testing.TestCaseWithMetadata",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.name)(x) }) })
  }), ({
    name: "case",
    term: testCase(((_x) => _x.case)(x))
  }), ({
    name: "description",
    term: ({ tag: "maybe", value: LibMaybes.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x.description)(x)) })
  }), ({
    name: "tags",
    term: ({ tag: "list", value: LibLists.map(tag)(((_x) => _x.tags)(x)) })
  })]
  }) });
}

export function testGroup(x: Testing.TestGroup): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.testing.TestGroup",
    fields: [({
    name: "name",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.name)(x) }) })
  }), ({
    name: "description",
    term: ({ tag: "maybe", value: LibMaybes.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x.description)(x)) })
  }), ({
    name: "subgroups",
    term: ({ tag: "list", value: LibLists.map(testGroup)(((_x) => _x.subgroups)(x)) })
  }), ({
    name: "cases",
    term: ({ tag: "list", value: LibLists.map(testCaseWithMetadata)(((_x) => _x.cases)(x)) })
  })]
  }) });
}

export function universalTestCase(x: Testing.UniversalTestCase): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.testing.UniversalTestCase",
    fields: [({
    name: "actual",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.actual)(x) }) })
  }), ({
    name: "expected",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.expected)(x) }) })
  })]
  }) });
}
