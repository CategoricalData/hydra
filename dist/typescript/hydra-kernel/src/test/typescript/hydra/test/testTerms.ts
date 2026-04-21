// Note: this is an automatically generated file. Do not edit.

/**
 * Term definitions for the test suite
 */



import * as Core from "../core.js";
import * as TestTestTypes from "./testTypes.js";

export function latlonRecord(lat: number): ((x: number) => Core.Term) {
  return ((lon: number) => ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypeLatLonName,
    fields: [({
    name: "lat",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: lat }) }) })
  }), ({
    name: "lon",
    term: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32", value: lon }) }) })
  })]
  }) }));
}

export const testDataArthur: Core.Term = ({ tag: "record", value: ({
    typeName: TestTestTypes.testTypePersonName,
    fields: [({
    name: "firstName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Arthur" }) })
  }), ({
    name: "lastName",
    term: ({ tag: "literal", value: ({ tag: "string", value: "Dent" }) })
  }), ({
    name: "age",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 42 }) }) })
  })]
  }) });

export const testElementArthur: Core.Binding = ({
    name: "firstName",
    term: testDataArthur,
    type: ({
    variables: [],
    type: ({ tag: "variable", value: TestTestTypes.testTypePersonName }),
    constraints: null
  })
  });

export const testElementFirstName: Core.Binding = ({
    name: "firstName",
    term: ({ tag: "project", value: ({
    typeName: TestTestTypes.testTypePersonName,
    field: "firstName"
  }) }),
    type: ({
    variables: [],
    type: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: TestTestTypes.testTypePersonName }),
    codomain: ({ tag: "literal", value: ({ tag: "string" }) })
  }) }),
    constraints: null
  })
  });
