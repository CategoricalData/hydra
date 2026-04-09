// Hydra kernel modules
// Generated from Hydra DSL sources via hydra-ext

// Hand-written primitive implementations
#[path = "lib"]
pub mod lib {
    pub mod lists;
    pub mod strings;
    pub mod logic;
    pub mod chars;
    pub mod maps;
    pub mod sets;
    pub mod math;
    pub mod eithers;
    pub mod maybes;
    pub mod pairs;
    pub mod literals;
    pub mod equality;
    pub mod flows;
    #[path = "../../gen-main/rust/hydra/lib/names.rs"]
    pub mod names;
}

#[path = "../../src/gen-main/rust/hydra/core.rs"]
pub mod core;

#[path = "../../src/gen-main/rust/hydra/grammar.rs"]
pub mod grammar;

#[path = "../../src/gen-main/rust/hydra/ast.rs"]
pub mod ast;

#[path = "../../src/gen-main/rust/hydra/context.rs"]
pub mod context;

#[path = "../../src/gen-main/rust/hydra/error.rs"]
pub mod error;

#[path = "../../src/gen-main/rust/hydra/graph.rs"]
pub mod graph;

#[path = "../../src/gen-main/rust/hydra/module.rs"]
pub mod module;

#[path = "../../src/gen-main/rust/hydra/coders.rs"]
pub mod coders;

#[path = "../../src/gen-main/rust/hydra/query.rs"]
pub mod query;

#[path = "../../src/gen-main/rust/hydra/classes.rs"]
pub mod classes;

#[path = "../../src/gen-main/rust/hydra/literals.rs"]
pub mod literals;

#[path = "../../src/gen-main/rust/hydra/testing.rs"]
pub mod testing;

#[path = "../../src/gen-main/rust/hydra/util.rs"]
pub mod util;

#[path = "../../src/gen-main/rust/hydra/variants.rs"]
pub mod variants;

#[path = "../../src/gen-main/rust/hydra/typing.rs"]
pub mod typing;

#[path = "../../src/gen-main/rust/hydra/phantoms.rs"]
pub mod phantoms;

#[path = "../../src/gen-main/rust/hydra/topology.rs"]
pub mod topology;

#[path = "../../src/gen-main/rust/hydra/relational.rs"]
pub mod relational;

#[path = "../../src/gen-main/rust/hydra/tabular.rs"]
pub mod tabular;

#[path = "../../src/gen-main/rust/hydra/parsing.rs"]
pub mod parsing;

#[path = "../../src/gen-main/rust/hydra/accessors.rs"]
pub mod accessors;

#[path = "../../src/gen-main/rust/hydra/constants.rs"]
pub mod constants;

#[path = "../../src/gen-main/rust/hydra/formatting.rs"]
pub mod formatting;

#[path = "../../src/gen-main/rust/hydra/names.rs"]
pub mod names;

#[path = "../../src/gen-main/rust/hydra/lexical.rs"]
pub mod lexical;

#[path = "../../src/gen-main/rust/hydra/adapt.rs"]
pub mod adapt;

#[path = "../../src/gen-main/rust/hydra/annotations.rs"]
pub mod annotations;

#[path = "../../src/gen-main/rust/hydra/arity.rs"]
pub mod arity;

#[path = "../../src/gen-main/rust/hydra/checking.rs"]
pub mod checking;

#[path = "../../src/gen-main/rust/hydra/code_generation.rs"]
pub mod code_generation;

#[path = "../../src/gen-main/rust/hydra/decoding.rs"]
pub mod decoding;

#[path = "../../src/gen-main/rust/hydra/encoding.rs"]
pub mod encoding;

#[path = "../../src/gen-main/rust/hydra/grammars.rs"]
pub mod grammars;

#[path = "../../src/gen-main/rust/hydra/hoisting.rs"]
pub mod hoisting;

#[path = "../../src/gen-main/rust/hydra/inference.rs"]
pub mod inference;

#[path = "../../src/gen-main/rust/hydra/languages.rs"]
pub mod languages;

#[path = "../../src/gen-main/rust/hydra/parsers.rs"]
pub mod parsers;

#[path = "../../src/gen-main/rust/hydra/reduction.rs"]
pub mod reduction;

#[path = "../../src/gen-main/rust/hydra/reflect.rs"]
pub mod reflect;

#[path = "../../src/gen-main/rust/hydra/rewriting.rs"]
pub mod rewriting;

#[path = "../../src/gen-main/rust/hydra/schemas.rs"]
pub mod schemas;

#[path = "../../src/gen-main/rust/hydra/serialization.rs"]
pub mod serialization;

#[path = "../../src/gen-main/rust/hydra/sorting.rs"]
pub mod sorting;

#[path = "../../src/gen-main/rust/hydra/substitution.rs"]
pub mod substitution;

#[path = "../../src/gen-main/rust/hydra/tarjan.rs"]
pub mod tarjan;

#[path = "../../src/gen-main/rust/hydra/templates.rs"]
pub mod templates;

#[path = "../../src/gen-main/rust/hydra/unification.rs"]
pub mod unification;

// Nested sub-modules
pub mod json {
    #[path = "../../gen-main/rust/hydra/json/model.rs"]
    pub mod model;
    #[path = "../../gen-main/rust/hydra/json/decode.rs"]
    pub mod decode;
    #[path = "../../gen-main/rust/hydra/json/encode.rs"]
    pub mod encode;
    #[path = "../../gen-main/rust/hydra/json/parser.rs"]
    pub mod parser;
    #[path = "../../gen-main/rust/hydra/json/writer.rs"]
    pub mod writer;
    #[path = "../../gen-main/rust/hydra/json/bootstrap.rs"]
    pub mod bootstrap;
}

pub mod show {
    #[path = "../../gen-main/rust/hydra/show/core.rs"]
    pub mod core;
    #[path = "../../gen-main/rust/hydra/show/error.rs"]
    pub mod error;
    #[path = "../../gen-main/rust/hydra/show/graph.rs"]
    pub mod graph;
    #[path = "../../gen-main/rust/hydra/show/meta.rs"]
    pub mod meta;
    #[path = "../../gen-main/rust/hydra/show/typing.rs"]
    pub mod typing;
    #[path = "../../gen-main/rust/hydra/show/util.rs"]
    pub mod util;
    #[path = "../../gen-main/rust/hydra/show/accessors.rs"]
    pub mod accessors;
}

pub mod decode {
    #[path = "../../gen-main/rust/hydra/decode/core.rs"]
    pub mod core;
    #[path = "../../gen-main/rust/hydra/decode/accessors.rs"]
    pub mod accessors;
    #[path = "../../gen-main/rust/hydra/decode/ast.rs"]
    pub mod ast;
    #[path = "../../gen-main/rust/hydra/decode/classes.rs"]
    pub mod classes;
    #[path = "../../gen-main/rust/hydra/decode/coders.rs"]
    pub mod coders;
    #[path = "../../gen-main/rust/hydra/decode/context.rs"]
    pub mod context;
    #[path = "../../gen-main/rust/hydra/decode/grammar.rs"]
    pub mod grammar;
    #[path = "../../gen-main/rust/hydra/decode/module.rs"]
    pub mod module;
    #[path = "../../gen-main/rust/hydra/decode/parsing.rs"]
    pub mod parsing;
    #[path = "../../gen-main/rust/hydra/decode/phantoms.rs"]
    pub mod phantoms;
    #[path = "../../gen-main/rust/hydra/decode/query.rs"]
    pub mod query;
    #[path = "../../gen-main/rust/hydra/decode/relational.rs"]
    pub mod relational;
    #[path = "../../gen-main/rust/hydra/decode/tabular.rs"]
    pub mod tabular;
    #[path = "../../gen-main/rust/hydra/decode/testing.rs"]
    pub mod testing;
    #[path = "../../gen-main/rust/hydra/decode/topology.rs"]
    pub mod topology;
    #[path = "../../gen-main/rust/hydra/decode/typing.rs"]
    pub mod typing;
    #[path = "../../gen-main/rust/hydra/decode/util.rs"]
    pub mod util;
    #[path = "../../gen-main/rust/hydra/decode/variants.rs"]
    pub mod variants;
    pub mod json {
        #[path = "../../../gen-main/rust/hydra/decode/json/model.rs"]
        pub mod model;
    }
}

pub mod encode {
    #[path = "../../gen-main/rust/hydra/encode/core.rs"]
    pub mod core;
    #[path = "../../gen-main/rust/hydra/encode/accessors.rs"]
    pub mod accessors;
    #[path = "../../gen-main/rust/hydra/encode/ast.rs"]
    pub mod ast;
    #[path = "../../gen-main/rust/hydra/encode/classes.rs"]
    pub mod classes;
    #[path = "../../gen-main/rust/hydra/encode/coders.rs"]
    pub mod coders;
    #[path = "../../gen-main/rust/hydra/encode/context.rs"]
    pub mod context;
    #[path = "../../gen-main/rust/hydra/encode/grammar.rs"]
    pub mod grammar;
    #[path = "../../gen-main/rust/hydra/encode/module.rs"]
    pub mod module;
    #[path = "../../gen-main/rust/hydra/encode/parsing.rs"]
    pub mod parsing;
    #[path = "../../gen-main/rust/hydra/encode/phantoms.rs"]
    pub mod phantoms;
    #[path = "../../gen-main/rust/hydra/encode/query.rs"]
    pub mod query;
    #[path = "../../gen-main/rust/hydra/encode/relational.rs"]
    pub mod relational;
    #[path = "../../gen-main/rust/hydra/encode/tabular.rs"]
    pub mod tabular;
    #[path = "../../gen-main/rust/hydra/encode/testing.rs"]
    pub mod testing;
    #[path = "../../gen-main/rust/hydra/encode/topology.rs"]
    pub mod topology;
    #[path = "../../gen-main/rust/hydra/encode/typing.rs"]
    pub mod typing;
    #[path = "../../gen-main/rust/hydra/encode/util.rs"]
    pub mod util;
    #[path = "../../gen-main/rust/hydra/encode/variants.rs"]
    pub mod variants;
    pub mod json {
        #[path = "../../../gen-main/rust/hydra/encode/json/model.rs"]
        pub mod model;
    }
}

pub mod extract {
    #[path = "../../gen-main/rust/hydra/extract/core.rs"]
    pub mod core;
    #[path = "../../gen-main/rust/hydra/extract/helpers.rs"]
    pub mod helpers;
    #[path = "../../gen-main/rust/hydra/extract/json.rs"]
    pub mod json;
    #[path = "../../gen-main/rust/hydra/extract/util.rs"]
    pub mod util;
}

pub mod ext {
    pub mod org {
        pub mod json {
            #[path = "../../../../gen-main/rust/hydra/ext/org/json/decoding.rs"]
            pub mod decoding;
        }
    }
}
