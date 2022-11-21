# Hydra

Hydra is a transformation toolkit along the lines of [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale),
but open source, and with a more advanced type system and other new features.
It is currently in an intermediate "closing the loop" stage.
The main superpower of Hydra is that it is able to map schemas and data consistently between languages
in a way which maintains type conformance.
It is even able to map functional programs between selected languages, including parts of its own source code.
See the recent Data Day Texas presentation, "[Transpilers Gone Wild](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra)".

See [hydra-haskell](https://github.com/CategoricalData/hydra/hydra-haskell) for more information the Haskell implementation,
which is the most mature at this time.

You can find a design document [here](https://bit.ly/hydra-design-doc),
and a Slack channel [here](https://bit.ly/hydra-slack)
(click [here](https://join.slack.com/t/graphcommunity/shared_invite/zt-1a6ohrnn9-rXIBwn3L4NSC4cH0c1DN8A) for an invite to the Graph Community workspace, or send an email to josh at fortytwo net if the link has expired).

## Project structure

This repository currently contains a [hydra-haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell),
a [hydra-java](https://github.com/CategoricalData/hydra/tree/main/hydra-java),
and a [hydra-scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala) directory with language-specific builds,
as well as some common resources in the root directory.

Additional Hydra "coders" (type-aware encoders/decoders) at this time support
[Avro](https://avro.apache.org),
[JSON](https://json.org),
[YAML](https://en.wikipedia.org/wiki/YAML),
[RDF](https://www.w3.org/RDF) + [SHACL](https://www.w3.org/TR/shacl),
and LinkedIn's [PDL Schema](https://linkedin.github.io/rest.li/pdl_schema) language.
