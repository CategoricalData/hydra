# Hydra

Hydra is a transformation toolkit along the lines of [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale),
but open source, and with a more advanced type system and other new features.
Hydra maps data and schemas between languages in a way which maintains type conformance.
It will even map functional programs between selected languages, including parts of its own source code.

For more information, see [hydra-haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell)
as well as the Data Day Texas presentation, "[Transpilers Gone Wild](https://www.slideshare.net/joshsh/transpilers-gone-wild-introducing-hydra)".
You can find an early design document [here](https://bit.ly/hydra-design-doc)
and a Slack channel [here](https://bit.ly/hydra-slack)
(click [here](https://join.slack.com/t/graphcommunity/shared_invite/zt-1a6ohrnn9-rXIBwn3L4NSC4cH0c1DN8A) for an invite to the Graph Community workspace, or send an email to josh at fortytwo net if the link has expired).

This repository currently contains a [hydra-haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell),
a [hydra-java](https://github.com/CategoricalData/hydra/tree/main/hydra-java),
and an experimental [hydra-scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala) directory with language-specific builds,
as well as generated [docs](https://github.com/CategoricalData/hydra/tree/main/docs) and the [hydra-extensions](https://github.com/CategoricalData/hydra/tree/main/hydra-extensions) Haskell project.
The extensions are a collection of less-essential, and less thoroughly documented, Hydra models and tools, which nonetheless can be useful for building applications.
For example, there is a GeoJson model, a Coq model, and an AvroToRdf tool which has been used at LinkedIn for ingestion of Avro-formatted data into RDF triple stores.

In addition to Haskell, Java, and Scala, there are Hydra coders (type-aware encoders/decoders) for
[Avro](https://avro.apache.org),
[JSON](https://json.org),
[YAML](https://en.wikipedia.org/wiki/YAML),
[RDF](https://www.w3.org/RDF) + [SHACL](https://www.w3.org/TR/shacl),
and LinkedIn's [PDL Schema](https://linkedin.github.io/rest.li/pdl_schema) language.
