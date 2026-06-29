// Fixture data for the neo4j-validation demo.
//
// Seeds a small movie-domain graph against which the Hydra Neo4j validator is
// run. The graph type the demo validates against (defined in code) is roughly:
//
//   (:Person  { name :: STRING NOT NULL, born :: INTEGER })
//   (:Movie   { title :: STRING NOT NULL, released :: INTEGER NOT NULL })
//   (:Person)-[:ACTED_IN { roles :: LIST<STRING> }]->(:Movie)
//   (:Person)-[:LIKES]->(:Movie)
//   (:Person)-[:LIKES]->(:Person)
//
// The data below deliberately includes several violations so the demo prints a
// non-empty report:
//   - a Person with no `name` (property existence violation)
//   - a Movie whose `released` is a string, not an integer (property type violation)
//   - a Movie with no `released` (property existence violation)
//   - a LIKES from a Movie to a Person (no matching endpoint pattern)

MATCH (n) DETACH DELETE n;

CREATE
  (keanu:Person   {name: 'Keanu Reeves', born: 1964}),
  (carrie:Person  {name: 'Carrie-Anne Moss', born: 1967}),
  (nameless:Person {born: 1970}),                              // VIOLATION: missing name
  (matrix:Movie   {title: 'The Matrix', released: 1999}),
  (reloaded:Movie {title: 'The Matrix Reloaded', released: '2003'}), // VIOLATION: released is a string
  (untitled:Movie {title: 'Untitled'}),                        // VIOLATION: missing released

  (keanu)-[:ACTED_IN {roles: ['Neo']}]->(matrix),
  (carrie)-[:ACTED_IN {roles: ['Trinity']}]->(matrix),
  (keanu)-[:LIKES]->(matrix),
  (keanu)-[:LIKES]->(carrie),
  (matrix)-[:LIKES]->(keanu);                                  // VIOLATION: Movie-[:LIKES]->Person has no declared pattern
