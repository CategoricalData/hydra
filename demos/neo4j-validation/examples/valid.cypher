// A valid movie graph, written in plain Neo4j Cypher.
//
// This is exactly what you would type into cypher-shell to seed a graph. The demo
// reads this file, converts it to Hydra's Neo4j model, and validates it against the
// movie graph type in every host language — you never have to touch Hydra.
//
// Validated against:
//   (:Person { name :: STRING NOT NULL, born :: INTEGER })
//   (:Movie  { title :: STRING NOT NULL, released :: INTEGER NOT NULL })
//   (:Person)-[:ACTED_IN]->(:Movie)
//   (:Person)-[:LIKES]->(:Movie)   and   (:Person)-[:LIKES]->(:Person)

CREATE
  (keanu:Person   {name: 'Keanu Reeves', born: 1964}),
  (carrie:Person  {name: 'Carrie-Anne Moss', born: 1967}),
  (fan:Person     {name: 'A Fan'}),
  (matrix:Movie   {title: 'The Matrix', released: 1999}),
  (wick:Movie     {title: 'John Wick', released: 2014}),

  (keanu)-[:ACTED_IN {roles: ['Neo']}]->(matrix),
  (carrie)-[:ACTED_IN {roles: ['Trinity']}]->(matrix),
  (keanu)-[:ACTED_IN {roles: ['John Wick']}]->(wick),
  (fan)-[:LIKES]->(matrix),
  (fan)-[:LIKES]->(keanu);
