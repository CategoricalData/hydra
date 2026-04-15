package hydra.test.testTerms

import hydra.core.*

def latlonRecord(lat: Float)(lon: Float): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record(hydra.test.testTypes.testTypeLatLonName,
     Seq(hydra.core.Field("lat", hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(lat)))),
     hydra.core.Field("lon", hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(lon)))))))

lazy val testDataArthur: hydra.core.Term = hydra.core.Term.record(hydra.core.Record(hydra.test.testTypes.testTypePersonName,
   Seq(hydra.core.Field("firstName", hydra.core.Term.literal(hydra.core.Literal.string("Arthur"))),
   hydra.core.Field("lastName", hydra.core.Term.literal(hydra.core.Literal.string("Dent"))),
   hydra.core.Field("age", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))))

lazy val testElementArthur: hydra.core.Binding = hydra.core.Binding("firstName", hydra.test.testTerms.testDataArthur,
   Some(hydra.core.TypeScheme(Seq(), hydra.core.Type.variable(hydra.test.testTypes.testTypePersonName),
   None)))

lazy val testElementFirstName: hydra.core.Binding = hydra.core.Binding("firstName",
   hydra.core.Term.project(hydra.core.Projection(hydra.test.testTypes.testTypePersonName,
   "firstName")), Some(hydra.core.TypeScheme(Seq(), hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable(hydra.test.testTypes.testTypePersonName),
   hydra.core.Type.literal(hydra.core.LiteralType.string))), None)))
