package hydra.impl.java;

import hydra.core.FloatType;
import hydra.core.IntegerType;
import hydra.core.Type;
import org.junit.jupiter.api.Test;

import static hydra.impl.java.dsl.Core.*;
import static hydra.impl.java.dsl.Types.*;
import static org.junit.jupiter.api.Assertions.*;


/**
 * Test illustrating the use of the Types API in Java, which is used for constructing type definitions.
 *
 * Note: for simplicity, these types are annotated using the String class. A more typical annotation class is {@link hydra.core.Meta}
 */
@SuppressWarnings("unchecked")
public class TypesTest {

  private final Type<String> latLonType = record("LatLon", // record types always have a name
      field("lat", float32()),
      field("lon", float32()));

  private final Type<String> locationType = union("Location",
      field("name", string()),
      field("latlon", nominal("LatLon"))); // refer to named types using nominal(name)

  private final Type<String> stringToIntType = function(
      string(), int32());

  private final Type<String> pairOfLatLonsToFloatType = function(
      nominal("LatLon"), nominal("LatLon"), float32());

  private final Type<String> annotatedType = annot("this is a string-annotated double type", float64());

  @Test
  public void constructedTypesAreAsExpected() {
    assertTrue(latLonType instanceof Type.Record);
    assertEquals(name("LatLon"), ((Type.Record<String>) latLonType).value.typeName);
    assertEquals(fieldName("lat"), ((Type.Record<String>) latLonType).value.fields.get(0).name);

    assertTrue(locationType instanceof Type.Union);
    assertEquals(name("Location"), ((Type.Union<String>) locationType).value.typeName);
    assertEquals(fieldName("latlon"), ((Type.Union<String>) locationType).value.fields.get(1).name);

    assertTrue(stringToIntType instanceof Type.Function);
    assertEquals(string(), ((Type.Function<String>) stringToIntType).value.domain);
    assertEquals(int32(), ((Type.Function<String>) stringToIntType).value.codomain);

    assertTrue(pairOfLatLonsToFloatType instanceof Type.Function);
    assertTrue(((Type.Function<String>) pairOfLatLonsToFloatType).value.domain instanceof Type.Nominal);
    assertEquals(name("LatLon"), ((Type.Nominal<String>) ((Type.Function<String>) pairOfLatLonsToFloatType).value.domain).value);

    assertTrue(annotatedType instanceof Type.Annotated);
    // Notice that annotations, here, are String-valued
    assertEquals("this is a string-annotated double type", ((Type.Annotated<String>) annotatedType).value.annotation);
    assertEquals(float64(), ((Type.Annotated<String>) annotatedType).value.subject);
  }

  @Test
  public void demonstrateVisitor() {
    Type.PartialVisitor<Integer> countFields = new Type.PartialVisitor<Integer>() {
      @Override
      public Integer visit(Type.Record instance) {
        return instance.value.fields.size();
      }

      @Override
      public Integer visit(Type.Union instance) {
        return instance.value.fields.size();
      }

      @Override
      public Integer otherwise(Type ignored) {
        return 0;
      }
    };

    assertEquals(2, latLonType.accept(countFields));
    assertEquals(2, locationType.accept(countFields));
    assertEquals(0, stringToIntType.accept(countFields));
  }

  public void ignoreMe() {
      hydra.core.FloatType it = new hydra.core.FloatType.Float32();
      String s = it.accept(new hydra.core.FloatType.Visitor<String>() {
          @Override
          public String visit(FloatType.Bigfloat instance) {
              return instance.toString();
          }

          @Override
          public String visit(FloatType.Float32 instance) {
              return instance.toString();
          }

          @Override
          public String visit(FloatType.Float64 instance) {
              return instance.toString();
          }
      });
  }
}
