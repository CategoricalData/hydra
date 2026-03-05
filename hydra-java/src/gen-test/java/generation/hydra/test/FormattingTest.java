// Note: this is an automatically generated file. Do not edit.
// formatting

package generation.hydra.test;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class FormattingTest {

    // case conversion

    @Test

    public void testCaseConversionNum1LowerSnakeCaseNegUpperSnakeCase() {

        assertEquals(

            "A_HELLO_WORLD_42_A42_42A_B",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.LowerSnake(),
  new hydra.util.CaseConvention.UpperSnake(),
  "a_hello_world_42_a42_42a_b"));

    }

    @Test

    public void testCaseConversionNum2LowerSnakeCaseNegCamelcase() {

        assertEquals(

            "aHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.LowerSnake(),
  new hydra.util.CaseConvention.Camel(),
  "a_hello_world_42_a42_42a_b"));

    }

    @Test

    public void testCaseConversionNum3LowerSnakeCaseNegPascalcase() {

        assertEquals(

            "AHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.LowerSnake(),
  new hydra.util.CaseConvention.Pascal(),
  "a_hello_world_42_a42_42a_b"));

    }

    @Test

    public void testCaseConversionNum4LowerSnakeCaseNegLowerSnakeCase() {

        assertEquals(

            "a_hello_world_42_a42_42a_b",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.LowerSnake(),
  new hydra.util.CaseConvention.LowerSnake(),
  "a_hello_world_42_a42_42a_b"));

    }

    @Test

    public void testCaseConversionNum5UpperSnakeCaseNegLowerSnakeCase() {

        assertEquals(

            "a_hello_world_42_a42_42a_b",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.UpperSnake(),
  new hydra.util.CaseConvention.LowerSnake(),
  "A_HELLO_WORLD_42_A42_42A_B"));

    }

    @Test

    public void testCaseConversionNum6UpperSnakeCaseNegCamelcase() {

        assertEquals(

            "aHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.UpperSnake(),
  new hydra.util.CaseConvention.Camel(),
  "A_HELLO_WORLD_42_A42_42A_B"));

    }

    @Test

    public void testCaseConversionNum7UpperSnakeCaseNegPascalcase() {

        assertEquals(

            "AHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.UpperSnake(),
  new hydra.util.CaseConvention.Pascal(),
  "A_HELLO_WORLD_42_A42_42A_B"));

    }

    @Test

    public void testCaseConversionNum8UpperSnakeCaseNegUpperSnakeCase() {

        assertEquals(

            "A_HELLO_WORLD_42_A42_42A_B",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.UpperSnake(),
  new hydra.util.CaseConvention.UpperSnake(),
  "A_HELLO_WORLD_42_A42_42A_B"));

    }

    @Test

    public void testCaseConversionNum9CamelcaseNegLowerSnakeCase() {

        assertEquals(

            "a_hello_world42_a4242a_b",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Camel(),
  new hydra.util.CaseConvention.LowerSnake(),
  "aHelloWorld42A4242aB"));

    }

    @Test

    public void testCaseConversionNum10CamelcaseNegUpperSnakeCase() {

        assertEquals(

            "A_HELLO_WORLD42_A4242A_B",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Camel(),
  new hydra.util.CaseConvention.UpperSnake(),
  "aHelloWorld42A4242aB"));

    }

    @Test

    public void testCaseConversionNum11CamelcaseNegPascalcase() {

        assertEquals(

            "AHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Camel(),
  new hydra.util.CaseConvention.Pascal(),
  "aHelloWorld42A4242aB"));

    }

    @Test

    public void testCaseConversionNum12CamelcaseNegCamelcase() {

        assertEquals(

            "aHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Camel(),
  new hydra.util.CaseConvention.Camel(),
  "aHelloWorld42A4242aB"));

    }

    @Test

    public void testCaseConversionNum13PascalcaseNegLowerSnakeCase() {

        assertEquals(

            "a_hello_world42_a4242a_b",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Pascal(),
  new hydra.util.CaseConvention.LowerSnake(),
  "AHelloWorld42A4242aB"));

    }

    @Test

    public void testCaseConversionNum14PascalcaseNegUpperSnakeCase() {

        assertEquals(

            "A_HELLO_WORLD42_A4242A_B",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Pascal(),
  new hydra.util.CaseConvention.UpperSnake(),
  "AHelloWorld42A4242aB"));

    }

    @Test

    public void testCaseConversionNum15PascalcaseNegCamelcase() {

        assertEquals(

            "aHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Pascal(),
  new hydra.util.CaseConvention.Camel(),
  "AHelloWorld42A4242aB"));

    }

    @Test

    public void testCaseConversionNum16PascalcaseNegPascalcase() {

        assertEquals(

            "AHelloWorld42A4242aB",

            hydra.formatting.Formatting.convertCase(
  new hydra.util.CaseConvention.Pascal(),
  new hydra.util.CaseConvention.Pascal(),
  "AHelloWorld42A4242aB"));

    }
}
