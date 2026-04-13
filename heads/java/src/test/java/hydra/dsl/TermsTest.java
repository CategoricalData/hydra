package hydra.dsl;

import hydra.core.Projection;
import hydra.core.Term;
import hydra.dsl.prims.Strings;
import hydra.core.Name;
import org.junit.jupiter.api.Test;

import static hydra.dsl.Terms.name;
import static hydra.dsl.Terms.annot;
import static hydra.dsl.Terms.apply;
import static hydra.dsl.Terms.field;
import static hydra.dsl.Terms.float32;
import static hydra.dsl.Terms.inject;
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.list;
import static hydra.dsl.Terms.primitive;
import static hydra.dsl.Terms.project;
import static hydra.dsl.Terms.record;
import static hydra.dsl.Terms.variable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;


/**
 * Test illustrating the use of the Terms API in Java, which is used for constructing terms (untyped expressions).
 */
@SuppressWarnings("unchecked")
public class TermsTest {
    /**
     * See {@link hydra.dsl.TypesTest} for the record type corresponding to this term.
     */
    private final Term bayAreaLatLon = record("LatLon", // records always have a name
            field("lat", float32(37.7749f)), field("lon", float32(-122.4194f)));

    /**
     * An injection term chooses one of the variants of a union type, and supplies an appropriate term.
     * See {@link hydra.dsl.TypesTest} for the union type corresponding to this term
     */
    private final Term bayAreaLocation = inject("Location", field("latlon", bayAreaLatLon));

    /**
     * See {@link hydra.dsl.TypesTest} for a function type corresponding to this term.
     */
    private final Term stringLength = primitive("hydra.lib.strings.length");

    private final Term cat3 =
            lambda("s1", lambda("s2",
                    lambda("s3", apply(Strings.cat(), list(variable("s1"), variable("s2"), variable("s3"))))));

    private final Term longitude = project("LatLon", "lon");

    private final Term longitudeAnnotated = annot(
            new Name("description"), Terms.string("Gets the longitude from a LatLon"),
            longitude);

    private Term.PartialVisitor<Integer> countBoundVariables() {
        return new Term.PartialVisitor<Integer>() {
            @Override
            public Integer visit(Term.Annotated instance) {
                return instance.value.body.accept(countBoundVariables());
            }

            @Override
            public Integer visit(Term.Lambda instance) {
                return 1 + instance.value.body.accept(countBoundVariables());
            }

            @Override
            public Integer otherwise(Term ignored) {
                return 0;
            }
        };
    }

    @Test
    public void constructedTermsAreAsExpected() {
        assertTrue(bayAreaLatLon instanceof Term.Record);
        assertEquals(name("LatLon"), ((Term.Record) bayAreaLatLon).value.typeName);
        assertEquals(float32(-122.4194f), ((Term.Record) bayAreaLatLon).value.fields.get(1).term);

        assertTrue(bayAreaLocation instanceof Term.Union);
        assertEquals(name("Location"), ((Term.Union) bayAreaLocation).value.typeName);
        assertEquals(name("latlon"), ((Term.Union) bayAreaLocation).value.field.name);

        assertTrue(Strings.length() instanceof Term.Variable);
        assertEquals(name("hydra.lib.strings.length"),
                ((Term.Variable) Strings.length()).value);

        assertTrue(cat3 instanceof Term.Lambda);
        assertEquals(new Name("s1"),
                ((Term.Lambda) cat3).value.parameter);

        assertTrue(longitude instanceof Term.Project);
        Projection proj = ((Term.Project) longitude).value;
        assertEquals(name("LatLon"), proj.typeName);
        assertEquals(name("lon"), proj.field);

        assertTrue(longitudeAnnotated instanceof Term.Annotated);
        // Notice that annotations, here, are String-valued
        assertHasDescription(longitudeAnnotated, "Gets the longitude from a LatLon");
        assertEquals(longitude, ((Term.Annotated) longitudeAnnotated).value.body);
    }

    @Test
    public void demonstrateVisitor() {
        assertEquals(0, bayAreaLatLon.accept(countBoundVariables()));
        assertEquals(3, cat3.accept(countBoundVariables()));
    }

    private void assertHasDescription(Term term, String expected) {
        String desc = term.accept(new Term.PartialVisitor<>() {
            @Override
            public String visit(Term.Annotated instance) {
                Term desc = instance.value.annotation.get(new Name("description"));
                return ((hydra.util.Either.Right<?, String>) hydra.extract.Core.string(null, desc)).value;
            }

            @Override
            public String otherwise(Term ignored) {
                fail("not annotated");
                return null;
            }
        });

        assertEquals(expected, desc);
    }
}
