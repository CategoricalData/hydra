package hydra.overlay.java.tinkerpop;

import hydra.tinkerpop.gremlin.RootTraversal;

import org.apache.tinkerpop.gremlin.language.grammar.GremlinQueryParser;
import org.apache.tinkerpop.gremlin.process.traversal.Bytecode;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;
import org.apache.tinkerpop.gremlin.process.traversal.translator.GroovyTranslator;

/**
 * Gremlin-text entry points, tying the {@link HydraToBytecode} / {@link BytecodeToHydra} mappers to
 * TinkerPop's own ANTLR parser and Groovy translator.
 *
 * <ul>
 *   <li><b>Gremlin text → Hydra:</b> {@link #parse(String)} uses TinkerPop's
 *       {@link GremlinQueryParser} (the compiled ANTLR parser from {@code Gremlin.g4}) to produce a
 *       {@link Traversal}, takes its {@link Bytecode}, and maps it with {@link BytecodeToHydra}.
 *       No grammar is vendored here.</li>
 *   <li><b>Hydra → Gremlin text:</b> {@link #toGremlin(RootTraversal)} maps to {@link Bytecode} via
 *       {@link HydraToBytecode}, then renders it with TinkerPop's {@link GroovyTranslator}.</li>
 * </ul>
 *
 * <p>For execution rather than text, map Hydra → {@link Bytecode} with {@link HydraToBytecode} and use
 * {@code org.apache.tinkerpop.gremlin.process.traversal.translator.JavaTranslator.of(g).translate(bc)}
 * to obtain a runnable {@code GraphTraversal} on a {@link GraphTraversalSource}.
 */
public final class GremlinText {

    private GremlinText() {
    }

    /**
     * Parses a Gremlin query string into the Hydra {@code hydra.tinkerpop.gremlin} model.
     *
     * <p>Uses TinkerPop's own ANTLR parser; the resulting {@link Traversal}'s {@link Bytecode} is mapped
     * by {@link BytecodeToHydra}. Coverage of step mapping is whatever {@link BytecodeToHydra} supports;
     * unmapped steps throw {@link UnsupportedOperationException}.
     */
    public static RootTraversal parse(String gremlin) {
        Object parsed = GremlinQueryParser.parse(gremlin);
        if (!(parsed instanceof Traversal)) {
            throw new IllegalArgumentException(
                    "Gremlin string did not parse to a Traversal (got "
                            + (parsed == null ? "null" : parsed.getClass().getName()) + "): " + gremlin);
        }
        Bytecode bc = ((Traversal<?, ?>) parsed).asAdmin().getBytecode();
        return BytecodeToHydra.fromBytecode(bc);
    }

    /**
     * Renders a Hydra {@link RootTraversal} as a Gremlin-Groovy query string, via {@link Bytecode} and
     * TinkerPop's {@link GroovyTranslator}.
     */
    public static String toGremlin(RootTraversal rt) {
        Bytecode bc = HydraToBytecode.toBytecode(rt);
        return GroovyTranslator.of("g").translate(bc).getScript();
    }
}
