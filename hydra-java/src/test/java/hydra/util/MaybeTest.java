package hydra.util;

import java.util.Optional;

import org.junit.jupiter.api.Test;

import java.util.NoSuchElementException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class MaybeTest {
    @Test
    public void testParityBetweenOptionalAndOpt() {
        java.util.Optional<String> optional;
        Optional<String> opt;

        optional = java.util.Optional.empty();
        opt = Optional.empty();
        assertTrue(optional.isEmpty());
        assertTrue(opt.isEmpty());
        assertFalse(optional.isPresent());
        assertFalse(opt.isPresent());
        assertThrows(NoSuchElementException.class, optional::get);
        assertThrows(NoSuchElementException.class, opt::get);
        optional.ifPresent(s -> {
            throw new RuntimeException();
        });
        opt.ifPresent(s -> {
            throw new RuntimeException();
        });

        optional = java.util.Optional.of("foo");
        opt = Optional.of("foo");
        assertFalse(optional.isEmpty());
        assertFalse(opt.isEmpty());
        assertTrue(optional.isPresent());
        assertTrue(opt.isPresent());
        assertEquals("foo", optional.get());
        assertEquals("foo", optional.get());
        final java.util.Optional optional2 = optional;
        final Optional<String> opt2 = opt;
        assertThrows(RuntimeException.class, () -> optional2.ifPresent(s -> {
            throw new RuntimeException();
        }));
        assertThrows(RuntimeException.class, () -> opt2.ifPresent(s -> {
            throw new RuntimeException();
        }));

        assertThrows(NullPointerException.class, () -> java.util.Optional.of(null));
        assertThrows(NullPointerException.class, () -> Optional.of(null));
    }
}
