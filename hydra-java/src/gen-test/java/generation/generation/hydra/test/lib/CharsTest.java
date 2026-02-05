// Note: this is an automatically generated file. Do not edit.
// hydra.lib.chars primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class CharsTest {

    // isAlphaNum

    @Test

    public void testIsalphanumLetter() {

        assertEquals(

            true,

            hydra.lib.chars.IsAlphaNum.apply(97));

    }

    @Test

    public void testIsalphanumDigit() {

        assertEquals(

            true,

            hydra.lib.chars.IsAlphaNum.apply(53));

    }

    @Test

    public void testIsalphanumSpace() {

        assertEquals(

            false,

            hydra.lib.chars.IsAlphaNum.apply(32));

    }

    @Test

    public void testIsalphanumPunctuation() {

        assertEquals(

            false,

            hydra.lib.chars.IsAlphaNum.apply(46));

    }

    // isLower

    @Test

    public void testIslowerLowercase() {

        assertEquals(

            true,

            hydra.lib.chars.IsLower.apply(97));

    }

    @Test

    public void testIslowerUppercase() {

        assertEquals(

            false,

            hydra.lib.chars.IsLower.apply(65));

    }

    @Test

    public void testIslowerDigit() {

        assertEquals(

            false,

            hydra.lib.chars.IsLower.apply(53));

    }

    // isSpace

    @Test

    public void testIsspaceSpace() {

        assertEquals(

            true,

            hydra.lib.chars.IsSpace.apply(32));

    }

    @Test

    public void testIsspaceTab() {

        assertEquals(

            true,

            hydra.lib.chars.IsSpace.apply(9));

    }

    @Test

    public void testIsspaceNewline() {

        assertEquals(

            true,

            hydra.lib.chars.IsSpace.apply(10));

    }

    @Test

    public void testIsspaceLetter() {

        assertEquals(

            false,

            hydra.lib.chars.IsSpace.apply(97));

    }

    // isUpper

    @Test

    public void testIsupperUppercase() {

        assertEquals(

            true,

            hydra.lib.chars.IsUpper.apply(65));

    }

    @Test

    public void testIsupperLowercase() {

        assertEquals(

            false,

            hydra.lib.chars.IsUpper.apply(97));

    }

    @Test

    public void testIsupperDigit() {

        assertEquals(

            false,

            hydra.lib.chars.IsUpper.apply(53));

    }

    // toLower

    @Test

    public void testTolowerUppercase() {

        assertEquals(

            97,

            hydra.lib.chars.ToLower.apply(65));

    }

    @Test

    public void testTolowerLowercase() {

        assertEquals(

            97,

            hydra.lib.chars.ToLower.apply(97));

    }

    @Test

    public void testTolowerDigit() {

        assertEquals(

            53,

            hydra.lib.chars.ToLower.apply(53));

    }

    // toUpper

    @Test

    public void testToupperLowercase() {

        assertEquals(

            65,

            hydra.lib.chars.ToUpper.apply(97));

    }

    @Test

    public void testToupperUppercase() {

        assertEquals(

            65,

            hydra.lib.chars.ToUpper.apply(65));

    }

    @Test

    public void testToupperDigit() {

        assertEquals(

            53,

            hydra.lib.chars.ToUpper.apply(53));

    }
}
