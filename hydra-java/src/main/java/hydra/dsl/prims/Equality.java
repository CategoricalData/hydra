package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.equality.EqualBigfloat;
import hydra.lib.equality.EqualBigint;
import hydra.lib.equality.EqualBinary;
import hydra.lib.equality.EqualBoolean;
import hydra.lib.equality.EqualFloat32;
import hydra.lib.equality.EqualFloat64;
import hydra.lib.equality.EqualInt16;
import hydra.lib.equality.EqualInt32;
import hydra.lib.equality.EqualInt64;
import hydra.lib.equality.EqualInt8;
import hydra.lib.equality.EqualString;
import hydra.lib.equality.EqualTerm;
import hydra.lib.equality.EqualType;
import hydra.lib.equality.EqualUint16;
import hydra.lib.equality.EqualUint32;
import hydra.lib.equality.EqualUint64;
import hydra.lib.equality.EqualUint8;


public interface Equality {
    static Term equalBigfloat() {
        return new EqualBigfloat().term();
    }

    static Term equalBigint() {
        return new EqualBigint().term();
    }

    static Term equalBinary() {
        return new EqualBinary().term();
    }

    static Term equalBoolean() {
        return new EqualBoolean().term();
    }

    static Term equalFloat32() {
        return new EqualFloat32().term();
    }

    static Term equalFloat64() {
        return new EqualFloat64().term();
    }

    static Term equalInt8() {
        return new EqualInt8().term();
    }

    static Term equalInt16() {
        return new EqualInt16().term();
    }

    static Term equalInt32() {
        return new EqualInt32().term();
    }

    static Term equalInt64() {
        return new EqualInt64().term();
    }

    static Term equalString() {
        return new EqualString().term();
    }

    static Term equalTerm() {
        return new EqualTerm().term();
    }

    static Term equalType() {
        return new EqualType().term();
    }

    static Term equalUint8() {
        return new EqualUint8().term();
    }

    static Term equalUint16() {
        return new EqualUint16().term();
    }

    static Term equalUint32() {
        return new EqualUint32().term();
    }

    static Term equalUint64() {
        return new EqualUint64().term();
    }
}
