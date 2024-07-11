package hydra.util;

/**
 * A simple class for tuples (instances of product types) in Java.
 * Tuples above a length of 9 are currently unsupported. See javaMaxTupleLength.
 */
public abstract class Tuple {
    /**
     * A zero-tuple, i.e. unit element.
     */
    public static class Tuple0 extends Tuple {
    }

    /**
     * A one-tuple.
     */
    public static class Tuple1<T1> extends Tuple0 {
        public final T1 object1;

        public Tuple1(T1 object1) {
            this.object1 = object1;
        }
    }

    /**
     * A two-tuple, i.e. a pair.
     */
    public static class Tuple2<T1, T2> extends Tuple1<T1> {
        public final T2 object2;

        public Tuple2(T1 object1, T2 object2) {
            super(object1);
            this.object2 = object2;
        }
    }

    /**
     * A three-tuple, i.e. a triple.
     */
    public static class Tuple3<T1, T2, T3> extends Tuple2<T1, T2> {
        public final T3 object3;

        public Tuple3(T1 object1, T2 object2, T3 object3) {
            super(object1, object2);
            this.object3 = object3;
        }
    }

    /**
     * A four-tuple, i.e. a quad.
     */
    public static class Tuple4<T1, T2, T3, T4> extends Tuple3<T1, T2, T3> {
        public final T4 object4;

        public Tuple4(T1 object1, T2 object2, T3 object3, T4 object4) {
            super(object1, object2, object3);
            this.object4 = object4;
        }
    }

    /**
     * A five-tuple.
     */
    public static class Tuple5<T1, T2, T3, T4, T5> extends Tuple4<T1, T2, T3, T4> {
        public final T5 object5;

        public Tuple5(T1 object1, T2 object2, T3 object3, T4 object4, T5 object5) {
            super(object1, object2, object3, object4);
            this.object5 = object5;
        }
    }

    /**
     * A six-tuple.
     */
    public static class Tuple6<T1, T2, T3, T4, T5, T6> extends Tuple5<T1, T2, T3, T4, T5> {
        public final T6 object6;

        public Tuple6(T1 object1, T2 object2, T3 object3, T4 object4, T5 object5, T6 object6) {
            super(object1, object2, object3, object4, object5);
            this.object6 = object6;
        }
    }

    /**
     * A seven-tuple.
     */
    public static class Tuple7<T1, T2, T3, T4, T5, T6, T7> extends Tuple6<T1, T2, T3, T4, T5, T6> {
        public final T7 object7;

        public Tuple7(T1 object1, T2 object2, T3 object3, T4 object4, T5 object5, T6 object6, T7 object7) {
            super(object1, object2, object3, object4, object5, object6);
            this.object7 = object7;
        }
    }

    /**
     * An eight-tuple.
     */
    public static class Tuple8<T1, T2, T3, T4, T5, T6, T7, T8> extends Tuple7<T1, T2, T3, T4, T5, T6, T7> {
        public final T8 object8;

        public Tuple8(T1 object1, T2 object2, T3 object3, T4 object4, T5 object5, T6 object6, T7 object7, T8 object8) {
            super(object1, object2, object3, object4, object5, object6, object7);
            this.object8 = object8;
        }
    }

    /**
     * A nine-tuple.
     */
    public static class Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9> extends Tuple8<T1, T2, T3, T4, T5, T6, T7, T8> {
        public final T9 object9;

        public Tuple9(T1 object1, T2 object2, T3 object3, T4 object4, T5 object5, T6 object6, T7 object7, T8 object8,
                      T9 object9) {
            super(object1, object2, object3, object4, object5, object6, object7, object8);
            this.object9 = object9;
        }
    }
}
