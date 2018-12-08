package bluebell.utils;

import bluebell.utils.IDominates;
import java.util.HashMap;
import java.util.Objects;
import java.util.Map;

public class MemoizedDominates<T> 
                        implements IDominates<T> {
    public class Pair {
        private T _a;
        private T _b;

        public Pair(T a, T b) {
            _a = a;
            _b = b;
        }

        public boolean equals(Object other) {
            if (other instanceof MemoizedDominates.Pair) {
                MemoizedDominates.Pair p 
                    = (MemoizedDominates.Pair)(other);
                return Objects.equals(_a, p._a)
                    && Objects.equals(_b, p._b);
            }
            return false;
        }

        public int hashCode() {
            return Objects.hash(_a, _b);
        }
    }

    private HashMap<Pair, Boolean> _memory 
        = new HashMap<Pair, Boolean>();
    private IDominates<T> _wrapped;

    public MemoizedDominates(IDominates<T> wrapped) {
        _wrapped = wrapped;
    }

    synchronized public boolean dominates(T a, T b) {
        Pair p = new Pair(a, b);
        Boolean value = _memory.get(p);
        if (value == null) {
            boolean x = _wrapped.dominates(a, b);
            _memory.put(p, x);
            return x;
        } else {
            return value;
        }
    }

    public void disp() {
        System.out.println("Memoized dominates entry set");
        for (Map.Entry<Pair, Boolean> entry: _memory.entrySet()) {
            Pair k = entry.getKey();
            Boolean v = entry.getValue();
            System.out.println("  - (" + k._a.toString() + 
                ", " + k._b.toString() + ") = " +
                v.toString());
        }
    }
}
