package bluebell.utils.ebmd;

import java.util.Objects;

public class PairKey {
    Object _a = null;
    Object _b = null;
        
    public PairKey(Object a, Object b) {
        _a = a;
        _b = b;
    }

    public boolean equalsWithNull(Object a, Object b) {
        return ((a == null) && (b == null)) 
            || ((a != null) && a.equals(b));
    }

    public boolean equals(Object other) {
        if (other instanceof PairKey) {
            PairKey k = (PairKey)other;
            return equalsWithNull(_a, k._a) 
                && equalsWithNull(_b, k._b);
        }
        return false;
    }

    public int hashCode() {
        return Objects.hash(_a, _b);
    }
}
