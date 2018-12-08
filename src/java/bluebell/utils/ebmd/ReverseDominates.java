package bluebell.utils;

import bluebell.utils.IDominates;

public class ReverseDominates<T> implements IDominates<T> {
    private IDominates<T> _wrapped;

    public ReverseDominates(IDominates<T> w) {
        _wrapped = w;
    }

    public boolean dominates(T a, T b) {
        return _wrapped.dominates(b, a);
    }
}
