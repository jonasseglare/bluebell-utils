package bluebell.utils.ebmd;

import java.util.ArrayList;

public class PromotionPath implements Comparable<PromotionPath> {
    private ArrayList<Promotion> _path = new ArrayList<Promotion>();

    private Object _dstKey = null;
    private IArgSpec _dstSpec = null;
    double _totalCost = 0.0;

    public PromotionPath() {}

    public void setDst(Object dstKey, IArgSpec dstSpec) {
        _dstKey = dstKey;
        _dstSpec = dstSpec;
    }

    public int compareTo(PromotionPath other) {
        if (_totalCost < other._totalCost) {
            return -1;
        } else if (_totalCost == other._totalCost) {
            return 0;
        }
        return 1;
    }
    
    public void add(Promotion p) {
        if (p == null) {
            throw new RuntimeException("Promotion must not be null");
        }
        _path.add(p);
        _totalCost += p.getCost();
    }
    
    public double getCost() {
        return _totalCost;
    }

    public Object apply(Object input) {
        Object x = input;
        for (int i = 0; i < _path.size(); i++) {
            x = _path.get(i).apply(x);
        }
        return x;
    }

    public String toString() {
        String dst = "PromotionPath cost=" + _totalCost;
        for (int i = 0; i < _path.size(); i++) {
            dst += "\n  " + _path.get(i).toString();
        }
        return dst;
    }

    public boolean matches(Object value) {
        if (_path.isEmpty()) {
            return _dstSpec.evaluate(value);
        } else {
            return _path.get(0).matches(value);
        }
    }

    public Object promote(boolean check, Object src) {
        Object x = src;
        for (int i = 0; i < _path.size(); i++) {
            Promotion p = _path.get(i);
            if (check && !p.matches(x)) {
                throw new RuntimeException("Value '" + x.toString() + "' does not matches promotion '" + p.toString() + "' at i='" + i + "' in " + toString());
            }
            x = p.apply(x);
        }
        if (check && _dstSpec == null) {
            throw new RuntimeException("No dstSpec");
        }
        if (check && !_dstSpec.evaluate(x)) {
            throw new RuntimeException("Value '" + x.toString() + "' of type '" + x.getClass().toString() + "' does not match the final arg-spec in " + toString());
        }
        return x;
    }

    static public PromotionPath findBestMatch(
        ArrayList<PromotionPath> paths,
        Object value) {
        for (int i = 0; i < paths.size(); i++) {
            PromotionPath p = paths.get(i);
            if (p.matches(value)) {
                return p;
            }
        }
        return null;
    }
}
