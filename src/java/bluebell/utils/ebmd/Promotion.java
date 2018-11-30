package bluebell.utils.ebmd;

import clojure.lang.IFn;

public class Promotion {
    double _cost = 1.0;
    IFn _promoter = null;
    Object _src = null;
    ArgSpec _srcSpec = null;
    Object _dst = null;

    private void validate() {
        if (_promoter == null) {
            throw new RuntimeException("No promotoer");
        }
        if (_cost <= 0.0) {
            throw new RuntimeException("No promotion cost");
        }
    }

    public Promotion(IFn prom, double cost) {
        _promoter = prom;
        _cost = cost;
        validate();
    }

    public Promotion(IFn prom) {
        _promoter = prom;
        validate();
    }

    public double getCost() {
        return _cost;
    }

    public Object apply(Object x) {
        return _promoter.invoke(x);
    }

    Promotion withSrcDst(Object src, Object dst) {
        Promotion p = new Promotion(_promoter, _cost);
        p._src = src;
        p._dst = dst;
        return p;
    }

    Promotion resolve(Registry reg) {
        Promotion p = withSrcDst(_src, _dst);
        p._srcSpec = reg.resolve(_src);
        return p;
    }

    public boolean matches(Object x) {
        if (_srcSpec == null) {
            throw new RuntimeException(
                "No source spec for promotion "
                + toString());
        }
        return _srcSpec.evaluate(x);
    }

    public String toString() {
        if (_src == null) {
            return "Unknown promotion";
        }
        return _src.toString() + "--(" + _cost 
            + ")-->" + _dst.toString();
    }
}
