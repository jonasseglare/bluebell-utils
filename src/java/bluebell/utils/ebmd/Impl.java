package bluebell.utils.ebmd;

import bluebell.utils.ebmd.Signature;
import clojure.lang.IFn;
import clojure.lang.ArraySeq;

public class Impl {
    private Signature _sig;
    private IFn _fn;

    // Extra stuff
    double _promotionCost = Double.POSITIVE_INFINITY;
    private PromotionPath[] _paths = null;

    public Impl(Signature sig, IFn fn) {
        _sig = sig;
        _fn = fn;
    }

    public Signature getSignature() {
        return _sig;
    }
    
    public Impl(Signature sig, IFn fn, PromotionPath[] paths) {
        if (paths.length != sig.getArity()) {
            throw new RuntimeException("Bad length of paths");
        }
        _sig = sig;
        _fn = fn;
        _paths = paths;
        _promotionCost = 0.0;
        for (int i = 0; i < paths.length; i++) {
            _promotionCost += paths[i].getCost();
        }
    }

    public double getPromotionCost() {
        return _promotionCost;
    }

    public Impl evaluatePromotionPaths(Object[] args) {
        PromotionPath[] p = _sig.evaluatePromotionPaths(args);
        if (p == null) {
            return null;
        }
        return new Impl(_sig, _fn, p);
    }

    public Object evaluate(boolean check, Object[] args) {
        if (args == null) {
            throw new RuntimeException("Null args provided");
        }
        if (_paths == null) {
            throw new RuntimeException("No promotion paths");
        }
        if (_paths.length != args.length) {
            throw new RuntimeException("Path array mismatch");
        }

        int n = args.length;
        Object[] tmp = new Object[n];
        for (int i = 0; i < n; i++) {
            tmp[i] = _paths[i].promote(check, args[i]);
        }
        return _fn.applyTo(ArraySeq.create(tmp));
    }
}
