package bluebell.utils.ebmd;

import java.util.HashMap;
import java.util.Set;
import bluebell.utils.ebmd.IArgSpec;
import clojure.lang.IFn;
import java.util.ArrayList;
import bluebell.utils.ParetoFrontier;
import bluebell.utils.IDominates;
import bluebell.utils.ebmd.Registry;
import java.util.HashSet;

public class ArgSpec implements IArgSpec {
    private Set<Object> _pos;
    private Set<Object> _neg;
    private IFn _pred = null;

    public ArgSpec(
        IFn pred, Set<Object> pos, Set<Object> neg) {
        if (pred == null) {
            throw new RuntimeException("pred must not be null");
        }
        if (pos == null) {
            throw new RuntimeException("pos must not be null");
        }
        if (neg == null) {
            throw new RuntimeException("neg must not be null");
        }
        _pred = pred;
        _pos = pos;
        _neg = neg;

        for (Object x: _pos) {
            if (!evaluate(x)) {
                throw new RuntimeException("Predicate " + _pred.toString() + " returned false for positive sample " + x);
            }
        }
        for (Object x: _neg) {
            if (evaluate(x)) {
                throw new RuntimeException("Predicate " + _pred.toString() + " returned true for negative sample " + x);
            }
        }
    }

    public boolean evaluate(Object x) {
        Object result = _pred.invoke(x);
        if (result == null) {
            return false;
        } else if (result instanceof Boolean) {
            return ((Boolean)result).booleanValue();
        } else {
            return true;
        }
    }

    public void accumulateOwnSamples(Set<Object> dst) {
        dst.addAll(_pos);
        dst.addAll(_neg);
    }

    public IFn getPredicate() {
        return _pred;
    }

    public Set<Object> getPositive() {
        return _pos;
    }

    public Set<Object> getNegative() {
        return _neg;
    }

    public Object getIndirection() {
        return null;
    }

    public Set<Object> filter(Set<Object> c) {
        HashSet<Object> dst = new HashSet<Object>();
        for (Object o: c) {
            if (evaluate(o)) {
                dst.add(o);
            }
        }
        return dst;
    }

    public boolean equivalentOnSamples(
        Set<Object> samples, IArgSpec other) {
        if (other instanceof ArgSpec) {
            ArgSpec as = (ArgSpec)other;
            boolean eq = _pos.equals(as._pos) 
                && _neg.equals(as._neg)
                && filter(samples).equals(as.filter(samples));
            if (eq) {
                // Consider renaming 'equivalentOnSamples'
                // to include the word 'update'
                _pred = as._pred;
            }
            return eq;
        }
        return false;
    }

    public void build(
        Object thisKey, 
        Registry r, 
        Set<IArgSpec> extensions) {
        if (0 < extensions.size()) {
            System.out.println(
                "WARNING: The ArgSpec with key " 
                + thisKey + " cannot have extensions");
        }
    }

    public void accumulateUnion(
        Set<IArgSpec> dst) {
        dst.add(this);
    }
}
