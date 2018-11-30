package bluebell.utils.ebmd;

import java.util.HashMap;
import java.util.Set;
import bluebell.utils.ebmd.IArgSpec;
import clojure.lang.IFn;

public class ArgSpec implements IArgSpec {
    private Set<Object> _pos;
    private Set<Object> _neg;
    private IFn _pred = null;

    public boolean dominatesOnSamples(
        Set<Object> samples,
        ArgSpec other) {
        boolean maybeDom = false;
        for (Object x: samples) {
            boolean a = evaluate(x);
            boolean b = other.evaluate(x);
            if (!maybeDom && !a && b) {
                maybeDom = true;
            } else if (a && !b) {
                return false;
            }
        }
        return maybeDom;
    }

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

    public void accumulateSamples(Set<Object> dst) {
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
}
