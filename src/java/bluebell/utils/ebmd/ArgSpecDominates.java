package bluebell.utils.ebmd;

import bluebell.utils.IDominates;
import bluebell.utils.ebmd.ArgSpec;
import bluebell.utils.ebmd.Registry;
import java.util.Set;

public class ArgSpecDominates implements IDominates<Object> {
    private Set<Object> _samples;
    private Registry _reg;

    public ArgSpecDominates(Registry r, Set<Object> s) {
        _samples = s;
        _reg = r;
    }

    private boolean dominatesOnSamples(
        Set<Object> samples, IArgSpec as, IArgSpec bs) {
        boolean maybeDom = false;
        for (Object x: samples) {
            boolean a = as.evaluate(x);
            boolean b = bs.evaluate(x);
            if (!maybeDom && !a && b) {
                maybeDom = true;
            } else if (a && !b) {
                return false;
            }
        }
        return maybeDom;
    }

    public boolean dominates(Object aKey, Object bKey) {
        if (aKey == null) {
            return false;
        } else if (bKey == null) {
            return true;
        }
        IArgSpec a = _reg.resolve(aKey);
        IArgSpec b = _reg.resolve(bKey);
        return dominatesOnSamples(_samples, a, b);
    }
}
