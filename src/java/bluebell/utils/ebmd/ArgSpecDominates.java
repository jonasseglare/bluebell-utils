package bluebell.utils.ebmd;

import bluebell.utils.IDominates;
import bluebell.utils.ebmd.ArgSpec;
import bluebell.utils.ebmd.Registry;
import java.util.Set;

public class ArgSpecDominates implements IDominates<IArgSpec> {
    private Set<Object> _samples;

    public ArgSpecDominates(Set<Object> s) {
        _samples = s;
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

    public boolean dominates(IArgSpec as, IArgSpec bs) {
        if (as == null) {
            return false;
        } else if (bs == null) {
            return true;
        }
        return dominatesOnSamples(_samples, as, bs);
    }
}
