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

    public boolean dominates(Object aKey, Object bKey) {
        if (aKey == null) {
            return false;
        } else if (bKey == null) {
            return true;
        }
        ArgSpec a = _reg.resolve(aKey);
        ArgSpec b = _reg.resolve(bKey);
        return a.dominatesOnSamples(_samples, b);
    }
}
