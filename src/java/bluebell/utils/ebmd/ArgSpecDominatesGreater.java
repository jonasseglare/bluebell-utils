package bluebell.utils.ebmd;

import bluebell.utils.IDominates;
import bluebell.utils.ebmd.ArgSpec;
import bluebell.utils.ebmd.ArgSpecDominates;
import bluebell.utils.ebmd.Registry;
import java.util.Set;

public class ArgSpecDominatesGreater implements IDominates<IArgSpec> {
    private ArgSpecDominates _sub;

    public ArgSpecDominatesGreater(Set<Object> s) {
        _sub = new ArgSpecDominates(s);
    }

    public boolean dominates(IArgSpec as, IArgSpec bs) {
        return _sub.dominates(bs, as);
    }
}
