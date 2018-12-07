package bluebell.utils.ebmd;

import java.util.Set;
import bluebell.utils.ebmd.ArgSpecVars;
import java.util.ArrayList;
import bluebell.utils.ParetoFrontier;

public interface IArgSpec {
    public Object getIndirection();
    public boolean evaluate(Object x);
    public void accumulateSamples(Set<Object> dst);
    public void build(Object thisKey, Set<IArgSpec> extensions);
    public void accumulateUnion(Set<IArgSpec> dst);
}

