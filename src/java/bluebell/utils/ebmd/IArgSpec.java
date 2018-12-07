package bluebell.utils.ebmd;

import java.util.Set;
import bluebell.utils.ebmd.ArgSpecVars;

public interface IArgSpec {
    public Object getIndirection();
    public boolean evaluate(Object x);
    public void accumulateSamples(Set<Object> dst);
}

