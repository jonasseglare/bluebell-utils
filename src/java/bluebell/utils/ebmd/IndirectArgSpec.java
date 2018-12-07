package bluebell.utils.ebmd;

import java.util.HashMap;
import java.util.Set;
import bluebell.utils.ebmd.IArgSpec;
import bluebell.utils.ebmd.Promotion;

public class IndirectArgSpec implements IArgSpec {
    Object _target = null;

    public IndirectArgSpec(Object target) {
        _target = target;
    }
    
    public Object getIndirection() {
        return _target;
    }

    public boolean evaluate(Object x) {
        throw new RuntimeException("IndirectArgSpec cannot evaluate");
    }

    public void accumulateSamples(Set<Object> dst) {
        throw new RuntimeException(
            "IndirectArgSpec cannot accumulate samples");
    }
}
