package bluebell.utils.ebmd;

import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
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

    public void accumulateUnion(Set<IArgSpec> dst) {
        throw new RuntimeException("Not applicable");
    }

    public void accumulateSamples(Set<Object> dst) {
        throw new RuntimeException(
            "IndirectArgSpec cannot accumulate samples");
    }

    public void build(Object thisKey, Set<IArgSpec> extensions) {}
}
