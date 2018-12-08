package bluebell.utils.ebmd;

import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
import bluebell.utils.ebmd.IArgSpec;
import bluebell.utils.ebmd.Promotion;
import bluebell.utils.IDominates;
import bluebell.utils.ebmd.Registry;
import java.util.Objects;

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

    public void accumulateOwnSamples(Set<Object> dst) {}

    public void build(
        Object thisKey, 
        Registry r, 
        Set<IArgSpec> extensions) {}

    public boolean equivalentOnSamples(
        Set<Object> samples, IArgSpec other) {
        if (other instanceof IndirectArgSpec) {
            return Objects.equals(
                _target, ((IndirectArgSpec)other)._target);
        }
        return false;
    }
}
