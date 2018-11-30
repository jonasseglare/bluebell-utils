package bluebell.utils.ebmd;

import java.util.HashMap;
import bluebell.utils.ebmd.IArgSpec;
import bluebell.utils.ebmd.Promotion;

public class IndirectArgSpec implements IArgSpec {
    Object _target = null;

    public IndirectArgSpec(Object target) {
        _target = target;
    }
    
    public Object getTarget() {
        return _target;
    }
}
