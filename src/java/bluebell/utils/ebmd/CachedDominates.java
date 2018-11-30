package bluebell.utils.ebmd;

import java.util.HashMap;
import bluebell.utils.IDominates;
import bluebell.utils.ebmd.PairKey;

public class CachedDominates<T> implements IDominates<T> {
    
    IDominates<T> _dom = null;
    private int _evalCounter = 0;

    private HashMap<PairKey, Boolean> _cache 
        = new HashMap<PairKey, Boolean>();

    public CachedDominates(IDominates<T> dom) {
        _dom = dom;
    }

    public int getEvalCounter() {
        return _evalCounter;
    }

    public boolean dominates(T a, T b) {
        PairKey k = new PairKey(a, b);
        Boolean value = _cache.get(k);
        if (value == null) {
            _evalCounter++;
            value = _dom.dominates(a, b);
            _cache.put(k, value);
        }
        return value;
    }
}
