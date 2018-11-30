package bluebell.utils.ebmd;

import bluebell.utils.ebmd.Registry;
import bluebell.utils.ebmd.ImplDominatesPromotionCost;
import bluebell.utils.ebmd.ImplDominatesSignature;
import bluebell.utils.ebmd.ArgSpecDominates;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import bluebell.utils.ParetoFrontier;
import java.util.ArrayList;

public class PolyFn {
    private Registry _reg = null;
    private HashSet<Object> _samples;
    private HashSet<Impl> _impls = new HashSet<Impl>();
    private int _lastRebuilt = -1;

    public PolyFn(Registry r, Object key) {
        if (r == null) {
            throw new RuntimeException(
                "Registry must not be null");
        }
        _reg = r;
        //r.addPolyFn(key, this);
    }

    public void addImplementation(
        Impl impl) {
        _impls.add(impl);
        _lastRebuilt = -1;
    }

    public Object call(Object[] args) {
        _reg.rebuildIfNeeded();
        int rebuiltAt = _reg.getRebuiltAt();
        if (rebuiltAt != _lastRebuilt) {
            _samples = new HashSet<Object>();
            Iterator<Impl> iter = _impls.iterator();
            while (iter.hasNext()) {
                Signature sig = iter.next().getSignature();
                sig.accumulateSamples(_reg, _samples);
                sig.rebuild(_reg);
            }
            _lastRebuilt = rebuiltAt;
        }
        ParetoFrontier<Impl> promotionCostFrontier 
            = new ParetoFrontier<Impl>(
                new ImplDominatesPromotionCost());
        Iterator<Impl> iter = _impls.iterator();
        while (iter.hasNext()) {
            Impl e = iter.next().evaluatePromotionPaths(
                args);
            if (e != null) {
                promotionCostFrontier.insert(e);
            }
        }
        ParetoFrontier<Impl> argSpecFrontier 
            = new ParetoFrontier<Impl>(
                new ImplDominatesSignature(
                    new ArgSpecDominates(_reg, _samples)));
        for (Impl x: promotionCostFrontier.getElements()) {
            argSpecFrontier.insert(x);
        }
        ArrayList<Impl> candidates = argSpecFrontier.getElements();
        if (candidates.size() == 0) {
            throw new RuntimeException("No matching call");
        } else if (candidates.size() == 1) {
            return candidates.get(0).evaluate(
                _reg.getSettings().check, args);
        } else {
            throw new RuntimeException(
                "Ambiguous polymorphic dispatch, there are " 
                + candidates.size() + " candidates");
        }
     }
}
