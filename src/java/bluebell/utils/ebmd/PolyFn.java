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
import java.util.Set;
import java.util.Map;
import clojure.lang.Symbol;
import bluebell.utils.ReadAndUpdateMachine;
import bluebell.utils.ConstantCallable;
import java.util.concurrent.Callable;
import bluebell.utils.MemoizedDominates;
import bluebell.utils.IDominates;
import java.util.Objects;


public class PolyFn {
    private ReadAndUpdateMachine _raum = new ReadAndUpdateMachine();
    private Registry _reg = null;
    private HashMap<Integer, HashMap<Signature, Impl>> 
        _implsPerArity 
        = new HashMap<Integer, HashMap<Signature, Impl>>();
    private IDominates<Impl> _implDominates;
    private int _lastRebuilt = -1;
    private Symbol _sym = null;

    public PolyFn(Registry r, Symbol sym) {
        if (r == null) {
            throw new RuntimeException(
                "Registry must not be null");
        }
        _sym = sym;
        _reg = r;
    }

    public HashSet<Object> getAllArgSpecs() {
        HashSet<Object> result = new HashSet<Object>();
        for (Map.Entry<Integer, HashMap<Signature, Impl>> kv: 
                 _implsPerArity.entrySet()) {
            for (Map.Entry<Signature, Impl> kv2: 
                     kv.getValue().entrySet()) {
                Object[] keys = kv2.getKey().getArgSpecKeys();
                for (int i = 0; i < keys.length; i++) {
                    result.add(keys[i]);
                }
            }
        }
        return result;
    }

    private HashMap<Signature, Impl> getImplsForArity(int arity) {
        HashMap<Signature, Impl> impls = _implsPerArity.get(arity);
        if (impls == null) {
            impls = new HashMap<Signature, Impl>();
            _implsPerArity.put(arity, impls);
        }
        return impls;
    }

    public void addImplementation(
        Impl impl) {
        _raum.withUpdate(new ConstantCallable<Integer>() {
                public void run() {

                    Signature sig = impl.getSignature();
                    HashMap<Signature, Impl> impls 
                        = getImplsForArity(sig.getArity());
                    Impl existing = impls.get(sig);
                    if (existing == null) {
                        impls.put(sig, impl);
                        _lastRebuilt = -1;
                    } else {
                        existing.setFn(impl.getFn());
                    }
                
                }
            });
    }

    private String shorten(String s) {
        int n = 30;
        int m = 12;
        if (s.length() <= (n + m)) {
            return s;
        } else {
            return s.substring(0, n) + "...";
        }
    }

    private void rebuildSub() {
        int rebuiltAt = _reg.getRebuiltAt();
        _implDominates = new MemoizedDominates<Impl>(
            new ImplDominatesSignature(
                _reg.getArgSpecDominates()));
        if (rebuiltAt != _lastRebuilt) {
            for (Map.Entry<Integer, 
                     HashMap<Signature, Impl>> 
                     entry: 
                     _implsPerArity.entrySet()) {
                HashMap<Signature, Impl> impls 
                    = entry.getValue();
                for (Map.Entry<Signature, Impl> 
                         kv: 
                         impls.entrySet()) {
                    Signature sig = kv.getKey();
                    sig.rebuild(_reg);
                }
            }
            _lastRebuilt = rebuiltAt;
        }
    }

    private void rebuildIfNeeded() {
        ReadAndUpdateMachine regRaum = _reg.getReadAndUpdateMachine();
        regRaum.withTryUpdate(
            new Runnable() {
                public void run() {
                    _reg.rebuildIfNeeded();
                }
            });

        // Once the registry is rebuilt, we can update *this* PolyFn,
        // if possible
        regRaum.withRead(new ConstantCallable<Integer>() {
                public void run() {
                    _raum.withTryUpdate(new Runnable() {
                            public void run() {
                                rebuildSub();
                            }
                        });
                }
            });
    }

    private Object callSub(Object[] args) {
        ParetoFrontier<Impl> promotionCostFrontier 
            = new ParetoFrontier<Impl>(
                new ImplDominatesPromotionCost());
        HashMap<Signature, Impl> impls = getImplsForArity(args.length);
        for (Map.Entry<Signature, Impl> kv: impls.entrySet()) {
            Impl e = kv.getValue().evaluatePromotionPaths(args);
            if (e != null) {
                promotionCostFrontier.insert(e);
            }
        }
        ParetoFrontier<Impl> argSpecFrontier 
            = new ParetoFrontier<Impl>(_implDominates);
        for (Impl x: promotionCostFrontier.getElements()) {
            argSpecFrontier.insert(x);
        }
        ArrayList<Impl> candidates = argSpecFrontier.getElements();
        if (candidates.size() == 0) {
            String msg = String.format(
                "No matching call to polymorphic function %s for these arguments",
                Objects.toString(_sym));
            for (int i = 0; i < args.length; i++) {
                msg += "\n  " + i + ": " 
                    + shorten(Objects.toString(args[i]));
            }
            throw new RuntimeException(msg);
        } else if (candidates.size() == 1) {
            return candidates.get(0).evaluate(
                _reg.getSettings().check, args);
        } else {
            String msg = "Ambiguous polymorphic dispatch, there are " 
                + candidates.size() + " candidates:";
            for (Impl c: candidates) {
                msg += "\n  * " + c.getSignature().toString();
            }
            throw new RuntimeException(msg);
        }
    }

    public Object call(Object[] args) {
        rebuildIfNeeded();
        return _reg.getReadAndUpdateMachine().withRead(
            new Callable<Object>() {
                public Object call() {
                    return _raum.withRead(new Callable<Object>() {
                            public Object call() {
                                return callSub(args);
                            }
                        });
                }
            });
    }

    public String renderPolySummary() {
        String summary = "Polymorphic function '" 
            + _sym.toString() + "'";
        for (Map.Entry<Integer, HashMap<Signature, Impl>> kv: 
                 _implsPerArity.entrySet()) {
            summary += "\nArity " + kv.getKey().toString();
            for (Map.Entry<Signature, Impl> si: kv.getValue().entrySet()) {
                Signature sig = si.getKey();
                if (sig == null) {
                    throw new RuntimeException(
                        "Null signatures not allowed");
                }
                summary += "\n  " + sig.toString();
            }
        }
        return summary;
    }

    public Set<Integer> getArities() {
        return _implsPerArity.keySet();
    }
}
