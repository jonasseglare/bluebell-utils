package bluebell.utils.ebmd;

import bluebell.utils.ebmd.IArgSpec;
import bluebell.utils.ebmd.ArgSpec;
import bluebell.utils.ebmd.ArgSpecVars;
import bluebell.utils.ebmd.ArgSpecVarsVisitor;
import bluebell.utils.ebmd.IndirectArgSpec;
import bluebell.utils.ebmd.EmptyArgSpec;
import bluebell.utils.ebmd.Promotion;
import bluebell.utils.ebmd.PolyFn;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Collections;

public class Registry {
    private Settings _settings;
    private int _mutationCounter = 0;
    private int _rebuiltAt = -1;
    private HashMap<Object, ArgSpecVars> _registry 
        = new HashMap<Object, ArgSpecVars>();

    public Registry(Settings s) {
        _settings = s;
    }

    public Settings getSettings() {
        return _settings;
    }

    public void registerIndirection(Object key, Object target) {
        registerArgSpec(key, new IndirectArgSpec(target));
    }

    public void registerArgSpec(Object key, IArgSpec src) {
        if (src == null) {
            throw new RuntimeException("Provided null argspec");
        }
        _mutationCounter++;
        ArgSpecVars vars = _registry.get(key);
        if (vars != null) {
            vars.argSpec = src;
        } else {
            vars = new ArgSpecVars();
            vars.argSpec = src;
            _registry.put(key, vars);
        }
    }

    public ArgSpecVars trackIndirections(
        Object key, ArgSpecVarsVisitor v) {
        while (true) {
            ArgSpecVars vars = _registry.get(key);
            if (vars == null) {
                throw new RuntimeException("No arg-spec with key " 
                    + key.toString());
            } else {
                IArgSpec as = vars.argSpec;
                if (v != null) {
                    v.visit(vars);
                }
                if (as == null) {
                    throw new RuntimeException(
                        "Missing arg-spec at key " + key.toString());
                } else if (as instanceof ArgSpec) {
                    return vars;
                } else if (as instanceof IndirectArgSpec) {
                    key = ((IndirectArgSpec)as).getTarget();
                }
            }
        }
    }


    public ArgSpec resolve(Object key) {
        return (ArgSpec)trackIndirections(key, null).argSpec;
    }

    public HashMap<Object, Promotion> getPromotions(Object key) {
        HashMap<Object, Promotion> dst 
            = new HashMap<Object, Promotion>();
        trackIndirections(key, new ArgSpecVarsVisitor() {
                public void visit(ArgSpecVars vars) {
                    dst.putAll(vars.promotions);
                }
            });
        return dst;
    }

    private ArgSpecVars getOrMakeArgVarsAtKey(Object k) {
        ArgSpecVars x = _registry.get(k);
        if (x == null) {
            x = new ArgSpecVars();
            _registry.put(k, x);
            return x;
        } else {
            return x;
        }
    }

    public void registerPromotion(
        Object dstKey,
        Promotion prom,
        Object srcKey) {
        _mutationCounter++;
        getOrMakeArgVarsAtKey(dstKey)
            .promotions
            .put(srcKey, prom.withSrcDst(
                    srcKey, dstKey));
    }

    private ArrayList<PromotionPath> listPromotionPathsSub(
        Object dstKey, HashSet<Object> visited) {
        ArrayList<PromotionPath> result 
            = new ArrayList<PromotionPath>();
        if (!visited.contains(dstKey)) {
            visited.add(dstKey);

            ArgSpec sp = resolve(dstKey);
            result.add(new PromotionPath());

            HashMap<Object, Promotion> proms = getPromotions(dstKey);
            
            for (HashMap.Entry<Object, Promotion> kv : 
                     proms.entrySet()) {
                Object subKey = kv.getKey();
                Promotion prom = kv.getValue().resolve(this);

                ArrayList<PromotionPath> paths =
                    listPromotionPathsSub(subKey, visited);

                for (int i = 0; i < paths.size(); i++) {
                    PromotionPath p = paths.get(i);
                    p.add(prom);
                    result.add(p);
                }
            }
            visited.remove(dstKey);
        }
        return result;
    }

    public ArrayList<PromotionPath> listPromotionPaths(
        Object dstKey) {
        HashSet<Object> visited = new HashSet<Object>();
        ArrayList<PromotionPath> p 
            = listPromotionPathsSub(dstKey, visited);
        ArgSpec sp = resolve(dstKey);
        for (int i = 0; i < p.size(); i++) {
            p.get(i).setDst(dstKey, sp);
        }
        Collections.sort(p);
        return p;
    }

    public ArrayList<PromotionPath> getPromotionPaths(Object dstKey) {
        ArgSpecVars v = _registry.get(dstKey);
        if (v == null) {
            throw new RuntimeException(
                "No arg-spec at " + dstKey.toString());
        }
        if (v.promotionPaths == null) {
            throw new RuntimeException(
                "No promotion paths have been built for "
                + dstKey);
        }
        return v.promotionPaths;
    }

    private void rebuildPromotionPaths() {
        for (HashMap.Entry<Object, ArgSpecVars> kv : 
                 _registry.entrySet()) {
            kv.getValue().promotionPaths = 
                listPromotionPaths(kv.getKey());
        }
    }

    public void rebuild() {
        rebuildPromotionPaths();
        _rebuiltAt = _mutationCounter;
    }

    public boolean rebuildIfNeeded() {
        if (_mutationCounter == _rebuiltAt) {
            return false;
        } else {
            rebuild();
            return true;
        }
    }

    public int getRebuiltAt() {
        return _rebuiltAt;
    }

    public Set<Object> getArgSpecKeys() {
        return _registry.keySet();
    }
}
