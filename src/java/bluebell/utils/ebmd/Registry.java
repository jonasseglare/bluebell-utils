package bluebell.utils.ebmd;

import bluebell.utils.ebmd.IArgSpec;
import bluebell.utils.ebmd.ArgSpec;
import bluebell.utils.ebmd.ArgSpecVars;
import bluebell.utils.ebmd.ArgSpecVarsVisitor;
import bluebell.utils.ebmd.IndirectArgSpec;
import bluebell.utils.ebmd.Promotion;
import bluebell.utils.ebmd.PolyFn;
import bluebell.utils.ReadAndUpdateMachine;
import bluebell.utils.ReadAndUpdateMachineSettings;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Collections;
import java.util.concurrent.Callable;
import java.lang.Runnable;
import bluebell.utils.GraphUtils;
import bluebell.utils.INeighborhoodFunction;
import java.util.List;
import bluebell.utils.MemoizedDominates;
import bluebell.utils.ebmd.ArgSpecDominates;
import bluebell.utils.IDominates;

public class Registry {
    private ReadAndUpdateMachine _raum = new ReadAndUpdateMachine(
        ReadAndUpdateMachineSettings.debugSettings());
    private Settings _settings;
    private int _mutationCounter = 0;
    private int _rebuiltAt = -1;
    private HashMap<Object, ArgSpecVars> _registry 
        = new HashMap<Object, ArgSpecVars>();
    private HashSet<Object> _allSamples;
    private MemoizedDominates<IArgSpec> _argSpecDominates;

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
        _raum.withUpdate(new Callable<Integer>() {
                public Integer call() {
                    if (src == null) {
                        throw new RuntimeException("Provided null argspec");
                    }
                    ArgSpecVars vars = _registry.get(key);
                    if (vars != null) {
                        IArgSpec old = vars.argSpec;
                        if (_allSamples == null || !vars.argSpec.equivalentOnSamples(_allSamples, src)) {
                            vars.argSpec = src;
                            _mutationCounter++;
                        }
                    } else {
                        vars = new ArgSpecVars();
                        vars.argSpec = src;
                        _registry.put(key, vars);
                        _mutationCounter++;
                    }
                    return 0;
                }
            });
    }

    public void registerArgSpecUnion(Object key) {
        registerArgSpec(key, new ArgSpecUnion());
    }

    public ArgSpecVars trackIndirections(
        Object key, ArgSpecVarsVisitor v) {
        return ArgSpecVars.trackIndirections(_registry, key, v);
    }


    public IArgSpec resolve(Object key) {
        return trackIndirections(key, null).argSpec;
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
            _mutationCounter++;
            x = new ArgSpecVars();
            _registry.put(k, x);
            return x;
        } else {
            return x;
        }
    }

    public void extendArgSpec(Object dstKey, Object src) {
        _raum.withUpdate(new Callable<Integer>() {
                public Integer call() {
                    if (getOrMakeArgVarsAtKey(dstKey).extensions.add(src)) {
                        _mutationCounter++;
                    }
                    return 0;
                }
            });
    }

    public void registerPromotion(
        Object dstKey,
        Promotion prom,
        Object srcKey) {
        _raum.withUpdate(new Callable<Integer>() {
                public Integer call() {
                    _mutationCounter++;
                    getOrMakeArgVarsAtKey(dstKey)
                        .promotions
                        .put(srcKey, prom.withSrcDst(
                                srcKey, dstKey));
                    return 0;
                }
            });
    }

    private ArrayList<PromotionPath> listPromotionPathsSub(
        Object dstKey, HashSet<Object> visited) {
        ArrayList<PromotionPath> result 
            = new ArrayList<PromotionPath>();
        if (!visited.contains(dstKey)) {
            visited.add(dstKey);

            IArgSpec sp = resolve(dstKey);
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
        IArgSpec sp = resolve(dstKey);
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

    private void checkForCycles() {
        ArrayList<Object> vertices = new ArrayList<Object>();
        vertices.addAll(_registry.keySet());
        INeighborhoodFunction<Object> neigh = new INeighborhoodFunction<Object>() {
                public List<Object> getNeighbors(Object k) {
                    ArgSpecVars v = _registry.get(k);
                    if (v == null) {
                        throw new RuntimeException(
                            "Missing arg-spec at key " 
                            + k.toString());
                    }
                    ArrayList<Object> dst = new ArrayList<Object>();
                    Object ind = v.argSpec.getIndirection();
                    if (ind != null) {
                        dst.add(ind);
                    }
                    dst.addAll(v.extensions);
                    return dst;
                }
            };
        List<Object> cycles = GraphUtils.findCycles(vertices, neigh);
        if (!cycles.isEmpty()) {
            String msg = "The following arg-spec keys are part of cycles:";
            for (Object k: cycles) {
                msg += " " + k.toString();
            }
            throw new RuntimeException(msg);
        }
    }

    public IDominates<IArgSpec> getArgSpecDominates() {
        return _argSpecDominates;
    }

    private void rebuildArgSpecs() {
        _allSamples = new HashSet<Object>();
        checkForCycles();
        
        // Clean up all the vars
        for (HashMap.Entry<Object, ArgSpecVars> kv : 
                 _registry.entrySet()) {
            ArgSpecVars x = kv.getValue();
            x.reset();
            x.argSpec.accumulateOwnSamples(_allSamples);
        }
        _argSpecDominates = new MemoizedDominates<IArgSpec>(
            new ArgSpecDominates(_allSamples));

        // Build indirections
        for (HashMap.Entry<Object, ArgSpecVars> kv : 
                 _registry.entrySet()) {
            ArgSpecVars v = kv.getValue();
            Object indirection = v.argSpec.getIndirection();
            ArgSpecVars dst = _registry.get(indirection);
            if (dst != null) {
                dst.referents.add(kv.getKey());
            }
        }

        // Build the arg specs
        for (HashMap.Entry<Object, ArgSpecVars> kv : 
                 _registry.entrySet()) {
            kv.getValue().build(
                kv.getKey(), this, _registry);
        }
    }

    public HashSet<Object> getAllSamples() {
        return _allSamples;
    }

    private void rebuildPromotionPaths() {
        for (HashMap.Entry<Object, ArgSpecVars> kv : 
                 _registry.entrySet()) {
            kv.getValue().promotionPaths = 
                listPromotionPaths(kv.getKey());
        }
    }

    public void rebuild() {
        long start = System.currentTimeMillis();
        // Set this at the top, so that recursive call will see it as being rebuilt.
        _rebuiltAt = _mutationCounter;

        rebuildArgSpecs();
        rebuildPromotionPaths();
        if (_settings.dispRebuildTime) {
            System.out.println(
                "Rebuilt EBMD registry in " + (System.currentTimeMillis() - start) 
                + " milliseconds");
        }
    }

    public boolean rebuildIfNeeded() {
        if (_mutationCounter == _rebuiltAt) {
            return false;
        } else {
            rebuild();
            return true;
        }
    }

    public int getMutationCounter() {
        return _mutationCounter;
    }

    public int getRebuiltAt() {
        return _rebuiltAt;
    }

    public Set<Object> getArgSpecKeys() {
        return _registry.keySet();
    }

    public ReadAndUpdateMachine getReadAndUpdateMachine() {
        return _raum;
    }
}
