package bluebell.utils.ebmd;

import bluebell.utils.ebmd.Promotion;
import bluebell.utils.ebmd.PromotionPath;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import bluebell.utils.ebmd.ArgSpecVarsVisitor;
import bluebell.utils.IDominates;

public class ArgSpecVars {
    public HashMap<Object, Promotion> promotions 
        = new HashMap<Object, Promotion>();
    public ArrayList<PromotionPath> promotionPaths = null;
    public IArgSpec argSpec = null;
    public boolean built = false;
    public HashSet<Object> extensions = new HashSet<Object>();
    public HashSet<Object> referents = new HashSet<Object>();

    public void reset() {
        built = false;
        referents.clear();
    }

    public static ArgSpecVars trackIndirections(
        HashMap<Object, ArgSpecVars> reg,
        Object key, ArgSpecVarsVisitor v) {
        while (true) {
            ArgSpecVars vars = reg.get(key);
            if (vars == null) {
                if (key == null) {
                    throw new RuntimeException(
                        "No arg-spec with null key");
                } else {
                    throw new RuntimeException("No arg-spec with key " 
                        + key.toString());
                }
            } else {
                IArgSpec as = vars.argSpec;
                if (v != null) {
                    v.visit(vars);
                }
                if (as == null) {
                    throw new RuntimeException(
                        "Missing arg-spec at key " 
                        + key.toString());
                } else {
                    key = as.getIndirection();
                    if (key == null) {
                        return vars;
                    }
                }
            }
        }
    }

    // Get the extensions of not only this object, but also its parents
    private void getAllExtensions(
        HashMap<Object, ArgSpecVars> vars, Set<Object> extKeys) {
        extKeys.addAll(extensions);
        for (Object ref: referents) {
            vars.get(ref).getAllExtensions(vars, extKeys);
        }
    }

    public void build(
        Object thisKey, 
        IDominates<IArgSpec> dom,
        HashMap<Object, ArgSpecVars> vars) {
        if (!built) {
            HashSet<Object> extKeys = new HashSet<Object>();
            getAllExtensions(vars, extKeys);
            
            HashSet<IArgSpec> exts = new HashSet<IArgSpec>();

            // First build all its dependencies
            for (Object e: extKeys) {
                ArgSpecVars v = trackIndirections(vars, e, null);
                v.build(e, dom, vars);
                exts.add(v.argSpec);
            }
            
            argSpec.build(thisKey, dom, exts);
            built = true;
        }
    }
}
