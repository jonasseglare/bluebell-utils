package bluebell.utils.ebmd;

import bluebell.utils.ebmd.Promotion;
import bluebell.utils.ebmd.PromotionPath;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.HashSet;

public class ArgSpecVars {
    public HashMap<Object, Promotion> promotions 
        = new HashMap<Object, Promotion>();

    public ArrayList<PromotionPath> promotionPaths = null;

    public IArgSpec argSpec = null;

    public boolean built = false;

    public HashSet<Object> extensions = new HashSet<Object>();

    public void build(HashMap<Object, ArgSpecVars> vars) {
        if (!built) {
            for (Object e: extensions) {
                ArgSpecVars v = vars.get(e);
                if (v == null) {
                    throw new RuntimeException(
                        "Reference to undefined extension spec " 
                        + e.toString());
                } else {
                    v.build(vars);
                }
            }
            built = true;
        }
    }
}
