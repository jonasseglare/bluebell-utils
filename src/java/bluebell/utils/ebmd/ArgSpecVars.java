package bluebell.utils.ebmd;

import bluebell.utils.ebmd.Promotion;
import bluebell.utils.ebmd.PromotionPath;
import java.util.HashMap;
import java.util.ArrayList;

public class ArgSpecVars {
    public HashMap<Object, Promotion> promotions 
        = new HashMap<Object, Promotion>();

    public ArrayList<PromotionPath> promotionPaths = null;

    public IArgSpec argSpec = null;
}
