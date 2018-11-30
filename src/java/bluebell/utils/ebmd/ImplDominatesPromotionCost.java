package bluebell.utils.ebmd;

import bluebell.utils.ebmd.Signature;
import bluebell.utils.ebmd.Impl;
import bluebell.utils.IDominates;


public class ImplDominatesPromotionCost implements IDominates<Impl> {
    public boolean dominates(Impl a, Impl b) {
        return a.getPromotionCost() < b.getPromotionCost();
    }
}
