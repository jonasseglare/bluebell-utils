package bluebell.utils.ebmd;

import bluebell.utils.ebmd.Signature;
import bluebell.utils.ebmd.Impl;
import bluebell.utils.IDominates;


public class ImplDominatesSignature implements IDominates<Impl> {
    private IDominates<Object> _argSpecDominates;
    
    public ImplDominatesSignature(IDominates<Object> argSpecDominates) {
        _argSpecDominates = argSpecDominates;
    }
    
    public boolean dominates(Impl a, Impl b) {
        return a.getSignature().dominates(
            _argSpecDominates, 
            b.getSignature());
    }
}
