package bluebell.utils.ebmd;

import java.util.Objects;
import java.util.ArrayList;
import java.util.Set;
import java.util.Arrays;
import bluebell.utils.IDominates;
import bluebell.utils.ebmd.Registry;

public class Signature {
    private Object[] _allData;
    private int _hashCode = 0;
    private IArgSpec[] _allArgSpecs;
    private ArrayList<ArrayList<PromotionPath>> _cachedPaths;
    private IArgSpec _cachedJoint;

    public Signature(Object[] argSpecKeys, Object jointPredicate) {
        int n = argSpecKeys.length;
        for (int i = 0; i < n; i++) {
            if (argSpecKeys[i] == null) {
                throw new RuntimeException(
                    "Null arg-spec keys are not allowed");
            }
        }
        _allData = Arrays.copyOf(argSpecKeys, n+1);
        _allData[n] = jointPredicate;
        _hashCode = Arrays.hashCode(_allData);
    }

    public Object[] getArgSpecKeys() {
        return Arrays.copyOf(_allData, _allData.length-1);
    }

    public int getArity() {
        return _allData.length-1;
    }

    public boolean dominates(
        IDominates<IArgSpec> argSpecDominates, 
        Signature other) {
        if (_allArgSpecs == null) {
            throw new RuntimeException("Signature has not been built");
        }

        int n = _allArgSpecs.length;
        if (n != other._allArgSpecs.length) {
            throw new RuntimeException("Trying to compare signatures of different arities");
        }

        boolean maybeDom = false;
        for (int i = 0; i < n; i++) {
            IArgSpec ak = _allArgSpecs[i];
            IArgSpec bk = other._allArgSpecs[i];
            if (argSpecDominates.dominates(ak, bk)) {
                maybeDom = true;
            } else if (argSpecDominates.dominates(bk, ak)) {
                return false;
            }
        }
        return maybeDom;
    }

    public boolean equals(Object other0) {
        if (other0 == null) {
            return false;
        }
        if (!(other0 instanceof Signature)) {
            return false;
        }
        Signature other = (Signature)other0;
        if (getArity() != other.getArity()) {
            return false;
        }

        int n = _allData.length;
        for (int i = 0; i < n; i++) {
            if (!(Objects.equals(_allData[i], other._allData[i]))) {
                return false;
            }
        }
        return true;
    }

    public int hashCode() {
        return _hashCode;
    }


    public void rebuild(Registry reg) {
        int n = getArity();
        _cachedPaths = new ArrayList<ArrayList<PromotionPath>>();
        for (int i = 0; i < n; i++) {
            _cachedPaths.add(reg.getPromotionPaths(_allData[i]));
        }
        _cachedJoint = _allData[n] == null? 
            null : reg.resolve(_allData[n]);

        int n1 = _allData.length;
        _allArgSpecs = new IArgSpec[n1];
        for (int i = 0; i < n1; i++) {
            Object k = _allData[i];
            _allArgSpecs[i] = k == null? null : reg.resolve(k);
        }
    }


    public PromotionPath[] evaluatePromotionPaths(
        Object[] args) {
        if (args.length != getArity()) {
            throw new RuntimeException(
                "Bad arity, expected " + getArity() 
                + " but got " + args.length);
        }
        int n = getArity();
        if (_cachedPaths == null) {
            throw new RuntimeException(
                "The arg-specs have not been built");
        }
        PromotionPath[] dst = new PromotionPath[n];
        for (int i = 0; i < n; i++) {
            PromotionPath best = PromotionPath.findBestMatch(
                _cachedPaths.get(i), args[i]);
            if (best == null) {
                return null;
            }
            dst[i] = best;
        }
        if (_cachedJoint != null 
            && !_cachedJoint.evaluate(
                clojure.lang.LazilyPersistentVector.create(args))) {
            return null;
        }
        return dst;
    }

    public String toString() {
        boolean space = false;
        String dst = "";
        for (int i = 0; i < _allData.length-1; i++) {
            if (space) {
                dst += " ";
            }
            space = true;

            dst += _allData[i].toString();
        }
        return dst;
    }
}
