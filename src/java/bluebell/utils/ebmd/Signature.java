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

    private ArrayList<ArrayList<PromotionPath>> _cachedPaths;
    private ArgSpec _cachedJoint;

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
        IDominates<Object> argSpecDominates, 
        Signature other) {

        int n = _allData.length;
        if (n != other._allData.length) {
            throw new RuntimeException("Trying to compare signatures of different arities");
        }

        boolean maybeDom = false;
        for (int i = 0; i < n; i++) {
            Object ak = _allData[i];
            Object bk = other._allData[i];
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

    public void accumulateSamples(Registry reg, Set<Object> dst) {
        for (int i = 0; i < _allData.length; i++) {
            Object key = _allData[i];
            if (key != null) {
                ArgSpec as = reg.resolve(key);
                as.accumulateSamples(dst);
            }
        }
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
