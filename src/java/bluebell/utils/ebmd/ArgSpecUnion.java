package bluebell.utils.ebmd;

import bluebell.utils.ebmd.IArgSpec;
import java.util.Set;
import java.util.ArrayList;
import java.util.HashSet;
import bluebell.utils.ParetoFrontier;
import java.util.Collections;
import java.util.Comparator;
import bluebell.utils.IDominates;
import bluebell.utils.ebmd.Registry;
import bluebell.utils.ReverseDominates;

public class ArgSpecUnion implements IArgSpec {
    HashSet<Object> _samples;
    HashSet<IArgSpec> _union;
    ArrayList<IArgSpec> _specs;

    public ArrayList<IArgSpec> getExtensionArgSpecs() {
        return _specs;
    }

    public Object getIndirection() {
        return null;
    }

    public boolean evaluate(Object x) {
        checkInitialized();
        for (IArgSpec s: _specs) {
            if (s.evaluate(x)) {
                return true;
            }
        }
        return false;
    }

    private void checkInitialized() {
        if (_samples == null || _union == null || _specs == null) {
            throw new RuntimeException(
                "Trying to use an ArgSpecUnion that has not been build");
        }
    }

    public void accumulateOwnSamples(Set<Object> dst) {}

    public void accumulateUnion(Set<IArgSpec> dst) {
        checkInitialized();
        dst.addAll(_union);
    }


    class ArgSpecWithMatchCount {
        public int matchCount = 0;
        public IArgSpec spec = null;

        ArgSpecWithMatchCount(IArgSpec s, int mc) {
            spec = s;
            matchCount = mc;
        }
    };

    private static int countMatches(IArgSpec sp, Set<Object> samples) {
        int counter = 0;
        for (Object s: samples) {
            if (sp.evaluate(s)) {
                counter++;
            }
        }
        return counter;
    }

    public boolean equivalentOnSamples(
        Set<Object> samples, IArgSpec other) {
        return other instanceof ArgSpecUnion;
    }


    public void build(
        Object thisKey, 
        Registry r,
        Set<IArgSpec> extensions) {
        _samples = new HashSet<Object>();
        _union = new HashSet<IArgSpec>();

        // Accumulate samples and arg-specs from extensions
        for (IArgSpec e: extensions) {
            e.accumulateUnion(_union);
        }

        // Remove redundant arg-specs that are covered by other ones
        ParetoFrontier<IArgSpec> frontier 
            = new ParetoFrontier<IArgSpec>(
                new ReverseDominates<IArgSpec>(
                    r.getArgSpecDominates()));
        for (IArgSpec s: _union) {
            frontier.insert(s);
        }

        // Sort the arg-specs so that the one covering most samples
        // comes first
        ArrayList<ArgSpecWithMatchCount> specs 
            = new ArrayList<ArgSpecWithMatchCount>();
        for (IArgSpec sp: frontier.getElements()) {
            specs.add(new ArgSpecWithMatchCount(
                    sp, countMatches(sp, _samples)));
        }
        Collections.sort(specs, new Comparator<ArgSpecWithMatchCount>() {
                public int compare(
                    ArgSpecWithMatchCount a, 
                    ArgSpecWithMatchCount b) {
                    if (a.matchCount < b.matchCount) {
                        return 1;
                    } else if (a.matchCount > b.matchCount) {
                        return -1;
                    }
                    return 0;
                }
            });

        // Build the list of arg-specs
        _specs = new ArrayList<IArgSpec>();
        for (ArgSpecWithMatchCount x: specs) {
            _specs.add(x.spec);
        }
    }
}
