package bluebell.utils;

import bluebell.utils.IDominates;
import java.util.ArrayList;

public class ParetoFrontier<T> {
    private IDominates<T> _dom = null;
    private ArrayList<T> _elements = new ArrayList<T>();

    public ParetoFrontier(IDominates<T> dom) {
        _dom = dom;
    }

    public void insert(T x) {
        int n = _elements.size();
        for (int i = 0; i < n; i++) {
            if (_dom.dominates(_elements.get(i), x)) {
                return;
            }
        }
        int at = 0;
        for (int i = 0; i < n; i++) {
            T e = _elements.get(i);
            if (!_dom.dominates(x, e)) {
                if (i != at) {
                    _elements.set(at, e);
                }
                at++;
            }
        }
        if (_elements.size() == at) {
            _elements.add(x);
        } else {
            while (_elements.size() > at+1) {
                _elements.remove(_elements.size()-1);
            }
            _elements.set(at, x);
        }
        for (int i = 0; i < _elements.size(); i++) {
            if (_elements.get(i) == null) {
                throw new RuntimeException("Null detected");
            }
        }
    }

    public ArrayList<T> getElements() {
        return _elements;
    }
}
