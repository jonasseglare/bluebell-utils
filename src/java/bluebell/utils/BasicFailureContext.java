package bluebell.utils;

import clojure.lang.IFn;

public class BasicFailureContext extends AFailureContext {
    public IFn _is_error = null;
    private Object _error = null;

    synchronized private Object getOrSet(
        boolean set, Object new_value) {
        if (set) {
            _error = new_value;
        }
        return _error;
    }

    public BasicFailureContext(IFn is_error) {
        _is_error = is_error;
    }

    public boolean ok() {return getFailure() == null;}

    public Object getFailure() {return getOrSet(false, null);}

    public Object handleFailure(Object e) {
        assert(e != null); 
        getOrSet(true, e);
        return null;
    }

    public boolean isFailure(Object o) {
        return _is_error != null && (Boolean)(_is_error.invoke(o));
    }
}
