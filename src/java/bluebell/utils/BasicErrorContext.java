package bluebell.utils;

import clojure.lang.IFn;

public class BasicErrorContext extends AErrorContext {
    public IFn _is_error = null;
    private Object _error = null;

    synchronized private Object getOrSet(
        boolean set, Object new_value) {
        if (set) {
            _error = new_value;
        }
        return _error;
    }

    public BasicErrorContext(IFn is_error) {
        _is_error = is_error;
    }

    public boolean ok() {return getError() == null;}

    public Object getError() {return getOrSet(false, null);}

    public Object handleError(Object e) {
        assert(e != null); 
        getOrSet(true, e);
        return null;
    }

    public boolean isError(Object o) {
        return _is_error != null && (Boolean)(_is_error.invoke(o));
    }
}
