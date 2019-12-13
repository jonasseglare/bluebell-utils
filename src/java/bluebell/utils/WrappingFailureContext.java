package bluebell.utils;

import clojure.lang.IFn;

public class WrappingFailureContext extends AFailureContext {
    private IFailureContext _inner;
    private IFn _handleFailure = null;
    private IFn _errorPred = null;

    public WrappingFailureContext(IFailureContext inner, IFn h, IFn e) {
        _inner = inner;
        _handleFailure = h;
        _errorPred = e;
    }

    public boolean ok() {
        return _inner.ok();
    }

    public Object getFailure() {
        return _inner.getFailure();
    }

    public Object handleFailure(Object o) {
        return _handleFailure == null?
            _inner.handleFailure(o) : 
            _handleFailure.invoke(_inner, o);
    }

    public boolean isFailure(Object o) {
        return _errorPred == null?
            _inner.isFailure(o) :
            (Boolean)_errorPred.invoke(_inner, o);
    }
}
