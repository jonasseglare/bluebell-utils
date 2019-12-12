package bluebell.utils;

import clojure.lang.IFn;

public class WrappingErrorContext extends AErrorContext {
    private IErrorContext _inner;
    private IFn _handleError = null;
    private IFn _errorPred = null;

    public WrappingErrorContext(IErrorContext inner, IFn h, IFn e) {
        _inner = inner;
        _handleError = h;
        _errorPred = e;
    }

    public boolean ok() {
        return _inner.ok();
    }

    public Object getError() {
        return _inner.getError();
    }

    public Object handleError(Object o) {
        return _handleError == null?
            _inner.handleError(o) : 
            _handleError.invoke(_inner, o);
    }

    public boolean isError(Object o) {
        return _errorPred == null?
            _inner.isError(o) :
            (Boolean)_errorPred.invoke(_inner, o);
    }
}
