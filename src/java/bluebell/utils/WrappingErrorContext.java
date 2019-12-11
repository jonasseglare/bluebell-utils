package bluebell.utils;

import clojure.lang.IFn;

public class WrappingErrorContext implements IErrorContext {
    private IErrorContext _inner;
    private IFn _handleError = null;
    private IFn _catchPred = null;

    public WrappingErrorContext(IErrorContext inner, IFn h, IFn c) {
        _inner = inner;
        _handleError = h;
        _catchPred = c;
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

    public boolean shouldCatch(Exception e) {
        return _catchPred == null? 
            _inner.shouldCatch(e) :
            ((Boolean)_catchPred.invoke(_inner, e));
    }
}
