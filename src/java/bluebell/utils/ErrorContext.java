package bluebell.utils;

public class ErrorContext implements IErrorContext {
    private boolean _ok = true;
    private Object _error = false;

    public boolean ok() {
        return _ok;
    }

    public void reset() {
        _ok = true;
        _error = false;
    }

    public void setError(Object e) {
        _error = e;
        _ok = false;
    }

    public Object getError() {
        return _error;
    }
}
