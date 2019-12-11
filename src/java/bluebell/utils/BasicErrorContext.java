package bluebell.utils;

public class BasicErrorContext implements IErrorContext {
    private Object _error = null;

    public boolean ok() {return _error == null;}
    public Object getError() {return _error;}

    public Object handleError(Object e) {
        assert(e != null); 
        _error = e;
        return null;
    }
    public boolean shouldCatch(Exception e) {
        return false;
    }
}
