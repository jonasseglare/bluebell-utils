package bluebell.utils;

public interface IErrorContext {
    public boolean ok();
    public void setError(Object e);
    public Object getError();
    public void reset();
}
