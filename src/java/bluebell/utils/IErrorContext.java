package bluebell.utils;

public interface IErrorContext {
    public boolean isError(Object o);
    public boolean ok();
    public Object getError();
    public Object handleError(Object o);
}
