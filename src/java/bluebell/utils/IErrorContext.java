package bluebell.utils;

public interface IErrorContext {
    public boolean ok();
    public Object getError();
    public Object handleError(Object o);
    public boolean shouldCatch(Exception t);
}
