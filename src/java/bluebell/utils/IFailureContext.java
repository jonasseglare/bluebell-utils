package bluebell.utils;

public interface IFailureContext {
    public boolean isFailure(Object o);
    public boolean ok();
    public Object getFailure();
    public Object handleFailure(Object o);
}
