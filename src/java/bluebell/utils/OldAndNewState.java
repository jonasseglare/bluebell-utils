package bluebell.utils;

public class OldAndNewState<T> {
    public T oldState;
    public T newState;

    public OldAndNewState(T o, T n) {
        oldState = o;
        newState = n;
    }
} 
