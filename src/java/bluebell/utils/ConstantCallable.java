package bluebell.utils;

import java.util.concurrent.Callable;

public abstract class ConstantCallable<T> implements Callable<T> {
    private T _value;

    public abstract void run();

    public T call() {
        run();
        return _value;
    }
};
