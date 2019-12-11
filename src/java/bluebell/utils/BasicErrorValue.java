package bluebell.utils;

import clojure.lang.Keyword;

public class BasicErrorValue {
    private Keyword _k;
    private Object _data;
    private String _msg;

    public BasicErrorValue(Keyword k, String msg, Object data) {
        _k = k;
        _data = data;
        _msg = msg;
    }

    public Keyword getKey() {
        return _k;
    }

    public Object getData() {
        return _data;
    }

    public Object getMessage() {
        return _msg;
    }
}
