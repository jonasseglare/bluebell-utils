package bluebell.utils;

import clojure.lang.Keyword;

public class BasicFailureValue {
    private Keyword _k;
    private Object _data;
    private String _msg;

    public BasicFailureValue(Keyword k, String msg, Object data) {
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

    public String toString() {
        return "BasicFailureValue(key=" + _k.toString() + ", msg=" + 
            _msg + ", data=" + _data.toString() + ")";
    }
}
