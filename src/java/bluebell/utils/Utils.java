package bluebell.utils;

public class Utils {
    public static void check(boolean p, String msg) {
        if (!p) {
            throw new RuntimeException(msg);
        }
    }

    public static void error(String msg) {
        throw new RuntimeException(msg);
    }
}
