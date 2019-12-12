package bluebell.utils;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import java.util.concurrent.Callable;
import clojure.lang.RT;

public abstract class AErrorContext implements IErrorContext, IFn {
    private Object invokeSub(Callable c) {
        if (ok()) {
            try {
                Object y = c.call();
                if (isError(y)) {
                    return handleError(y);
                } else {
                    return y;
                }
            } catch (Exception e) {
                if (isError(e)) {
                    return handleError(e);
                } else {
                    throw new RuntimeException(e);
                }
            }
        } else {
            return null;
        }
    }



    public Object applyTo(ISeq arglist) {
        if (RT.seq(arglist) == null) {
            return invoke();
        } else {
            return invokeSub(new Callable() {
                    public Object call() {
                        return ((IFn)arglist.first()).applyTo(
                            arglist.next());
                    }
                });
        }
    }

    public void run() {}

    public Object call() {
        return null; //invoke();
    }

    public Object reportError(Object o) {
        return ok()? handleError(o) : null;
    }

    public Object invoke() {return getError();}






    /* 

       GENERATED CODE

     */

public Object invoke(Object f) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke();}});}

public Object invoke(Object f, Object arg0) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0);}});}

public Object invoke(Object f, Object arg0, Object arg1) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);}});}

public Object invoke(Object f, Object arg0, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object...  restArgs) {return invokeSub(new Callable() {public Object call() {return ((IFn)f).invoke(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, restArgs);}});}


}
