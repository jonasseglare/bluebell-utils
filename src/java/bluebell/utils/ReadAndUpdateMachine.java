package bluebell.utils;

import bluebell.utils.ConcurrentStateMachine;
import bluebell.utils.IStateTransition;
import bluebell.utils.ReadAndUpdateMachineSettings;
import java.util.HashSet;
import java.lang.Thread;
import java.util.concurrent.Callable;
import java.lang.Runnable;

public class ReadAndUpdateMachine {
    private ReadAndUpdateMachineSettings _settings;
    private long _currentThread = 0;
    ConcurrentStateMachine<Integer> _state 
        = new ConcurrentStateMachine<Integer>(0);
    private HashSet<Long> _threads = new HashSet<Long>();
    private boolean _tryIt = false;

    class BeginUpdate implements IStateTransition<Integer> {
        private long _tid = 0;

        BeginUpdate(boolean tryIt, long tid) {
            _tid = tid;
            _tryIt = tryIt;
        }

        public Integer nextOrNull(Integer i) {
            if (1 <= i && _threads.contains(_tid)) {
                throw new RuntimeException(
                    "Trying to update on the same thread as you are already reading, which is an error. The state is " + i);
            }
            boolean perform = (i == 0)
                || ((i < 0) && _currentThread == _tid);
            if (perform) {
                _currentThread = _tid;
                return Integer.valueOf(i-1);
            } else if (_tryIt) {
                return i;
            } else {
                return null;
            }
        }
    }

    class EndUpdate implements IStateTransition<Integer> {
        private long _tid = 0;

        EndUpdate(long tid) {
            _tid = tid;
        }
        
        public Integer nextOrNull(Integer i) {
            if (_currentThread != _tid) {
                throw new RuntimeException(
                    "Trying to end update on the wrong thread");
            }
            if (0 <= i) {
                throw new RuntimeException(
                    "Calling endUpdate in the wrong state");
            }
            return i+1;
        }
    }

    class BeginRead implements IStateTransition<Integer> {
        private long _tid = 0;

        BeginRead(long tid) {
            _tid = tid;
        }

        public Integer nextOrNull(Integer i) {
            if (0 <= i) {
                _currentThread = _tid;
                if (_settings.debug) {
                    _threads.add(_tid);
                }
                return i+1;
            } else if (_currentThread == _tid) {
                return i-1;
            } else {
                return null;
            }
        }
    }

    class EndRead implements IStateTransition<Integer> {
        private long _tid = 0;

        EndRead(long tid) {
            _tid = tid;
        }
        
        public Integer nextOrNull(Integer i) {
            if (1 <= i) {
                if (_settings.debug) {
                    _threads.remove(_tid);
                }
                return i-1;
            } else if ((i <= -1) && (_currentThread == _tid)) {
                return i+1;
            } else if (i == 0) {
                throw new RuntimeException(
                    "Cannot end read when idle");
            } else {
                return null;
            }
        }
    }

    public ReadAndUpdateMachine() {
        _settings = new ReadAndUpdateMachineSettings();
    }

    public ReadAndUpdateMachine(
        ReadAndUpdateMachineSettings s) {
        _settings = s;
    }

    public static long getThreadId() {
        return Thread.currentThread().getId();
    }


    // The API
    public void beginUpdate() {
        _state.perform(
            new BeginUpdate(
                false,
                getThreadId()));
    }
    
    public boolean tryBeginUpdate() {
        OldAndNewState<Integer> oan = 
            _state.perform(
                new BeginUpdate(true, getThreadId()));
        return !oan.oldState.equals(oan.newState);
    }

    public void endUpdate() {
        _state.perform(
            new EndUpdate(getThreadId()));
    }

    public void beginRead() {
        _state.perform(new BeginRead(getThreadId()));
    }
    
    public void endRead() {
        _state.perform(new EndRead(getThreadId()));
    }

    public int getState() {
        return _state.getCurrentState();
    }

    static public<V> V uncheckedCall(Callable<V> c) {
        try {
            return c.call();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public<V> V withRead(Callable<V> c) {
        beginRead();
        try {
            return uncheckedCall(c);
        } finally {
            endRead();
        }
    }
    
    public<V> V withUpdate(Callable<V> c) {
        beginUpdate();
        try {
            return uncheckedCall(c);
        } finally {
            endUpdate();
        }
    }

    public<V> boolean withTryUpdate(Runnable r) {
        if (tryBeginUpdate()) {
            try {
                r.run();
            } finally {
                endUpdate();
            }
            return true;
        } else {
            return false;
        }
    }
}
