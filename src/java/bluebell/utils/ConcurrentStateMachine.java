package bluebell.utils;

import java.util.ArrayList;
import java.util.concurrent.ArrayBlockingQueue;
import bluebell.utils.IStateTransition;

public class ConcurrentStateMachine<StateType> {
    class TAP {
        private ArrayBlockingQueue<StateType> _result 
            = new ArrayBlockingQueue<StateType>(1);
        private IStateTransition<StateType> _transition;

        StateType tryNext(StateType current) {
            StateType next = _transition.nextOrNull(current);
            if (next != null) {
                _result.add(next);
            }
            return next;
        }

        public ArrayBlockingQueue<StateType> getResult() {
            return _result;
        }

        TAP(IStateTransition<StateType> t) {
            _transition = t;
        }
    }

    private StateType _currentState;
    private ArrayList<TAP> _pending = new ArrayList<TAP>();

    public ConcurrentStateMachine(StateType initialState) {
        if (initialState == null) {
            throw new RuntimeException("Initial state cannot be null");
        }
        _currentState = initialState;
    }

    private void resolveAsMuchAsPossible() {
        while (true) {
            boolean resolved = false;
            for (int i = 0; i < _pending.size(); i++) {
                StateType next = _pending.get(i).tryNext(
                    _currentState);
                if (next != null) {
                    resolved = true;
                    _currentState = next;
                    _pending.remove(i);
                    break;
                }
            }
            
            if (!resolved) {
                return;
            }
        }
    }

    public synchronized ArrayBlockingQueue<StateType> submit(
        IStateTransition<StateType> transition) {

        TAP tap = new TAP(transition);
        _pending.add(tap);
        resolveAsMuchAsPossible();
        return tap.getResult();
    }

    public StateType perform(
        IStateTransition<StateType> transition) 
        throws InterruptedException {
        return submit(transition).take();
    }

    public synchronized StateType getCurrentState() {
        return _currentState;
    }

    public synchronized int getPendingCount() {
        return _pending.size();
    }
}
