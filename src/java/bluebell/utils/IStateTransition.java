package bluebell.utils;

public interface IStateTransition<StateType> {
    public StateType nextOrNull(StateType current);
}
