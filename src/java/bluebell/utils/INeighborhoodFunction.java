package bluebell.utils;

import java.util.List;

public interface INeighborhoodFunction<T> {
    public List<T> getNeighbors(T x);
}
