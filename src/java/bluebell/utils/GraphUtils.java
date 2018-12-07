package bluebell.utils;

import bluebell.utils.INeighborhoodFunction;
import java.util.HashSet;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

public class GraphUtils {
    static private<T> void findCyclesSub(
        T start, INeighborhoodFunction<T> neigh, HashMap<T, Boolean> nodeStatus, ArrayList<T> cycleNodes) {
        Boolean status = nodeStatus.get(start);
        if (status == null) {
            nodeStatus.put(start, false);
            for (T y: neigh.getNeighbors(start)) {
                findCyclesSub(y, neigh, nodeStatus, cycleNodes);
            }
            nodeStatus.put(start, true);
        } else if (status == false) {
            cycleNodes.add(start);
        }
    }

    static public<T> List<T> findCycles(
        List<T> vertices,
        INeighborhoodFunction<T> neigh) {
        HashMap<T, Boolean> nodeStatus = new HashMap<T, Boolean>();
        ArrayList<T> cycleNodes = new ArrayList<T>();
        for (T v: vertices) {
            findCyclesSub(v, neigh, nodeStatus, cycleNodes);
        }
        return cycleNodes;
    }
}
