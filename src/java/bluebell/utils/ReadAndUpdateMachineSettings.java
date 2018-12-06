package bluebell.utils;

public class ReadAndUpdateMachineSettings {
    public boolean debug = false;

    public static ReadAndUpdateMachineSettings debugSettings() {
        ReadAndUpdateMachineSettings dst = new ReadAndUpdateMachineSettings();
        dst.debug = true;
        return dst;
    }
}
