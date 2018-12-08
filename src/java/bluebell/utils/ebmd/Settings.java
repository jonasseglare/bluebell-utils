package bluebell.utils.ebmd;

public class Settings {
    public boolean check = true;
    public boolean dispRebuildTime = true;
    
    public Settings debug() {
        check = true;
        return this;
    }

    public Settings release() {
        check = false;
        return this;
    }
}
