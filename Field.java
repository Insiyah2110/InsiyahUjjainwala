import java.util.*;

interface Field extends Observer{
    void register();
    void unregister();
    void unregisterDead();
    
}