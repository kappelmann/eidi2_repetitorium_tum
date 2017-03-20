import java.util.*;

public class IsSorted<C extends Collection<E>, E extends Comparable<? super E>> {
    C o;
    public IsSorted(C x){
        // TODO assert that x is sorted
        o = x;
    }
    public C get(){
        return o;
    }
}
