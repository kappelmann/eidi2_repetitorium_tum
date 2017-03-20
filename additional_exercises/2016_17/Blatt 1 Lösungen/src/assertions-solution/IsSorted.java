import java.util.*;

public class IsSorted<C extends Collection<E>, E extends Comparable<? super E>> {
    C o;
    public IsSorted(C x){
        List<E> sorted = new ArrayList<E>(x);
        Collections.sort(sorted);
        assert x.equals(sorted);
        o = x;
    }
    public C get(){
        return o;
    }
}
