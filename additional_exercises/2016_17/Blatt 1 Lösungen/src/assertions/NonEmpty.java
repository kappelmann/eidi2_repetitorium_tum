import java.util.Collection;

public class NonEmpty<T extends Collection<?>> {
    T o;
    public NonEmpty(T x){
        // TODO assert that x is not empty
        o = x;
    }
    public T get(){
        return o;
    }
}
