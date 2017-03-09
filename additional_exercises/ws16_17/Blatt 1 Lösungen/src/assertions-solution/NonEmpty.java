import java.util.Collection;

public class NonEmpty<T extends Collection<?>> {
    T o;
    public NonEmpty(T x){
        assert !x.isEmpty();
        o = x;
    }
    public T get(){
        return o;
    }
}
