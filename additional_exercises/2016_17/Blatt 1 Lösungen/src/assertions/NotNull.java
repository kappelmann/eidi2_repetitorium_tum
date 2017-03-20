
public class NotNull<T> {
    T o;
    public NotNull(T x){
        // TODO assert that x is not null
        o = x;
    }
    public T get(){
        return o;
    }
}
