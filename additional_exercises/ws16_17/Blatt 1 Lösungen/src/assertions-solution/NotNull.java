
public class NotNull<T> {
    T o;
    public NotNull(T x){
        assert x != null;
        o = x;
    }
    public T get(){
        return o;
    }
}
