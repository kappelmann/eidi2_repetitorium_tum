import java.util.function.Consumer;

public class Optional<T> {
    T o;
    private Optional(T x){
        o = x;
    }
    public static <T> Optional<T> ofNullable(T x){
        return new Optional<T>(x);
    }
    public void ifPresent(Consumer<? super T> c){
        if(o != null) c.accept(o);
    }
    public T orElse(T x){
        return o == null ? x : o;
    }
}