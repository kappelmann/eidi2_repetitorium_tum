import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

public class FunConstraints {

    public static <T, R> Function<T, R> addPrecondition(Predicate<T> p, Function<T, R> f){
        return x -> { assert p.test(x); return f.apply(x); };
    }

    public static <T, R> Function<T, R> addPostcondition(Predicate<R> p, Function<T, R> f){
        return x -> { R r = f.apply(x); assert p.test(r); return r; };
    }

    public static <T, R> Function<T, R> addConditions(Predicate<T> pre, Predicate<R> post, Function<T, R> f){
        // compose would be nicer, but currying is too painful
        return addPostcondition(post, addPrecondition(pre, f));
    }
}
