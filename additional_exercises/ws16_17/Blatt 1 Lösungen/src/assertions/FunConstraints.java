import java.util.function.Function;
import java.util.function.Predicate;

// Note: You need at least Java version 8 ;-)

public class FunConstraints {

    public static <T, R> Function<T, R> addPrecondition(Predicate<T> p, Function<T, R> f){
        // TODO return a new function that asserts that condition p holds on its argument before applying f on it
        return null;
    }

    public static <T, R> Function<T, R> addPostcondition(Predicate<R> p, Function<T, R> f){
        // TODO return a new function that asserts that condition p holds on the returned value after applying f on its argument
        return null;
    }

    public static <T, R> Function<T, R> addConditions(Predicate<T> pre, Predicate<R> post, Function<T, R> f){
        // TODO return a function that checks the pre and post conditions
        return null;
    }
}
