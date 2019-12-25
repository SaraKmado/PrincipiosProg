import java.util.function.BiFunction;
import java.util.function.Function;

public class Ex9 {

	public static void main(String[] args) {
		BiFunction<Integer, Integer, Integer> sum = (x,y) -> x+y;
		System.out.println(sum.apply(5, 9));
		System.out.println(curry(sum).apply(5).apply(9));
	}
	
	static <T1,T2,R> Function<T1,Function<T2,R>> curry (BiFunction<T1,T2,R> f) {
		return x -> (y -> f.apply(x, y));
	}
}
