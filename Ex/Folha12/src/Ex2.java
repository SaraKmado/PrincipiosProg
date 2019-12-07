import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.function.Function;

public class Ex2 implements Arg3<Double,Double,Double,Double>{

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		System.out.println("a: insert double");
		Function<Double,Double> a = x -> Math.pow(x, 2);
		System.out.println(a.apply(sc.nextDouble()));
		
		System.out.println("b: insert 2 numbers");
		BiFunction <Double,Double,Double> b = (x,y) -> Math.max(x, y);
		System.out.println(b.apply(sc.nextDouble(), sc.nextDouble()));
		
		System.out.println("c: insert 3 numbers");
		System.out.println(new Ex2().apply(sc.nextDouble(), sc.nextDouble(), sc.nextDouble()));
		
		System.out.println("d: insert 4 numbers");
		System.out.println();
		sc.close();
	}

	@Override
	public Double apply(Double a, Double b, Double c) {
		BiFunction <Double,Double,Double> f = (x,y) -> Math.max(x, y);
		return f.apply(a, f.apply(b, c));
	}

}
