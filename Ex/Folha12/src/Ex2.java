import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.function.Function;

public class Ex2 implements Arg3<Double,Double,Double,Double>{

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		
		System.out.println("a: insert double");
		Function<Double,Double> a = x -> Math.pow(x, 2);
		double n = sc.nextDouble();
		System.out.print(n + "^2 is ");
		System.out.println(a.apply(n));
		System.out.println();
		
		System.out.println("b: insert 2 numbers");
		BiFunction <Double,Double,Double> b = (x,y) -> Math.max(x, y);
		n = sc.nextDouble();
		double m = sc.nextDouble();
		System.out.print("The biggest is ");
		System.out.println(b.apply(n, m));
		System.out.println();
		
		System.out.println("c: insert 3 numbers");
		n = sc.nextDouble();
		m = sc.nextDouble();
		double o = sc.nextDouble();
		System.out.print("The biggest is ");
		System.out.println(b.apply(b.apply(n, m),o));
		System.out.println();
		
		System.out.println("d: insert 2 numbers");
		int i = sc.nextInt();
		int j = sc.nextInt();
		Function <Integer,Function <Integer,Integer>> d = x -> (y -> x * y);
		System.out.print("Their product is ");
		System.out.println(d.apply(i).apply(j));
		System.out.println();
		
		System.out.println("e: insert a number");
		i = sc.nextInt();
		Function <Integer,String> even = x -> x % 2 == 0 ? "is" : "isn't";
		Function <Integer,String> grt10 = x -> x > 10 ? "is" : "isn't";
		BiFunction<Integer,Function<Integer,String>,String> e = (x,p) -> p.apply(x);
		System.out.println(i + " " + e.apply(i, even) + " even");
		System.out.println(i + " " + e.apply(i, grt10) + " greater than 10");
		System.out.println();
		
		sc.close();
	}

	@Override
	public Double apply(Double a, Double b, Double c) {
		BiFunction <Double,Double,Double> f = (x,y) -> Math.max(x, y);
		return f.apply(a, f.apply(b, c));
	}


}
