import java.util.Arrays;
import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.stream.Stream;
import java.util.stream.Stream.Builder;

public class Ex8 {

	//b
	public static void main(String[] args) {
		//Vetor representado por 2 streams de 4 elementos de formato (x,y,z)

		Stream<Double> as = Arrays.asList(0.0,1.1,2.2).stream();

		Stream<Double> bs = Arrays.asList(5.0,1.0,0.0).stream();

		double prod = zipWith((x,y) -> x * y, as, bs).reduce(0.0,(x,y) -> x + y);
		System.out.println(prod);
	}

	//a
	static <A,B,C> Stream<C> zipWith (BiFunction <A,B,C> f, Stream<A> as, Stream<B> bs){

		Iterator<A> ia = as.iterator();
		Iterator<B> ib = bs.iterator();

		Builder<C> bob = Stream.builder();

		while (ia.hasNext() && ib.hasNext()) {
			bob.accept(f.apply(ia.next(), ib.next()));
		}
		return bob.build();
	}
	
	static <A,B> Stream<Pair<A,B>> zip (Stream<A> as, Stream
			<B> bs){
		return zipWith((a,b) -> new Pair<A,B>(a,b),as,bs);
	}


	final static class Pair<T1,T2> {
		final T1 a; 
		final T2 b;
		Pair(T1 a, T2 b) {
			this.a = a; 
			this.b = b;
		}
	}

}
