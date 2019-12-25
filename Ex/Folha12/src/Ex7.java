import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Ex7 {
	public static void main(String[] args) {
		//a
		int sum = Stream.iterate(1, n -> n+1).limit(999).reduce(0, (x,y) -> y + x);
		System.out.println(sum);

		//b
		List<Integer> lista = Stream.iterate(-9, x -> x + 1).limit(13).map(x -> x * x).sorted().distinct().collect(Collectors.toList());
		System.out.println(lista);
		
		//c
		//resposta: 1/3
		//boxed transforma de doublestream para stream, para poder ser coletado
		List<Double> list = IntStream.range(0,1001).asDoubleStream().map(x -> x/1000).map(x -> x*x).boxed().collect(Collectors.toList());
		double prev = list.get(0);
		double xcurr = 0.001;
		double xprev = 0.000;
		list.remove(0);
		
		double integ = 0;
		for (Double curr : list) {
			integ += area(xcurr,curr,xprev,prev);
			xprev += 0.001;
			xcurr += 0.001;
			prev = curr;
		}
		
		System.out.println(integ);
		
		//d
		list = DoubleStream.iterate(0, x -> x + 0.001).limit(1001).map(x -> x*x).boxed().collect(Collectors.toList());
		prev = list.get(0);
		xcurr = 0.001;
		xprev = 0.000;
		list.remove(0);
		
		integ = 0;
		for (Double curr : list) {
			integ += area(xcurr,curr,xprev,prev);
			xprev += 0.001;
			xcurr += 0.001;
			prev = curr;
		}
		
		System.out.println(integ);
	}
	
	
	static double area(double x1,double y1, double x2,double y2) {
		double highery = Math.max(y1, y2);
		double lowery = Math.min(y1, y2);
		double dist = Math.abs(x1 - x2);
		
		return lowery * dist + dist * (highery - lowery) / 2;
		
	}
}
