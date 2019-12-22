import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

public class Ex6 {
	
	public static void main(String[] args) {
		List<String> list =  Arrays.asList("ola","BoM dIa","como vai isso","oi");
		
		Consumer<String> c = new Consumer<String>() {
			public void accept(String t) {
				System.out.println(t);
			}
		};
		Function<String,Boolean> p = x -> x.length() > 3;
		
		processarElementos(list,String::toUpperCase,p,c);
	}

	static <T,U> void processarElementos (Iterable<T> it, Function<T,U> funcao, Function<T,Boolean> predicado, Consumer<U> consumidor){
		ArrayList<T> list = new ArrayList<>();
		for (T t : it) {
			if (predicado.apply(t))
				list.add(t);
		}
		
		ArrayList<U> newList = new ArrayList<U>();
		for (T t : list) {
			newList.add(funcao.apply(t));
		}
		
		Iterable<U> result = (Iterable<U>) (newList);
		for (U u : result) {
			consumidor.accept(u);
		}
	}
}
