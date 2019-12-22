import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Ex4 {
	public static void main(String[] args) {
		List<String> list = Arrays.asList("1","2","3","4");
		List<Integer> result = listMap(Integer::parseInt,list); //b
		result.forEach(System.out::println);
	}

	//a
	static <A, B> List<B> listMap (Function<A, B> f, List<A>
	list){
		return list.stream().map(f).collect(Collectors.toList());
	}
}
