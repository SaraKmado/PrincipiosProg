import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public class Ex5 {

	public static void main(String[] args) {
		System.out.println("Ordering string by length");
		List<String> list = Arrays.asList("boas","ola","yo","yoooooo","oi","hey");
		System.out.println("  With lambda expressions:");
		a(list);
		System.out.println();
	}

	private static void a(List<String> list) {
		list.sort(Comparator.comparingInt(String::length));
		for (String string : list) {
			System.out.print(string + " ");
		}
	}
}
