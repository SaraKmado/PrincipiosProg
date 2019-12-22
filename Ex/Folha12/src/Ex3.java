import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.function.Function;

public class Ex3 {

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		ArrayList<Integer> list = new ArrayList<Integer>();
		System.out.println("a");
		Function<List<Integer>,List<Integer>> a = x -> concatenate(x,Arrays.asList(1,2,3));
		System.out.println("Insert integer numbers. One per line, empty line is end");
		String curr = sc.nextLine();
		while(curr != null && !curr.equals("")) {
			list.add(Integer.parseInt(curr));
			curr = sc.nextLine();
		}
		list = (ArrayList<Integer>) a.apply(list);
		System.out.println("Result:");
		list.forEach(System.out::println);
		System.out.println();
		
		System.out.println("b");
		Function<List<Integer>,List<Integer>> b = x -> concatenate(Arrays.asList(1,2,3),x);
		list = new ArrayList<Integer>();
		System.out.println("Insert integer numbers. One per line, empty line is end");
		curr = sc.nextLine();
		while(curr != null && !curr.equals("")) {
			list.add(Integer.parseInt(curr));
			curr = sc.nextLine();
		}
		list = (ArrayList<Integer>) b.apply(list);
		System.out.println("Result:");
		list.forEach(System.out::println);
		System.out.println();
		
		sc.close();
	}

	static <T> List<T> concatenate(List<T> l1, List<T> l2) {
		List<T> result = new ArrayList<>();
		result.addAll(l1);
		result.addAll(l2);
		return result;
	}

}
