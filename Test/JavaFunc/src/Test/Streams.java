package Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Streams {

	public static void main(String[] args) {
		List<Integer> lista = Arrays.asList(1,2,3,4,5,6,7,8,9);
		Integer soma = lista.stream().filter(x -> x%2 == 0).map(x->x*x).reduce(0,(x,y) -> x+y);
		System.out.println(soma);
		
		IntStream arrayStream = Arrays.stream(new int[] {1,2,3,4,5,6,7,8,9});
		Stream<Integer> intStream = Stream.of(1,2,3,4,5,6,7,8,9);
		
		soma = arrayStream.filter(x -> x%2 == 0).map(x->x*x).reduce(0,(x,y) -> x+y);
		System.out.println(soma);
		
		soma = intStream.filter(x -> x%2 == 0).map(x->x*x).reduce(0,(x,y) -> x+y);
		System.out.println(soma);
		
		Stream<Integer> s = Stream.iterate(0, n -> n + 1);
	
		Integer f = s.limit(15).filter(x -> x < 10).reduce(0,(x,y) -> x+y);
		System.out.println(f);
		System.out.println(sum(new int[] {1,2,3,4,5,6,7,8,9}));
		
		Stream<Integer> mat = Stream.iterate(0, n -> n+1);
		System.out.println(mat.anyMatch(x -> x < 10));
		
		
		
		Integer sum = Stream.iterate(0, n -> n+1).map(x -> x*x).filter(x -> x%2 == 1).limit(1000).reduce(0, (x,y) -> x+y);
		System.out.println(sum);
		
		
		List<String> listString = Arrays.asList("1", "2", "3","4");
		List<Integer> listInt = listString.stream().map(Integer::valueOf).collect(Collectors.toList());
		System.out.println(listInt);
		
		mainUser();
	}

	private static void mainUser() {
		User u1 = new User(7, "Joaquin", Sex.MALE);
		User u2 = new User(12,"Yoooooo", Sex.FEMALE);
		User u3 = new User(14,"HeYo", Sex.MALE);
		User u4 = new User (15,"BOYSSSSSSSS",Sex.MALE);
		List<User> utilizadores = Arrays.asList(u1,u2,u3,u4);
		
		List<User> maisque10masc = utilizadores.stream().filter(user -> user.getAge() >= 10).filter(user -> user.getSex() == Sex.MALE).collect(Collectors.toList());
		for (User user2 : maisque10masc) {
			System.out.println(user2);
		}
		
		Map<Sex,List<User>> group = utilizadores.stream().filter(user -> user.getAge() >= 10).collect(Collectors.groupingBy(User::getSex));
		System.out.println(group.toString());
	}

	private static int sum(int[] is) {
		int sum = 0;
		for (int i : is) {
			sum += i;
		}
		return sum;
	}

}
