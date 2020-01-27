package Test;

import java.util.Arrays;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

public class Test {

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		Function<Integer,Integer> dobro = x -> 2 * x;
		Function<Integer,Integer> dobro2 = x -> {return 2 * x;};
		
		System.out.println(dobro.apply(sc.nextInt()) + " " + dobro2.apply(sc.nextInt()));
		
		BiFunction<Integer,Integer,Integer> soma = (x,y) -> x + y;
		System.out.println(soma.apply(sc.nextInt(), sc.nextInt()));
		
		BinaryOperator<Integer> somaBinOp = (x,y) -> x + y;
		System.out.println(somaBinOp.apply(sc.nextInt(), sc.nextInt()));
		
		String[] strings = {"Zcx","asd","asd"};
		Arrays.sort(strings,String::compareToIgnoreCase);
		//String::compareToIgnoreCase nao precisa de argumenros, compareToIgnoreCase() precisa
		//String::compareToIgnoreCase <=> (x,y) -> x.compareToignoreCase(y)
		for (String string : strings) {
			System.out.println(string);
		}
		
		sc.close();
	}

}
