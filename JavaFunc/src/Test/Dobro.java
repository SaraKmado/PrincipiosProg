package Test;

import java.util.Scanner;

public class Dobro implements Funcao <Integer,Integer> {
	
	int dobro (int x) {
		return 2 * x;
	}
	
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		System.out.println(aplicar2Vezes (new Dobro(),in.nextInt()));
		in.close();
	}
	
	 static <A> A aplicar2Vezes (Funcao <A,A> f, A x) {
		return f.aplicar((f.aplicar(x)));
	}

	@Override
	public Integer aplicar(Integer x) {
		return dobro(x);
	}
}
