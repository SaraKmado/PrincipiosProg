package Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class Impar implements Predicado<Integer>{

	@Override
	public boolean testar(Integer x) {
		return x % 2 == 1;
	}
	
	static <A> Collection <A> filtrar (Predicado<A> pred, Collection<A> colecao) {
		Collection <A> resultado = new ArrayList<A>();
		for (A a : colecao) {
			if(pred.testar(a))
				resultado.add(a);
		}
		return resultado;
	}
	
	public static void main(String[] args) {
		Collection<Integer> lista = Arrays.asList(1,2,3,4,5,6,7,8,9);
		System.out.println(filtrar(new Impar(),lista));
	}
}
