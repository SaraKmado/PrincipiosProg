package Test;

class User {
	private final int age;
	private final String name;
	private final Sex sex;

	public User(int age, String name, Sex sex) {
		this.age = age;
		this.name = name;
		this.sex = sex;
	}
	public int getAge() { 
		return age; 
	}
	public Sex getSex() { 
		return sex; 
	}
	public String getName() { 
		return name; 
	}
	
	public String toString() {
		return name + " " + age + " " + sex;
	}
}
enum Sex {MALE, FEMALE}
