using System;

interface I<T> {}

    public class A<T,U> {
        // new A<U,T>
    }
    public class B1<V> : A<V,V> {}
    public class B2<V> : A<int,V> {}


public class HelloWorld
{
  public static void Main(string[] args) {
    Console.Write("Hello World!");
  }
}
