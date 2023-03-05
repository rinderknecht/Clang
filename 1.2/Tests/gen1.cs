using System;

class I<A,B> {
  public virtual void F<T>(T t) where T: A {}
}

class J<A>: I<A,int> {
  public override void F<T>(T t) { }
}

public class Test {
  public static void Main(string[] args) {
    Console.Write("Hello World!");
  }
}
