using System;

abstract class I<A,B,C> {
  abstract public void F<T>(T t) where T: A;
  abstract public void G<T>(T t) where T: B;
}

class Cls: I<object,Cls,string> {
  public override void F<T>(T t) { }
  public override void G<T>(T t) { }
}

public class Test {
  public static void Main(string[] args) {
    Console.Write("Hello World!");
  }
}
