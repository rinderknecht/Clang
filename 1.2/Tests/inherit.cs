using System;

interface I<A,B,C> {
  void F<T>(T t) where T: A;
  void G<T>(T t) where T: B;
//  void H<T>(T t) where T: C;
}

class Cls: I<object,Cls,string> {
  public void F<T>(T t) { }
  public void G<T>(T t) where T: Cls { }
//  public void H<T>(T t) where T: string { }
}

public class Test {
  public static void Main(string[] args) {
    Console.Write("Hello World!");
  }
}
