#define FOO
#line zozo
#if false // If Copy
A
  #if false
  B
  #elif true
  C
  #elif true
  Cbis
  #else
  D
  #endif
#else
E
  #if false
  F
  #elif FOO
  G
  #elif true
  Gbis
  #else 
  H
  #endif
#endif

public abstract class Maybe<T> {
  public abstract bool isSome();
}

public class Some<T> : Maybe<T> {
  private T v;
  public Some(T v) { this.v = v; }
  public override bool isSome() { return true; }
  public T get() { return v; }
}

public class None<T> : Maybe<T> {
  public override bool isSome() { return false; }
}

public class Foo {
  public static void Main() {
    string s =
"abc #if"; // #if FOO
  }
}
