#define A
#define B
#undef A
#define D

#if A || B && (D == true) || !D != false // Boolean expression
public abstract class Maybe<T> {
  public abstract bool isSome();
}
#line 17 "another_file"
#else
public class Some<T> : Maybe<T> {
  private T v;
  public Some(T v) { this.v = v; }
  public override bool isSome() { return true; }
  public T get() { return v; }
}
#endif
public class None<T> : Maybe<T> {
  public override bool isSome() { return false; }
}

public class Foo {
  public static void Main() {
    string s =
"abc #if"; // #if FOO
  }
}
