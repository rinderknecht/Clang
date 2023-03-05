#if X
/*
#else
/* */
class Q { }
#endif
  # line   default    // Comment
#line 123 // Changing the line
  #line 456 "file.cs" // Changing the name
public abstract class Maybe<T> {
  public abstract bool isSome();
}
#define NORMAL
public class Some<T> : Maybe<T> {
  private T v;
  public Some(T v) { this.v = v; }
  public override bool isSome() { return true; }
  public T get() { return v; }
}
#if NORMAL
public class None<T> : Maybe<T> {
  public override bool isSome() { return false; }
}
#else
public class None<T> : Maybe<T> {
  public override bool isSome() { return true; }
}
#endif
public class Foo {
  public static void Main() {
    string s =
"abc #if"; // #if FOO
  }
}
