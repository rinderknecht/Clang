


public abstract class Maybe<T> {
  public abstract bool isSome();
}
  #  region Let the fun begin!
public class Some<T> : Maybe<T> {
  private T v;
  public Some(T v) { this.v = v; }
  public override bool isSome() { return true; }
  public T get() { return v; }
}
      # endregion Too bad...

public class None<T> : Maybe<T> {
  public override bool isSome() { return false; }
}

public class Foo {
  public static void Main() {
    string s =
"abc #if"; // #if FOO
  }
}
