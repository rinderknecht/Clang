interface I<T> {}

class A<T> { 
  public virtual T M<U>(U u) where U : T {
    return u;
  }
}

class B<W> : A<I<W>> {
  public override I<W> M<U>(U u) {
    return u;
  }
}

class Bug {
  public static void Main () {}
}
