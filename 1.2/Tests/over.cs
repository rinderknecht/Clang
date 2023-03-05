using System;

/// <summary>
/// From Eric Lippert's blog post at
/// http://blogs.msdn.com/b/ericlippert/archive/2006/04/05/odious-ambiguous-overloads-part-one.aspx
/// and
/// http://blogs.msdn.com/b/ericlippert/archive/2006/04/06/odious-ambiguous-overloads-part-two.aspx
/// </summary>

// With Mono C#:
// c3 explicit I1 101
// c3 class 102

// With Visual C#:
// c3 class 101
// c3 explicit I2 102

public interface I1<U> {
    void M(U i); // generic first
    void M(int i);
}

public interface I2<U> {
    void M(int i);
    void M(U i); // generic second
}

public class C3: I1<int>, I2<int> {
    void I1<int>.M(int i) {
        Console.WriteLine("c3 explicit I1 " + i);
    }
    void I2<int>.M(int i) {
        Console.WriteLine("c3 explicit I2 " + i);
    }
    public void M(int i) {
        Console.WriteLine("c3 class " + i);
    }
}

class Test {
    static void Main() {
        C3 c3 = new C3();
        I1<int> i1_c3 = c3;
        I2<int> i2_c3 = c3;
        i1_c3.M(101);
        i2_c3.M(102);
    }
}
