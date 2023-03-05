using System;

using whatami = System.Int32;
using imanint = System.Int32;

namespace BedTimeStory
{
  
//  public struct G<T> {}
//  public struct G<T,U> {}

  public delegate int del(object o);
  public delegate object del3(object o);
  public struct sa { public del x; }
  public struct sb { public int x; }

  public struct whatthehell { int x; }

  public unsafe struct Horror {
    public static del operator *(Horror a, Horror b) { throw new System.NotImplementedException(); }
  }

  public unsafe struct Trauma {
    public static sa operator *(Trauma a, int b) { throw new System.NotImplementedException(); }
  }

  public unsafe delegate int* t1(int* p);

  public struct Syndrome {
    public static bool operator * (bool b, Syndrome s) { throw new System.NotImplementedException(); }
    public static bool operator * (Syndrome s, bool b) { throw new System.NotImplementedException(); }
    public static bool operator + (bool b, Syndrome s) { throw new System.NotImplementedException(); }
    public static bool operator + (Syndrome s, bool b) { throw new System.NotImplementedException(); }

    public static implicit operator bool (Syndrome s) { throw new System.NotImplementedException(); }
    //public static implicit operator bool (Trauma t) { throw new System.NotImplementedException(); } // error
  }

  public unsafe struct Damn : IDisposable {
    public static implicit operator Damn (int*[] s) { throw new System.NotImplementedException(); }
    public void Dispose () { throw new System.NotImplementedException(); }
  }

  class MainClass
  {
     public static void Main (string[] args)
     {
       string s = "\x1b[1;31mRouge";
       System.Console.WriteLine(s);

       using (var f = System.IO.File.Open("/tmp/a", System.IO.FileMode.OpenOrCreate)) {
         f.Write (new byte[] { 42 }, 0, 1);
         // f = null; // error
       }

       using (var f = System.IO.File.Open("/tmp/a", System.IO.FileMode.OpenOrCreate)) {
         f.Write (new byte[] { 42 }, 0, 1);
         // f = null; // error
       }
       
       // using (var e = new System.Collections.Generic.List<int> { 1, 2, 3 }.GetEnumerator()) {
       //   if (e.MoveNext()) {
       //     var x = e.Current;
       //   }
       // }

      using (System.IO.File.Open("/tmp/a", System.IO.FileMode.OpenOrCreate)) {
        // . | . | .
        // . | X | .
        // . | . | .
      }

      System.IO.FileStream fs = System.IO.File.Open("/tmp/a", System.IO.FileMode.OpenOrCreate);
      using (fs) {
        fs = null;
      }

      System.IO.FileStream fy = System.IO.File.Open("/tmp/a", System.IO.FileMode.OpenOrCreate);
      using (System.IO.FileStream fx = null, z = null, fw = null) {
        fs = null;
      }

      //unsafe {
      //  using (int*[] id = (int*[])0) {
      //  }
      //}

      //using (System.IO.FileStream fx) { } // error

      unsafe {
        object flop = null;
        Syndrome blop;
        bool bresult = flop is int*[];
//        if (flop is int***[,,,][][,,] + blop) { }

        object o = null;
        var x = (int***[][])o;
//        var y = (G<G<int***[][][]>***[][][],G<int***[][][]>***[][][]>)o;
        int*** ptr;
        var w = ((int)***ptr);
        
        /*
        del*** ptr2;
        var x2 = (int***[][])(o);
        var w2 = ((del)***ptr2) (o);
        var w2bis = ((del)***(ptr2)) (o);
  
        // Dereference and cast
        del* ptr3;
        var w3 = ((del) * (ptr3)) (o);
        */

        int*** ptr2;
        var x2 = (int***[][])(o);
        var w2 = ((int)* **ptr2);// (o)
        var w2bis = ((int)* **(ptr2));// (o);
        Tuple.Create (x2, w2, w2bis);

        // Dereference and cast
        int* ptr3;
        var w3 = ((int)* (ptr3));// (o);
  
        // Multiplication + appel
        Horror whatami = new Horror();
        Horror ptr4 = new Horror();
        whatami* ptr6 = (whatami*)0;
        var w4 = ((whatami) * (ptr4)) (o);
        var w5 = (whatami * ptr4) (o); // multiply
        var w6 = (whatami * )(ptr6); // cast

        Horror**** ptr7 = (Horror****)0;
        whatami***** ptr8 = (whatami*****)0;
        var w7 = (whatami ***** ptr7) (o); // multiply + dereference
        var w8 = (whatami ***** )(ptr8); // cast

//        t1 one = (t1)((whatami * two) => two);
        ////t1 three = (t1)((whatami * whatami) => (whatami * whatami));
//        t1 five = (t1)(six => six);
        // (f)(a, b)
        // (t)(

        ////Foo((whatami * whatami, imanint imanint) => Foo(imanint * imanint, whatami whatami));

        var i = (new int***[5][][][]);
//        var j = (new int***[5][6][7][8]);

        int* star = (int*)0;
        object abject = null;
        var disgust = (int***[,,,][,,][,])abject;

        if (o is int***[]) { }
        int** foobar;
        var truc = *foobar;
      }
     }

    public void fWesdrehg() {
      var whatami = 42;
      //var expr = whatami * ;
    }

    public delegate del del2(del a);

    public class Insane {
      public static del2 operator +(Insane x, Insane y) { throw new System.NotImplementedException (); }
    }

    public unsafe struct a {
      public static explicit operator a(int i) { throw new System.NotImplementedException (); }
      public static explicit operator f*(a a) { throw new System.NotImplementedException (); }
    }
    public struct f {
      public static explicit operator f(a a) { throw new System.NotImplementedException (); }
    }

    public static void CastCall1() {
      unsafe {
        int b = 42;
        del a = null;
        del2 f = null;
        del2[] g = null;
        var x = (f)(a); // WRONG: Not a cast
        var cast1 = ((f)(a)(b)); // ???: (a) is a cast?
        var cast2 = ((f*)(a)(b));
        var cast3 = ((f*)a(b)); // a calls b, then cast as (f*);
        var cast4 = ((f[])(object)(b));
        var cast5 = ((System.Int32)(object)(b));
        var call0 = ((g[1])(a)(b));
        var call1 = (f(a)(b));
        var call2 = (((f))(a)(b));
        var call3 = ((f+f)(a)(b));
//        del foo = (delegate(object x) { return 42; });
      }
    }

    public static class C {
      public delegate del h(del a);

      public static void CastCall2() {
        int b = 42;
        del a = null;
        h f = null;
        h x = f;
        var call2 = (((f))(a)(b));
        var call3 = (((h)x)(a)(b));
      }
    }

    public void Foo() { }

    public static void ImHungryA() {
      unsafe {
        sb** ptr5;
        // var w5 = ((sb) ** (ptr5));// (o); // error
      }
    }

    public static void ImHungryB() {
      unsafe {
          void* x;
        int* ptr5;
        Trauma deld;
        var w5b = ((deld) ** (ptr5));// (o);
      }
    }

    public static void MadnessDereference() {
      unsafe {
        var iamnot = 3.14;
        whatami* ptr5;
        var w5 = ((int) *(ptr5));
        var w5bis = ((int) (* (ptr5)));
        // var w6 = ((whatami) *(ptr5)); // error
        var w6bis = ((whatami) (* (ptr5)));
        // var w7 = ((System.Int32) *(ptr5)); // error
        var w7bis = ((System.Int32) (* (ptr5)));
      }
    }

    public static void MadnessMultiply() {
      unsafe {
        var whatami = 3.14;
        int ptr5 = 42;
        var w5 = ((whatami) * (ptr5));

//        System.Func<object, object> otherami = x => x;
        int* ptr6;
        var w5bis = (otherami (* (ptr6)));
        //var w5bis = ((otherami) (* (ptr6))); // error
      }
    }

    public static unsafe void DerefUnderstoodAsMultiply() {
      imanint** ptra = (imanint**)0;
      var wc = ((imanint) (** (ptra))); // does a dereference
      //var wa = ((imanint) ** (ptra)); // tries to do a multiply, gives an error because imanint is a type.
    }

    public static void Another1() {
      whatami[] foo;
    }

    public static void Another2() {
      del[,] whatami = null;
      whatami[42,345](123);
    }

    public static void Yeti1() {
      int x = 123;
      del3 maybenot = null;
      var y = (whatami)(x);
      var z = y;
    }

    public static void Yeti2() {
      int x = 123;
      del3 whatami = null;
      var y = (whatami)(x);
      var z = y;
    }
  }
}
