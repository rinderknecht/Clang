using System;

public class Add {
  public static void Main(string[] args) {
    var a = f(x);
    var b = f(x,y,z);
    var c = f(x)(y);    // CAST, but call
    var d = f(x,y)(z,w);
    var e = f(x)(z,w);  // CAST, but call
    var f = f(x,y)(z);
    var g = (t)x;         // CAST
    var h = (t)(x);       // CAST
    var i = (f+f)(x);
    var j = (f)(x,y,z); // CAST, but call
    var k = (f+f)(x,y,z);
    var l = (t)(u)x;      // CAST
    var m = (int)* l;      // CAST
    var n = (t***[][,])x;  // CAST
    var o = (int[,,])(x);  // CAST
//    var p = f(x)y;      // parse error
    var q = ((f))(x);

    var r = ((t)x,y,z); //  CAST, ACCEPTE
    var s = (x,y)(z);   // ACCEPTE
    var t = (t)f(x);
//    var no = (t+t)x;      // parse error
  }
}
