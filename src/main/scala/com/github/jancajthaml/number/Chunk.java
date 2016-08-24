package com.github.jancajthaml.number;

class Chunk {

  double a = 0.0;
  int n = 0;

  Chunk() {}

  Chunk(double A) {
    this(A, 0);
  }

  Chunk(double A, int N) {
    a = A;
    n = N;
  }

  double value() {
    return a * Math.pow(2, n);
  }

  static void dpdec(Chunk a, Chunk b) {
    if (a.a != 0.0) {
      double t1 = 0.3010299956639812 * a.n + (Math.log(Math.abs(a.a)) / 2.302585092994046);
      b.n = (int) t1;

      if (t1 < 0.0) {
        b.n -= 1;
      }
      double sig = Math.pow(10.0, (t1 - b.n));
      b.a = (a.a >= 0 ? Math.abs(sig) : -Math.abs(sig));
    } else {
      b.a = 0.0;
      b.n = 0;
    }
  }

}