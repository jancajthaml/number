package com.github.jancajthaml.number;

abstract class Shared {

  static final double   AL2             = 0.301029995663981195;
  static final double   CL2             = 1.4426950408889633;
  static final double   CPI             = 3.141592653589793;
  static final double   ALT             = 0.693147180559945309;
  static final double   LOGE10            = 2.302585092994046;
  static final int    NIT             = 3;
  static final int    N30             = 1073741824;
  
  private static int    precision_current     = 0;
  
  static int        pointer           = 0;
  static int        round           = 0;
  static int        precision_digits_current  = 0;
  static int        precision_words       = 0;
  static int        precision_digits      = 0;
  static int        precision_digits_out    = 0; 
  static int        ellog10           = 0; 
  static int        mp2             = 0; 
  static int        mp21            = 0;
  static int        nw              = 0; 
  static Complex[]    uu1           = null; 
  static Complex[]    uu2           = null;

  static
  { setMaximumPrecision(300); }

  public static final void setMaximumPrecision(int p)
  {
    if(p == precision_current) return;
    
    p             = Math.min(Math.abs(p), 3600);
    precision_current     = p;
    pointer         = 7;
    round           = 1;
    precision_digits      = precision_current; 
    precision_digits_out    = 10000000; 
    ellog10         = 10 - precision_digits; 
    precision_words     = (int)((precision_digits /  7.224719896) + 1);
    mp2           = precision_words + 2;
    mp21            = mp2 + 1;
    precision_digits_current  = precision_digits_out; 
    nw            = precision_words;
    int n           = nw + 1;
    double ti         = 0.0;
    int j           = 0;
    int i           = 0;
    double t1         = 0.75 * n;
    int m           = (int)(CL2 * Math.log (t1) + 1.0 - 5.6843418860808015e-14);
    int mq          = m + 2;
    int nq          = (int)(Math.pow(2, mq));
    uu1           = new Complex[nq];
    uu1[0]          = new Complex(mq,0);
    int ku          = 1;
    int ln          = 1;

    for(j = 1; j<= mq; j++)
    {
      t1 = Math.PI / ln;      
      for (i = 0; i<= ln - 1; i++)
      {
        ti      = i * t1;
        uu1[i+ku] = new Complex(Math.cos (ti), Math.sin (ti));
      }

      ku  +=  ln;
      ln  <<= 1;
    }

    double tpn  = 0.0;
    int k     = 0;
    t1      = 0.75 * n;
    uu2     = new Complex[ mq+nq ];
    ku      = mq + 1;
    uu2[0]    = new Complex(mq,0);
    int mm    = 0;
    int nn    = 0;
    int mm1   = 0;
    int mm2   = 0;
    int nn1   = 0;
    int nn2   = 0;
    int iu    = 0;

    for (k = 2; k<= mq - 1; k++)
    {
      uu2[k-1]  = new Complex(ku,0);
      mm      = k;
      nn      = (int)(Math.pow(2, mm));
      mm1     = (mm + 1) / 2;
      mm2     = mm - mm1;
      nn1     = (int)(Math.pow(2, mm1));
      nn2     = (int)(Math.pow(2, mm2));
      tpn     = 2.0 * Math.PI / nn;
      
      for(j = 0; j<nn2; j++)
      {
        for(i = 0; i<nn1; i++)
        {
          iu      = ku + i + j * nn1;
          t1      = tpn * i * j;
          uu2[iu-1] = new Complex(Math.cos (t1), Math.sin (t1));
        }
      }
      
      ku += nn;
    
    }
  }

  static final double nint(double x)
  { return (x < 0)?Math.ceil(x - .5):Math.floor(x + .5); }
  
  static final double fSign(double a, double b)
  { return (b>=0 ? Math.abs(a) : -Math.abs(a));}

  static final double log10(double val)
  { return (Math.log(val)/LOGE10); }

  static final int precisionToSize(int precision)
  {
    int maxnw = (int)((precision / 7.224719896) + 4);
    return (maxnw<8)?8:maxnw;
  }

}
