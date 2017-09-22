public class Arith {

  public static void main(String[] args) {
    Expr ex1 = new Add(new Add(new Lit(3), new Lit(4)), new Neg(new Lit(5)));
    System.out.println(ex1.pretty() + " = " + ex1.eval());
    
    Expr ex2 = new Mul(ex1, ex1);
    System.out.println(ex2.pretty() + " = " + ex2.eval());
  }

}

interface Expr {

  public int eval();
  public String pretty();

}

class Lit implements Expr {
  
  int value;
  
  public Lit(int v) {
    value = v;
  }

  public int eval() {
    return value;
  }

  public String pretty() {
    return Integer.toString(value);
  }

}

class Neg implements Expr {
  
  Expr sub;
  
  public Neg(Expr s) {
    sub = s;
  }

  public int eval() {
    return -sub.eval();
  }

  public String pretty() {
    return "-" + sub.pretty();
  }

}

class Add implements Expr {

  Expr left;
  Expr right;

  public Add(Expr l, Expr r) {
    left = l;
    right = r;
  }

  public int eval() {
    return left.eval() + right.eval();
  }

  public String pretty() {
    return "(" + left.pretty() + " + " + right.pretty() + ")";
  }

}

class Mul implements Expr {

  Expr left;
  Expr right;

  public Mul(Expr l, Expr r) {
    left = l;
    right = r;
  }

  public int eval() {
    return left.eval() * right.eval();
  }

  public String pretty() {
    return "(" + left.pretty() + " * " + right.pretty() + ")";
  }

}
