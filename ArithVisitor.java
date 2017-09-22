import java.util.*;

public class ArithVisitor {

  public static void main(String[] args) {
    
    Expr ex1 = new Add(new Add(new Lit(3), new Lit(4)), new Neg(new Lit(5)));
    
    String pretty = ex1.accept(new PrettyVisitor());
    int result = ex1.accept(new EvalVisitor());

    System.out.println(pretty + " = " + result);

    Set<Integer> lits = ex1.accept(new LitsVisitor());
    System.out.println("lits: " + lits);

  }

}

interface ExprVisitor<Result> {

  public Result visitLit(Lit expr);
  public Result visitNeg(Neg expr);
  public Result visitAdd(Add expr);

}

interface Expr {

  public <Result> Result accept(ExprVisitor<Result> visitor);

}

class Lit implements Expr {
  
  int value;
  
  public Lit(int v) {
    value = v;
  }

  public <Result> Result accept(ExprVisitor<Result> visitor) {
    return visitor.visitLit(this);
  }

}

class Neg implements Expr {
  
  Expr sub;
  
  public Neg(Expr s) {
    sub = s;
  }
  
  public <Result> Result accept(ExprVisitor<Result> visitor) {
    return visitor.visitNeg(this);
  }

}

class Add implements Expr {

  Expr left;
  Expr right;

  public Add(Expr l, Expr r) {
    left = l;
    right = r;
  }

  public <Result> Result accept(ExprVisitor<Result> visitor) {
    return visitor.visitAdd(this);
  }

}

class EvalVisitor implements ExprVisitor<Integer> {

  public Integer visitLit(Lit expr) {
    return expr.value;
  }
  
  public Integer visitNeg(Neg expr) {
    return -expr.sub.accept(this);
  }

  public Integer visitAdd(Add expr) {
    return expr.left.accept(this) + expr.right.accept(this);
  }

}

class PrettyVisitor implements ExprVisitor<String> {

  public String visitLit(Lit expr) {
    return Integer.toString(expr.value);
  }
  
  public String visitNeg(Neg expr) {
    return "-" + expr.sub.accept(this);
  }

  public String visitAdd(Add expr) {
    return "(" + expr.left.accept(this) + " + " + expr.right.accept(this) + ")";
  }

}

class LitsVisitor implements ExprVisitor<Set<Integer>> {

  public Set<Integer> visitLit(Lit expr) {
    Set<Integer> set = new HashSet<Integer>();
    set.add(expr.value);
    return set;
  }
  
  public Set<Integer> visitNeg(Neg expr) {
    return expr.sub.accept(this);
  }

  public Set<Integer> visitAdd(Add expr) {
    Set<Integer> set = expr.left.accept(this);
    set.addAll(expr.right.accept(this));
    return set;
  }

}
