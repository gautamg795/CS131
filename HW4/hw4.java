/* Name: Gautam Gupta

   UID: 304282688

   Others With Whom I Discussed Things: Kelly Hosokawa, David Pu

   Other Resources I Consulted:
   
*/

// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic expressions
interface Exp {
    double eval(); 	                       // Problem 1a
    List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    public Num(double _val) {
        val = _val;
    }
    protected double val;

    public double eval() {
        return val;
    }

    public List<Instr> compile() {
        List<Instr> l = new LinkedList<Instr>();
        l.add(new Push(val));
        return l;
    }

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }

    public String toString() { return "" + val; }
}

class BinOp implements Exp {
    protected Exp left, right;
    protected Op op;

    public BinOp(Exp _left, Op _op, Exp _right) {
        left = _left;
        op = _op;
        right = _right;
    }
    
    public double eval() {
        return op.calculate(left.eval(), right.eval());
    }

    public List<Instr> compile() {
        List<Instr> l = left.compile();
        l.addAll(right.compile());
        l.add(new Calculate(op));
        return l;
    }

    public boolean equals(Object o) {
    	if(!(o instanceof BinOp))
    		return false;
    	BinOp b = (BinOp) o;
    	return this.left.equals(b.left) && this.op.equals(b.op) &&
		    	this.right.equals(b.right);
    }

    public String toString() {
		return "BinOp(" + left + ", " + op + ", " + right + ")";
    }
}

// a representation of four arithmetic operators
enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}

// a type for arithmetic instructions
interface Instr {
    void execInstr(Stack<Double> stack);
}

class Push implements Instr {
    protected double val;

    public Push(double _val) {
        val = _val;
    }

    public void execInstr(Stack<Double> stack) {
        stack.push(val);
    }

	public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
		return "Push " + val;
    }

}

class Calculate implements Instr {
    protected Op op;

    public Calculate(Op _op) {
        op = _op;
    }

    public void execInstr(Stack<Double> stack) {
        double l = stack.pop();
        double r = stack.pop();
        stack.push(op.calculate(r, l));
    }

    public boolean equals(Object o) { return (o instanceof Calculate) && 
    						  ((Calculate)o).op.equals(this.op); }

    public String toString() {
		return "Calculate " + op;
    }    
}

class Instrs {
    protected List<Instr> instrs;
    protected Stack<Double> stack = new Stack<Double>();

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

    public double execute() {
        for (Instr i : instrs) {
            i.execInstr(stack);
        }
        return stack.pop();
    }  // Problem 1b
}


class CalcTest {
    public static void main(String[] args) {
	 //    // a test for Problem 1a
		Exp exp =
	    	new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
		    	  	  Op.TIMES,
		      	  new Num(3.0));
		assert(exp.eval() == 9.0);

		// // a test for Problem 1b
		List<Instr> is = new LinkedList<Instr>();
		is.add(new Push(1.0));
		is.add(new Push(2.0));
		is.add(new Calculate(Op.PLUS));
		is.add(new Push(3.0));
		is.add(new Calculate(Op.TIMES));
		Instrs instrs = new Instrs(is);
		assert(instrs.execute() == 9.0);

		// // a test for Problem 1c
		assert(exp.compile().equals(is));
    }
}


// PROBLEM 2

// the type for a set of strings
interface StringSet {
    int size();
    boolean contains(String s);
    void add(String s);
}

// an implementation of StringSet using a linked list
class ListStringSet implements StringSet {
    public ListStringSet() {
        head = new SEmpty();
    }
    public int size() {
        return head.length();
    }
    public boolean contains(String s) {
        return head.contains(s);
    }
    public void add(String s) {
       head = head.add(s); 
    }
    protected SNode head;
}

// a type for the nodes of the linked list
interface SNode {
    int length();
    boolean contains(String s);
    SNode add(String s);
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
    public int length() {
        return 0;
    }
    public boolean contains(String s) {
        return false;
    }
    public SNode add(String s) {
        return new SElement(s, this);
    }
}

// represents a non-empty node
class SElement implements SNode {
    public SElement(String s, SNode n) {
        elem = s;
        next = n;
    }
    public SNode add(String s) {
        int cmp = s.compareTo(elem);
        if (cmp == 0)
            return this;
        else if (cmp < 0)
            return new SElement(s, this);
        else { // cmp > 0
            this.next = this.next.add(s);
            return this;
        }
    }
    public int length() {
        return 1 + next.length();
    }
    public boolean contains(String s) {
        int cmp = s.compareTo(elem);
        if (cmp == 0)
            return true;
        else if (cmp < 0)
            return false;
        else // cmp > 0
            return next.contains(s);
    } 

    protected String elem;
    protected SNode next;
}

interface Set<T> {
    int size();
    boolean contains(T s);
    void add(T s);
    String toString();
}

class ListSet<T> implements Set<T> {
    public ListSet(Comparator<T> comp) {
        head = new Empty<T>(comp);
    }
    public int size() {
        return head.length();
    }
    public boolean contains(T t) {
        return head.contains(t);
    }
    public void add(T t) {
       head = head.add(t); 
    }
    public String toString() {
        return head.toString();
    }
    protected Node<T> head;
}

interface Node<T> {
    int length();
    boolean contains(T t);
    Node<T> add(T t);
    String toString();
}

class Empty<T> implements Node<T> {
    public Empty(Comparator<T> _comp) {
        comp = _comp;
    }
    public int length() {
        return 0;
    }
    public boolean contains(T t) {
        return false;
    }
    public Node<T> add(T t) {
        return new Element<T>(t, this, comp);
    }
    public String toString() {
        return "";
    }
    protected Comparator<T> comp;
}

class Element<T> implements Node<T> {
    public Element(T s, Node<T> n, Comparator<T> _comp) {
        elem = s;
        next = n;
        comp = _comp;
    }

    public Node<T> add(T t) {
        int cmp = comp.compare(elem, t);
        if (cmp == 0)
            return this;
        else if (cmp > 0)
            return new Element<T>(t, this, comp);
        else { // cmp < 0
            this.next = this.next.add(t);
            return this;
        }
    }

    public int length() {
        return 1 + next.length();
    }

    public boolean contains(T t) {
        int cmp = comp.compare(elem, t);
        if (cmp == 0)
            return true;
        else if (cmp > 0)
            return false;
        else // cmp < 0
            return next.contains(t);
    } 
    public String toString() {
        return " " + elem + next.toString();
    }

    protected T elem;
    protected Node<T> next;
    protected Comparator<T> comp;

}

class SetTest {
    public static void main(String[] args) {
        // StringSet ss = new ListStringSet();
        // assert(ss.size() == 0);
        // assert(!ss.contains("str"));
        // ss.add("bat");
        // assert(ss.size() == 1);
        // assert(ss.contains("bat"));
        // ss.add("bat");
        // assert(ss.size() == 1);
        // assert(ss.contains("bat"));
        // ss.add("cat");
        // assert(ss.size() == 2);
        // assert(ss.contains("cat"));

        // Set<String> s = new ListSet<String>(
        //         (String s1, String s2) -> s1.compareTo(s2));
        // assert(s.size() == 0);
        // assert(!s.contains("str"));
        // s.add("zebra");
        // assert(s.size() == 1);
        // assert(s.contains("zebra"));
        // s.add("zebra");
        // assert(s.size() == 1);
        // assert(s.contains("zebra"));
        // s.add("ant");
        // assert(s.size() == 2);
        // assert(s.contains("ant"));
        // s.add("bat");
        // assert(s.size() == 3);
        // assert(s.contains("bat"));
        // System.out.println(s);

        Set<Integer> i = new ListSet<Integer>((i1, i2) -> i1 - i2);
        for (int j = 0; j < 5; j++)
            i.add(j);
        System.out.println(i);
        Set<Integer> ii = new ListSet<Integer>((i1, i2) -> i2 - i1);
        for (int j = 0; j < 5; j++)
            ii.add(j);
        System.out.println(ii);
    }
}
