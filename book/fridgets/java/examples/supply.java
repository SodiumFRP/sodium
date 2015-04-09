import fridgets.Supply;

public class supply {
    public static void main(String[] args) {
        Supply a = new Supply();
        Supply b = a.child();
        Supply c = b.child();
        Supply bAgain = a.child();
        System.out.println("a = "+a.get());
        System.out.println("b = "+b.get());
        System.out.println("c = "+c.get());
        System.out.println("bAgain = "+bAgain.get());
    }
}

