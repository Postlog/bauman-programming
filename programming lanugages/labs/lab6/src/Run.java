import javax.swing.*;

public class Run {
    public static void main(String[] args) {
        initFrame();
    }

    public static void initFrame() {
        JFrame frame = new JFrame("N-Gon");
        frame.setContentPane(NGon.getInstance().getRootPanel());

        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setSize(600, 400);
        frame.setResizable(false);
        frame.setVisible(true);
    }
}

