import javax.swing.*;
import java.awt.*;

public class NGon {
    private static NGon instance = new NGon();
    private JPanel rootPanel;
    private JSpinner anglesCountSpinner;
    private NGonPanel ngonPanel;
    private JButton pickColorButton;
    private JSlider sizeSlider;

    private NGon() {
        initAngleCountSpinner();
        initPickColorButton();
        initSizeSlider();
    }

    public JPanel getRootPanel() {
        return this.rootPanel;
    }

    public static NGon getInstance() {
        if (instance == null) {
            NGon.instance = new NGon();
        }
        return NGon.instance;
    }

    private void initSizeSlider() {
        this.sizeSlider.setMinimum(NGonPanel.EDGE_LENGTH_RANGE.getMin());
        this.sizeSlider.setMaximum(NGonPanel.EDGE_LENGTH_RANGE.getMax());
        this.sizeSlider.setValue(this.ngonPanel.getEdgeLength());
        this.sizeSlider.addChangeListener(event -> {
            int value = this.sizeSlider.getValue();
            this.ngonPanel.setEdgeLength(value);
        });
    }

    private void initPickColorButton() {
        this.pickColorButton.addActionListener(event -> {
            Color selectedColor = JColorChooser.showDialog(this.rootPanel, "Выберите цвет", this.ngonPanel.getColor());
            if (selectedColor != null) {
                this.ngonPanel.setColor(selectedColor);
            }
        });
    }

    private void initAngleCountSpinner() {
        this.anglesCountSpinner.setValue(this.ngonPanel.getAnglesCount());
        this.anglesCountSpinner.addChangeListener(event -> {
            int value = Math.max((Integer) this.anglesCountSpinner.getValue(), NGonPanel.MINIMUM_ANGLES_COUNT);
            this.anglesCountSpinner.setValue(Math.max(value, NGonPanel.MINIMUM_ANGLES_COUNT));
            this.ngonPanel.setAnglesCount(value);
        });
    }

    public static class NGonPanel extends JPanel {
        public static final int MINIMUM_ANGLES_COUNT = 3;
        public static final Range EDGE_LENGTH_RANGE = new Range(50, 100);

        private int anglesCount = MINIMUM_ANGLES_COUNT;
        private Color color = Color.RED;
        private int edgeLength = EDGE_LENGTH_RANGE.getFromPercent(50);


        public void setAnglesCount(int count) {
            assert count >= MINIMUM_ANGLES_COUNT;

            this.anglesCount = count;
            this.repaint();
        }

        public void setColor(Color color) {
            this.color = color;
            this.repaint();
        }

        public void setEdgeLength(int edgeLength) {
            assert EDGE_LENGTH_RANGE.contain(edgeLength);

            this.edgeLength = edgeLength;
            this.repaint();
        }

        public int getEdgeLength() {
            return this.edgeLength;
        }

        public Color getColor() {
            return this.color;
        }

        public int getAnglesCount() {
            return this.anglesCount;
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            drawNGon(g);
        }

        private void drawNGon(Graphics g) {
            Graphics2D graphics = (Graphics2D) g;
            Polygon polygon = getPolygon(this.anglesCount, this.edgeLength, this.getWidth(), this.getHeight());
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            graphics.setColor(this.color);
            graphics.drawPolygon(polygon);
            graphics.fillPolygon(polygon);
        }

        private static Polygon getPolygon(int anglesCount, double radius, int width, int height) {
            Polygon polygon = new Polygon();
            for (int i = 0; i < anglesCount; ++i) {
                int x = (int) (radius * Math.cos(2 * Math.PI * i / anglesCount) + width / 2);
                int y = (int) (height / 2 - radius * Math.sin(2 * Math.PI * i / anglesCount));
                polygon.addPoint(x, y);
            }

            return polygon;
        }

    }

    private static class Range {
        private final int min;
        private final int max;

        public Range(int min, int max) {
            this.min = min;
            this.max = Math.max(max, min);
        }

        public int getMin() {
            return this.min;
        }

        public int getMax() {
            return this.max;
        }

        public int getLength() {
            return this.max - this.min;
        }

        public boolean contain(int value) {
            return this.min <= value && this.max >= value;
        }

        public int getFromPercent(int percent) {
            assert percent >= 0 && percent <= 100;

            return (int) (this.min + this.getLength() * ((double) percent / 100));
        }
    }
}
