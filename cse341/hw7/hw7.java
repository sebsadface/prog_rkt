public static final double epsilon = 0.00001;

public floatClose (double f1, double f2) {
    return Math.abs(f1 - f2) < epsilon;
}

public floatClosePoint (double x1, double y1, double x2, double y2) {
    return floatClose(x1, x2) && floatClose(y1, y2);
}

public abstract class GeometryExpression extends Object {}

public abstract class GeometryValue extends GeometryExpression {
    public twoPointsToLine (double x1, double y1, double x2, double y2) {
        if (floatClose(x1, x2)) {
            return new VerticalLine(x1);
        } else {
            double m = (y2 - y1) / (x2 - x1);
            double b = y1 - m * x1;
            return new Line(m, b);
        }
    }

    public intersectNoPoints (NoPoints np) {
        return np;
    }

    public intersectLineSegment (LineSegment seg) {
        this.intersect.twoPointsToLine(seg.x1, seg.y1, seg.x2, seg.y2).intersectWithSegmentAsLineResult(LineSegment seg);
    }

    public evalProg (List<Pair<String, GeometryExpression>>) {
        return this;
    }
}

public class NoPoints extends GeometryValue {
    public shift (double dx, double dy) {
        return this;
    }

    public intersect (GeometryValue other) {
        return other.intersectNoPoints(this);
    }

    public intersectPoint (Point p) {
        return this;
    }

    public intersectLine (Line line) {
        return this;
    }

    public intersectVerticalLine (VerticalLine vline) {
        return this;
    }

    public intersectWithSegmentAsLineResult (LineSegment seg) {
        return this;
    }

    public preprocessProg () {
        return this;
    }
}

public class Point extends GeometryValue {
    private double x;
    private double y;

    public Point (double x, double y) {
        this.x = x;
        this.y = y;
    }

    public getX () {
        return this.x;
    }

    public getY () {
        return this.y;
    }

    public shift (double dx, double dy) {
        return new Point(this.x + dx, this.y + dy);
    }

    public intersect (GeometryValue other) {
        return other.intersectPoint(this);
    }

    public intersectPoint (Point p) {
        if (floatClosePoint(this.x, this.y, p.getX(), p.getY())) {
            return this;
        } else {
            return new NoPoints();
        }
    }

    public intersectLine (Line line) {
        if (floatClose(this.y, line.getM() * this.x + line.getB())) {
            return this;
        } else {
            return new NoPoints();
        }
    }

    public intersectVerticalLine (VerticalLine vline) {
        if (floatClose(this.x, vline.getX())) {
            return this;
        } else {
            return new NoPoints();
        }
    }

    public intersectWithSegmentAsLineResult (LineSegment seg) {
        return seg.intersectPoint(this);
    }

    public preprocessProg () {
        return this;
    }
}

public class Line extends GeometryValue {
    private double m;
    private double b;

    public Line (double m, double b) {
        this.m = m;
        this.b = b;
    }

    public getM () {
        return this.m;
    }

    public getB () {
        return this.b;
    }

    public shift (double dx, double dy) {
        return new Line(this.m, this.b + dy - this.m * dx);
    }

    public intersect (GeometryValue other) {
        return other.intersectLine(this);
    }

    public intersectPoint (Point p) {
        return p.intersectLine(this);
    }

    public intersectLine (Line line) {
        if (floatClose(this.m, line.getM())) {
            if (floatClose(this.b, line.getB())) {
                return this;
            } else {
                return new NoPoints();
            }
        } else {
            double x = (line.getB() - this.b) / (this.m - line.getM());
            double y = this.m * x + this.b;
            return new Point(x, y);
        }
    }

    public intersectVerticalLine (VerticalLine vline) {
        return new Point(vline.getX(), this.m * vline.getX() + this.b);
    }

    public intersectWithSegmentAsLineResult (LineSegment seg) {
        return seg.intersectLine(this);
    }

    public preprocessProg () {
        return this;
    }
}

public class VerticalLine extends GeometryValue {
    private double x;

    public VerticalLine (double x) {
        this.x = x;
    }

    public getX () {
        return this.x;
    }

    public shift (double dx, double dy) {
        return new VerticalLine(this.x + dx);
    }

    public intersect (GeometryValue other) {
        return other.intersectVerticalLine(this);
    }

    public intersectPoint (Point p) {
        return p.intersectVerticalLine(this);
    }

    public intersectLine (Line line) {
        return line.intersectVerticalLine(this);
    }

    public intersectVerticalLine (VerticalLine vline) {
        if (floatClose(this.x, vline.getX())) {
            return this;
        } else {
            return new NoPoints();
        }
    }

    public intersectWithSegmentAsLineResult (LineSegment seg) {
        return seg.intersectVerticalLine(this);
    }

    public preprocessProg () {
        return this;
    }
}

public class LineSegment extends GeometryValue {
    private double x1;
    private double y1;
    private double x2;
    private double y2;

    public LineSegment (double x1, double y1, double x2, double y2) {
        this.x1 = x1;
        this.y1 = y1;
        this.x2 = x2;
        this.y2 = y2;
    }

    public getX1 () {
        return this.x1;
    }

    public getY1 () {
        return this.y1;
    }

    public getX2 () {
        return this.x2;
    }

    public getY2 () {
        return this.y2;
    }

    public shift (double dx, double dy) {
        return new LineSegment(this.x1 + dx, this.y1 + dy, this.x2 + dx, this.y2 + dy);
    }

    public intersect (GeometryValue other) {
        return other.intersectLineSegment(this);
    }

    public intersectPoint (Point p) {
        return p.intersectWithSegmentAsLineResult(this);
    }

    public intersectLine (Line line) {
        return line.intersectWithSegmentAsLineResult(this);
    }

    public intersectVerticalLine (VerticalLine vline) {
        return vline.intersectWithSegmentAsLineResult(this);
    }

    public intersectWithSegmentAsLineResult (LineSegment seg) {
        double x1 = Math.max(this.x1, seg.getX1());
        double y1 = Math.max(this.y1, seg.getY1());
        double x2 = Math.min(this.x2, seg.getX2());
        double y2 = Math.min(this.y2, seg.getY2());
        if (x1 > x2 || y1 > y2) {
            return new NoPoints();
        } else {
            return new LineSegment(x1, y1, x2, y2);
        }
    }

    public preprocessProg () {
        return this;
    }
}

public class Intersect extends GeometryExpression {
    private GeometryExpression e1;
    private GeometryExpression e2;

    public Intersect (GeometryExpression e1, GeometryExpression e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    public getE1 () {
        return this.e1;
    }

    public getE2 () {
        return this.e2;
    }

    public evalProg (List<Pair<String, GeometryExpression>>) {
        return this.e1.evalProg(env).intersect(this.e2.evalProg(env));
    }

    public preprocessProg () {
        return new Intersect(this.e1.preprocessProg(), this.e2.preprocessProg());
    }
}

public class Let extends GeometryExpression {
    private String name;
    private GeometryExpression e1;
    private GeometryExpression e2;

    public Let (String name, GeometryExpression e1, GeometryExpression e2) {
        this.name = name;
        this.e1 = e1;
        this.e2 = e2;
    }

    public getName () {
        return this.name;
    }

    public getE1 () {
        return this.e1;
    }

    public getE2 () {
        return this.e2;
    }

    public evalProg (List<Pair<String, GeometryExpression>>) {
        return this.e2.evalProg(env.put(this.name, this.e1.evalProg(env)));
    }

    public preprocessProg () {
        return new Let(this.name, this.e1.preprocessProg(), this.e2.preprocessProg());
    }
}

public class Var extends GeometryExpression {
    private String name;

    public Var (String name) {
        this.name = name;
    }

    public getName () {
        return this.name;
    }

    public evalProg (List<Pair<String, GeometryExpression>>) {
        return env.get(this.name);
    }

    public preprocessProg () {
        return this;
    }
}

public class Shift extends GeometryExpression {
    private GeometryExpression e;
    private double dx;
    private double dy;

    public Shift (GeometryExpression e, double dx, double dy) {
        this.e = e;
        this.dx = dx;
        this.dy = dy;
    }

    public getE () {
        return this.e;
    }

    public getDx () {
        return this.dx;
    }

    public getDy () {
        return this.dy;
    }

    public evalProg (List<Pair<String, GeometryExpression>>) {
        return this.e.evalProg(env).shift(this.dx, this.dy);
    }

    public preprocessProg () {
        return new Shift(this.e.preprocessProg(), this.dx, this.dy);
    }
}