class Zipper {
    private int value;

    BinaryTree tree;
    Zipper up;
    Zipper left;
    Zipper right;

    Zipper(int value) {
        this.value = value;
    }

    int getValue() {
        return value;
    }

    void setValue(int value) {
        this.value = value;
    }

    void setLeft(Zipper left) {
        this.left = left;
        if (this.left != null) {
            this.left.up = this;
        }
    }

    void setRight(Zipper right) {
        this.right = right;
        if (this.right != null) {
            this.right.up = this;
        }
    }

    BinaryTree toTree() {
        Zipper zipper = this;
        while (zipper.up != null) {
            zipper = zipper.up;
        }
        return zipper.tree;
    }

    String printTree() {
        return String.format("value: %d, left: %s, right: %s", value, safePrintTree(left), safePrintTree(right));
    }

    static String safePrintTree(Zipper zipper) {
        if (zipper == null) {
            return "null";
        }
        return String.format("{ %s }", zipper.printTree());
    }
}