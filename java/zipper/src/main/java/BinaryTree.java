class BinaryTree {
    private Zipper root;

    BinaryTree(int value) {
        this(new Zipper(value));
    }

    BinaryTree(Zipper root) {
        this.root = root;
        if (this.root != null) {
            this.root.tree = this;
        }
    }

    Zipper getRoot() {
        return root;
    }

    String printTree() {
        return root != null ? root.printTree() : "null";
    }
}
