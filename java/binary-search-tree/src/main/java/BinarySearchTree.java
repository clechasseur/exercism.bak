import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class BinarySearchTree<T extends Comparable<T>> {
    private Node<T> root;

    void insert(T value) {
        Node<T> node = new Node<>(value);
        if (root == null) {
            root = node;
        } else {
            root.insert(node);
        }
    }

    List<T> getAsSortedList() {
        if (root == null) {
            return Collections.emptyList();
        }
        return root.stream(0)
                .map(ElementInfo::getData)
                .collect(Collectors.toList());
    }

    List<T> getAsLevelOrderList() {
        if (root == null) {
            return Collections.emptyList();
        }
        return root.stream(0)
                .sorted(Comparator.comparingInt(ElementInfo::getLevel))
                .map(ElementInfo::getData)
                .collect(Collectors.toList());
    }

    Node<T> getRoot() {
        return root;
    }

    static class Node<T extends Comparable<T>> {
        private final T data;
        private Node<T> left;
        private Node<T> right;

        Node(final T data) {
            this.data = data;
        }

        Node<T> getLeft() {
            return left;
        }

        Node<T> getRight() {
            return right;
        }

        T getData() {
            return data;
        }

        void insert(Node<T> node) {
            if (data.compareTo(node.getData()) < 0) {
                if (right == null) {
                    right = node;
                } else {
                    right.insert(node);
                }
            } else if (left == null) {
                left = node;
            } else {
                left.insert(node);
            }
        }

        Stream<ElementInfo<T>> stream(int level) {
            Stream<ElementInfo<T>> output = Stream.empty();
            if (left != null) {
                output = left.stream(level + 1);
            }
            output = Stream.concat(output, Stream.of(new ElementInfo<>(data, level)));
            if (right != null) {
                output = Stream.concat(output, right.stream(level + 1));
            }
            return output;
        }
    }

    static class ElementInfo<T> {
        private final T data;
        private final int level;

        ElementInfo(final T data, final int level) {
            this.data = data;
            this.level = level;
        }

        T getData() {
            return data;
        }

        int getLevel() {
            return level;
        }
    }
}
