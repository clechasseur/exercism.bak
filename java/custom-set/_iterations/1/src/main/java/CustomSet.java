import java.lang.reflect.Array;
import java.util.*;

public class CustomSet<E extends Comparable<E>> implements Set<E>, Collection<E>, Iterable<E> {
    private Node root;
    private int siz;

    public CustomSet() {
    }

    public CustomSet(Collection<? extends E> items) {
        addAll(items);
    }

    @Override
    public int size() {
        return siz;
    }

    @Override
    public boolean isEmpty() {
        return root == null;
    }

    @Override
    public boolean contains(Object o) {
        return root != null && root.e.getClass().isAssignableFrom(o.getClass()) && root.find((E) o) != null;
    }

    @Override
    public Iterator<E> iterator() {
        return new CustomIterator();
    }

    @Override
    public Object[] toArray() {
        return toArray(new Object[siz]);
    }

    @Override
    public <T> T[] toArray(T[] a) {
        T[] result = a.length >= siz ? a : (T[]) Array.newInstance(a.getClass().getComponentType(), siz);
        int i = 0;
        for (E e : this) {
            result[i++] = (T) e;
        }
        for (; i < result.length; i++) {
            result[i] = null;
        }
        return result;
    }

    @Override
    public boolean add(E e) {
        Node node = new Node(e);
        boolean added = root == null || root.insert(node);
        if (added) {
            siz++;
        }
        if (root == null) {
            root = node;
        }
        return added;
    }

    @Override
    public boolean remove(Object o) {
        boolean removed = root != null && root.e.getClass().isAssignableFrom(o.getClass()) && root.remove((E) o);
        if (removed) {
            siz--;
        }
        return removed;
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return c.stream().allMatch(this::contains);
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        return c.stream().map(this::add).reduce(false, (a, b) -> a || b);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        if (isEmpty()) {
            return false;
        }
        CustomSet<E> other = new CustomSet<>();
        for (Object o : c) {
            if (root.e.getClass().isAssignableFrom(o.getClass())) {
                other.add((E) o);
            }
        }
        List<E> toRemove = new ArrayList<>();
        for (E e : this) {
            if (!other.contains(e)) {
                toRemove.add(e);
            }
        }
        return removeAll(toRemove);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        return c.stream().map(this::remove).reduce(false, (a, b) -> a || b);
    }

    @Override
    public void clear() {
        root = null;
        siz = 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CustomSet<?> customSet = (CustomSet<?>) o;
        return siz == customSet.siz && Arrays.equals(toArray(), customSet.toArray());
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(toArray());
    }

    public boolean isSubset(CustomSet<E> candidateSubset) {
        return containsAll(candidateSubset);
    }

    public boolean isDisjoint(CustomSet<E> other) {
        return getIntersection(other).isEmpty();
    }

    public CustomSet<E> getIntersection(CustomSet<E> other) {
        CustomSet<E> result = new CustomSet<>(this);
        result.retainAll(other);
        return result;
    }

    public CustomSet<E> getDifference(CustomSet<E> other) {
        CustomSet<E> result = new CustomSet<>(this);
        result.removeAll(other);
        return result;
    }

    public CustomSet<E> getUnion(CustomSet<E> other) {
        CustomSet<E> result = new CustomSet<>(this);
        result.addAll(other);
        return result;
    }

    private final class Node {
        E e;
        Node parent;
        Node left;
        Node right;

        Node(E e) {
            this.e = e;
        }

        boolean insert(Node node) {
            int cmp = node.e.compareTo(e);
            if (cmp < 0) {
                if (left == null) {
                    left = node;
                    node.parent = this;
                    return true;
                } else {
                    return left.insert(node);
                }
            } else if (cmp > 0) {
                if (right == null) {
                    right = node;
                    node.parent = this;
                    return true;
                } else {
                    return right.insert(node);
                }
            }
            return false;
        }

        Node find(E e) {
            int cmp = e.compareTo(this.e);
            if (cmp < 0) {
                return left != null ? left.find(e) : null;
            } else if (cmp > 0) {
                return right != null ? right.find(e) : null;
            }
            return this;
        }

        boolean remove(E e) {
            int cmp = e.compareTo(this.e);
            if (cmp < 0) {
                return left != null && left.remove(e);
            } else if (cmp > 0) {
                return right != null && right.remove(e);
            }
            if (left != null) {
                if (right != null) {
                    left.insert(right);
                }
                if (parent != null) {
                    parent.replace(this, left);
                } else {
                    setRoot(left);
                }
            } else if (parent != null) {
                parent.replace(this, right);
            } else {
                setRoot(right);
            }
            return true;
        }

        private void replace(Node toRemove, Node replacement) {
            if (left == toRemove) {
                left = replacement;
            } else {
                right = replacement;
            }
            if (replacement != null) {
                replacement.parent = this;
            }
        }

        private void setRoot(Node newRoot) {
            if (newRoot != null) {
                newRoot.parent = null;
            }
            root = newRoot;
        }
    }

    private final class CustomIterator implements Iterator<E> {
        private Node node;

        CustomIterator() {
            node = root;
            drillLeft();
        }

        @Override
        public boolean hasNext() {
            return node != null;
        }

        @Override
        public E next() {
            E e = node.e;
            if (node.right == null) {
                while (node.parent != null && node.parent.right == node) {
                    node = node.parent;
                }
                node = node.parent;
            } else {
                node = node.right;
                drillLeft();
            }
            return e;
        }

        @Override
        public void remove() {
            CustomSet.this.remove(next());
        }

        private void drillLeft() {
            if (node != null) {
                while (node.left != null) {
                    node = node.left;
                }
            }
        }
    }
}
