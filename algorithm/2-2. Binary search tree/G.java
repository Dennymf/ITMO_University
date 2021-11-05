import java.util.*;

public class laba {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Treap treap = new Treap();

        int n = Integer.parseInt(scanner.next());
        int m = Integer.parseInt(scanner.next());

        for (int i = 1; i <= n; i++) {
            treap.insert(i, i);
        }

        for (int i = 0; i < m; i++) {
            int left = Integer.parseInt(scanner.next());
            int right = Integer.parseInt(scanner.next());
            treap.toBegin(left, right);
        }
        treap.print();
        scanner.close();
    }
}

class Treap {
    Node root;
    Random randomGenerator = new Random();

    public Treap() {
        root = null;
    }

    private Pair<Node, Node> split(Node v, int k) {
        if (v == null) {
            return new Pair<>(null, null);
        }

        int w = vertexWeight(v.left);
        if (w < k) {
            Pair<Node, Node> splt = split(v.right, k - w - 1);
            v.right = splt.key;
            update(v);
            return new Pair<>(v, splt.value);
        } else {
            Pair<Node, Node> splt = split(v.left, k);
            v.left = splt.value;
            update(v);
            return new Pair<>(splt.key, v);
        }
    }

    private Node merge(Node t1, Node t2) {
        if (t1 == null) {
            return t2;
        }

        if (t2 == null) {
            return t1;
        }

        if (t1.y > t2.y) {
            t1.right = merge(t1.right, t2);
            update(t1);
            return t1;
        } else {
            t2.left = merge(t1, t2.left);
            update(t2);
            return t2;
        }
    }

    private int vertexWeight(Node v) {
        if (v == null) {
            return 0;
        } else {
            return v.w;
        }
    }

    public void insert(int pos, int value) {
        if (root == null) {
            root = new Node(value, randomGenerator.nextInt());
        } else {
            root = insert(root, pos, value);
        }
    }

    private void update(Node v) {
        if (v != null) {
            int w = 0;
            int sum = 0;
            if (v.left != null) {
                w += v.left.w;
                sum += v.left.sum;
            }
            if (v.right != null) {
                w += v.right.w;
                sum += v.right.sum;
            }

            v.w = w + 1;
            v.sum = sum + v.value;
        }
    }

    private Node insert(Node v, int pos, int value) {
        Pair<Node, Node> splt = split(v, pos - 1);
        splt.key = merge(splt.key, new Node(value, randomGenerator.nextInt()));
        v = merge(splt.key, splt.value);
        return v;
    }


    public void toBegin(int left, int right) {
        Pair<Node, Node> splt = split(root, left - 1);
        Pair<Node, Node> splt2 = split(splt.value, right - left + 1);
        root = merge(splt2.key, splt.key);
        root = merge(root, splt2.value);
    }

    public void print() {
        print(root);
    }

    private void print(Node v) {
        if (v == null) {
            return;
        }

        print(v.left);
        System.out.print(v.value + " ");
        print(v.right);
    }
}

class Node {
    int value, y, w = 1;
    int sum = 0;
    Node left = null;
    Node right = null;

    Node(int value, int y) {
        this.value = value;
        this.y = y;
    }
}

final class Pair<K, V> implements Map.Entry<K, V> {
    public K key;
    public V value;

    public Pair(K key, V value) {
        this.key = key;
        this.value = value;
    }

    public K getKey() {
        return key;
    }

    public V getValue() {
        return value;
    }

    public V setValue(V value) {
        V old = this.value;
        this.value = value;
        return old;
    }
}