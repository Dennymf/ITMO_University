import java.io.*;
import java.util.*;

public class laba {
    public static void main(String[] args) throws IOException {
        InputReader scanner = new InputReader(System.in);
        OutputStream out = new BufferedOutputStream(System.out);
        Treap treap = new Treap();

        int n = Integer.parseInt(scanner.next());
        int m = Integer.parseInt(scanner.next());

        for (int i = 1; i <= m + n; i++) {
            treap.insert(i, 0);
        }

        for (int i = 1; i <= n; i++) {
            int x = Integer.parseInt(scanner.next());
            treap.query(x, i);
            treap.minIndex = Integer.MAX_VALUE;
        }

        treap.go(treap.root, 0);
        treap.getArray(treap.root);

        out.write(Integer.toString(treap.a.size()).getBytes());
        out.write("\n".getBytes());
        for (int item : treap.a) {
            out.write(Integer.toString(item).getBytes());
            out.write(" ".getBytes());
        }
        out.close();
    }

    static class InputReader {
        public BufferedReader reader;
        public StringTokenizer token;

        public InputReader(InputStream stream) {
            reader = new BufferedReader(new InputStreamReader(stream), 32768);
            token = null;
        }

        public String next() {
            while (token == null || !token.hasMoreTokens()) {
                try {
                    token = new StringTokenizer(reader.readLine());
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            return token.nextToken();
        }
    }
}

class Treap {
    Node root;
    Random randomGenerator = new Random();
    public static int minIndex = Integer.MAX_VALUE;
    public static int lastIndex = 0;

    public Treap() {
        root = null;
    }

    ArrayList<Integer> a = new ArrayList<>();

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

    private int vertexSum(Node v) {
        if (v == null) {
            return 0;
        } else {
            return v.sum;
        }
    }

    public void insert(int pos, int value) {
        if (root == null) {
            root = new Node(value, randomGenerator.nextInt());
        } else {
            root = insert(root, pos, value);
        }
    }

    private int vMin(Node v) {
        return v == null ? Integer.MAX_VALUE : v.min;
    }


    private void update(Node v) {
        if (v != null) {
            v.w = 1 + vertexWeight(v.left) + vertexWeight(v.right);
            v.sum = v.value + vertexSum(v.left) + vertexSum(v.right);
            v.min = Math.min(v.value, Math.min(vMin(v.left), vMin(v.right)));
        }
    }

    public void query(int index, int value) {
        Pair<Node, Node> splt = split(root, index - 1);
        Node seg = splt.value;
        find(seg, 0);
        seg = remove(seg, minIndex);
        seg = merge(new Node(value, randomGenerator.nextInt()), seg);
        root = merge(splt.key, seg);
    }

    public void find(Node v, int index) {
        if (v == null) {
            return;
        }

        if (v.value == 0) {
            minIndex = Math.min(minIndex, index + vertexWeight(v.left) + 1);
        }

        if (vMin(v.left) == 0) {
            find(v.left, index);
        } else {
            find(v.right, index + vertexWeight(v.left) + 1);
        }
    }

    public void go(Node v, int index) {
        if (v == null) {
            return;
        }

        go(v.left, index);
        if (v.value != 0) {
            lastIndex = index + vertexWeight(v.left) + 1;
        }
        go(v.right, index + vertexWeight(v.left) + 1);
    }

    private Node insert(Node v, int pos, int value) {
        Pair<Node, Node> splt = split(v, pos - 1);
        splt.key = merge(splt.key, new Node(value, randomGenerator.nextInt()));
        v = merge(splt.key, splt.value);
        return v;
    }

    private Node remove(Node v, int pos) {
        Pair<Node, Node> splt = split(v, pos - 1);
        Pair<Node, Node> splt2 = split(splt.value, 1);

        v = merge(splt.key, splt2.value);
        update(v);
        return v;
    }

    public void getArray(Node v) {
        if (v == null || a.size() > lastIndex) {
            return;
        }

        getArray(v.left);
        if (a.size() < lastIndex) {
            a.add(v.value);
        }
        getArray(v.right);
    }
}

class Node {
    int value;
    int y;
    int w = 1;
    int min;
    int sum = 0;
    Node left = null;
    Node right = null;

    Node(int value, int y) {
        this.value = value;
        this.min = value;
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