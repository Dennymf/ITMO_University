import java.io.*;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;

public class laba {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        PrintWriter writer = new PrintWriter(System.out);
        Treap treap = new Treap();

        int n = Integer.parseInt(scanner.next());

        long y = 0;
        boolean prevQueue = false;

        for (int i = 0; i < n; i++) {
            String query = scanner.next();

            if (query.equals("+")) {
                long x = Long.parseLong(scanner.next());
                if (prevQueue) {
                    treap.insert((x % (long)1e9 + (long)y % (long)1e9) % (long) 1e9);
                } else {
                    treap.insert(x);
                }
                prevQueue = false;
            } else {
                long left = Long.parseLong(scanner.next());
                long right = Long.parseLong(scanner.next());

                long ans = treap.querySum(left, right);
                y = ans;
                prevQueue = true;
                System.out.println(ans);
            }
        }
        scanner.close();
        writer.close();
    }
}

class Treap {
    Node root;
    Random randomGenerator = new Random();

    public Treap() {
        root = null;
    }

    private Pair<Node, Node> split(Node v, long key) {
        if (v == null) {
            return new Pair<>(null, null);
        }

        if (key > v.x) {
            Pair<Node, Node> splt = split(v.right, key);
            v.right = splt.key;
            update(v);
            return new Pair<>(v, splt.value);
        } else {
            Pair<Node, Node> splt = split(v.left, key);
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

    private long vertexSum(Node v) {
        return v == null ? 0 : v.sum;
    }

    private void update(Node v) {
        if (v != null) {
            v.sum = v.x + vertexSum(v.left) + vertexSum(v.right);
        }
    }

    public void insert(long x) {
        if (root == null) {
            root = new Node(x, randomGenerator.nextInt());
        } else {
            if (search(x) == null || search(x) != x) {
                root = insert(root, new Node(x, randomGenerator.nextInt()));
            }
        }
    }

    private Node insert(Node v, Node k) {
        Pair<Node, Node> splt = split(v, k.x);
        splt.key = merge(splt.key, k);
        v = merge(splt.key, splt.value);
        return v;
    }

    public long querySum(long left, long right) {
        long l = getSum(root, left, 0);
        long r = getSum(root, right + 1, 0);
        return r - l;
    }

    private long getSum(Node v, long key, long sum) {
        if (v == null) {
            return sum;
        }

        if (key > v.x) {
            return getSum(v.right, key, sum + vertexSum(v.left) + v.x);
        } else {
            return getSum(v.left, key, sum);
        }
    }

    public Long search(long x) {
        Node f = search(root, x);
        if (f == null) {
            return null;
        } else {
            return f.x;
        }
    }

    private Node search(Node v, long x) {
        if (v == null) {
            return null;
        }

        if (v.x == x) {
            return v;
        }

        if (x < v.x) {
            if (v.left == null) {
                return v;
            }
            return search(v.left, x);
        } else {
            if (v.right == null) {
                return v;
            }
            return search(v.right, x);
        }
    }
    
}

class Node {
    int y;
    long x, sum;
    Node left = null;
    Node right = null;

    Node(long x, int y) {
        this.x = x;
        this.y = y;
        this.sum = x;
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