import java.io.*;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;

public class laba {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        PrintWriter writer = new PrintWriter(System.out);
        Treap treap = new Treap();

        while (scanner.hasNext()) {
            String query = scanner.next();
            int x = Integer.parseInt(scanner.next());
            switch (query) {
                case "insert":
                    treap.insert(x);
                    break;

                case "delete":
                    treap.remove(x);
                    break;

                case "exists":
                    if (treap.isEmpty()) {
                        System.out.println("false");
                    } else {
                        System.out.println(treap.contains(x));
                    }
                    break;

                case "next":
                    if (treap.isEmpty()) {
                        System.out.println("none");
                    } else {
                        if (treap.next(x) == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(treap.next(x));
                        }
                    }
                    break;

                case "prev":
                    if (treap.isEmpty()) {
                        System.out.println("none");
                    } else {
                        if (treap.prev(x) == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(treap.prev(x));
                        }
                    }
                    break;
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

    public boolean isEmpty() {
        return root == null;
    }

    private Pair<Node, Node> split(Node v, int key) {
        if (v == null) {
            return new Pair<>(null, null);
        }

        if (key > v.x) {
            Pair<Node, Node> splt = split(v.right, key);
            v.right = splt.key;
            return new Pair<>(v, splt.value);
        } else {
            Pair<Node, Node> splt = split(v.left, key);
            v.left = splt.value;
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
            return t1;
        } else {
            t2.left = merge(t1, t2.left);
            return t2;
        }
    }

    public void insert(int x) {
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


    public void remove(int x) {
        if (root != null) {
            root = remove(root, x);
        }
    }

    private Node remove(Node v, int x) {
        if (v == null) {
            return null;
        }

        if (v.x == x) {
            return merge(v.left, v.right);
        }

        if (x < v.x) {
            v.left = remove(v.left, x);
        } else {
            v.right = remove(v.right, x);
        }

        return v;
    }

    public Integer search(int x) {
        Node found = search(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node search(Node v, int x) {
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

    public boolean contains(int x) {
        return contains(root, x);
    }

    private boolean contains(Node v, int x) {
        if (x == v.x) {
            return true;
        }

        if (x < v.x) {
            if (v.left == null) {
                return false;
            }

            return contains(v.left, x);
        } else {
            if (v.right == null) {
                return false;
            }

            return contains(v.right, x);
        }
    }

    public Integer next(int x) {
        Node found = next(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node next(Node v, int x) {
        Node cur = v;
        Node suc = null;

        while (cur != null) {
            if (x < cur.x) {
                suc = cur;
                cur = cur.left;
            } else {
                cur = cur.right;
            }
        }

        return suc;
    }

    public Integer prev(int x) {
        Node found = prev(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node prev(Node v, int x) {
        Node cur = v;
        Node suc = null;

        while (cur != null) {
            if (x <= cur.x) {
                cur = cur.left;
            } else {
                suc = cur;
                cur = cur.right;
            }
        }

        return suc;
    }
}

class Node {
    int x, y;
    Node left = null;
    Node right = null;

    Node(int x, int y) {
        this.x = x;
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