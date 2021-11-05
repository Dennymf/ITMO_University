import java.io.*;
import java.util.Scanner;

public class laba {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        PrintWriter writer = new PrintWriter(System.out);

        Node root = null;
        boolean b = false;
        while (scanner.hasNext()) {
            String query = scanner.next();
            int x = Integer.parseInt(scanner.next());
            Node f;
            switch (query) {
                case "insert":
                    if (root == null) {
                        root = new Node(x, null);
                    } else {
                        root.insert(root, x);
                    }
                    break;

                case "delete":
                    if (root != null) {
                        root = root.delete(root, x);
                    }
                    break;

                case "exists":
                    if (root != null) {
                        System.out.println(root.contains(root, x));
                    } else {
                        System.out.println("false");
                    }
                    break;

                case "next":
                    if (root == null) {
                        System.out.println("none");
                    } else {
                        f = root.search(root, x);
                        if (f == null) {
                            System.out.println("none");
                        } else {
                            if (f.key > x) {
                                System.out.println(f.key);
                            } else {
                                f = root.next(f);
                                if (f == null) {
                                    System.out.println("none");
                                } else {
                                    System.out.println(f.key);
                                }
                            }
                        }
                    }
                    break;

                case "prev":
                    if (root == null) {
                        System.out.println("none");
                    } else {
                        f = root.search(root, x);
                        if (f == null) {
                            System.out.println("none");
                        } else {
                            if (f.key < x) {
                                System.out.println(f.key);
                            } else {
                                f = root.prev(f);
                                if (f == null) {
                                    System.out.println("none");
                                } else {
                                    System.out.println(f.key);
                                }
                            }
                        }
                    }
                    break;

                default:

                    break;
            }
        }
        scanner.close();
        writer.close();
    }

    static class Node {
        int key;
        Node left = null;
        Node right = null;
        Node parent;

        Node(int key, Node parent) {
            this.key = key;
            this.parent = parent;
        }

        void insert(Node v, int x) {
            if (v.key == x) {
                return;
            }

            if (x < v.key) {
                if (v.left == null) {
                    v.left = new Node(x, v);
                } else {
                    insert(v.left, x);
                }
            } else {
                if (v.right == null) {
                    v.right = new Node(x, v);
                } else {
                    insert(v.right, x);
                }
            }
        }

        Node search(Node v, int x) {
            if (v == null) {
                return v;
            }

            if (v.key == x) {
                return v;
            }

            if (x < v.key) {
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

        boolean contains(Node v, int x) {
            if (x == v.key) {
                return true;
            }

            if (x < v.key) {
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

        Node findMin(Node v) {
            if (v.left == null) {
                return v;
            }

            return findMin(v.left);
        }

        Node findMax(Node v) {
            if (v.right == null) {
                return v;
            }

            return findMax(v.right);
        }

        Node delete(Node v, int x) {
            if (v == null) {
                return v;
            }

            if (x < v.key) {
                v.left = delete(v.left, x);
            } else if (x > v.key) {
                v.right = delete(v.right, x);
            } else if (v.left != null && v.right != null) {
                v.key = findMin(v.right).key;
                v.right = delete(v.right, v.key);
            } else {
                if (v.left != null) {
                    Node tmp = v;
                    v = v.left;
                    v.parent = tmp.parent;
                } else {
                    Node tmp = v;
                    v = v.right;
                    if (v != null) {
                        v.parent = tmp.parent;
                    }
                }
            }

            return v;
        }

        Node next(Node v) {
            if (v.right != null) {
                return findMin(v.right);
            }

            Node p = v.parent;
            while (p != null && v == p.right) {
                v = p;
                p = v.parent;
            }

            return p;
        }

        Node prev(Node v) {
            if (v.left != null) {
                return findMax(v.left);
            }

            Node p = v.parent;
            while (p != null && v == p.left) {
                v = p;
                p = v.parent;
            }

            return p;
        }
    }
}