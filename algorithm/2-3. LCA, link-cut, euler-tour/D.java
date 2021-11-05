import java.io.*;
import java.util.*;

public class laba {
    public static void main(String[] args) throws IOException {
        InputReader in = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(System.out);

        int n = in.nextInt(), v, u;

        Tree tree = new Tree(n);

        for (int i = 0; i < n; i++) {
            String query = in.next();

            switch (query) {
                case "+":
                    v = in.nextInt();
                    tree.add(v);
                    break;

                case "-":
                    v = in.nextInt();
                    tree.del(v);
                    break;

                case "?":
                    v = in.nextInt();
                    u = in.nextInt();
                    System.out.println(tree.alive_lca(v, u));
                    break;
            }
        }
        out.close();
    }

    static class InputReader {
        public BufferedReader reader;
        public StringTokenizer token;

        public InputReader(InputStream in) {
            reader = new BufferedReader(new InputStreamReader(in), 32768);
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

        public int nextInt() {
            return Integer.parseInt(next());
        }

    }

    static class Tree {
        int s;
        boolean[] used;
        int[] pr, d, w;
        int[][] dp, min;
        int r = 1, log, curN = 1;

        Tree(int s) {
            used = new boolean[s + 100];
            this.pr = new int[s + 100];
            this.d = new int[s + 100];
            this.dp = new int[s + 100][log2(s + 100) + 5];

            log = log2(s + 100);

            used[1] = true;
            pr[1] = 0;
            d[1] = 0;
        }

        public int alive_lca(int v, int u) {
            int lca = lca(v, u);
            return find_alive_parent(lca);
        }

        private int find_alive_parent(int lca) {
            if (used[lca]) {
                return lca;
            }

            return pr[lca] = find_alive_parent(pr[lca]);
        }

        public int lca(int v, int u) {
            if (d[v] > d[u]) {
                int tmp = v;
                v = u;
                u = tmp;
            }

            int h = d[u] - d[v];
            for (int i = log; i >= 0; i--)
                if ((1 << i) <= h) {
                    h -= (1 << i);
                    u = dp[u][i];
                }

            if (u == v)
                return v;

            for (int i = log; i >= 0; i--)
                if (dp[v][i] != dp[u][i]) {
                    v = dp[v][i];
                    u = dp[u][i];
                }

            return pr[v];
        }

        public void add(int p){
            curN++;
            d[curN] = d[p] + 1;
            used[curN] = true;
            pr[curN] = p;
            dp[curN][0] = p;

            for(int i = 1; i <= log2(curN); i++)
                dp[curN][i] = dp[dp[curN][i - 1]][i - 1];
        }

        public void del(int v) {
            used[v] = false;
        }
    }

    public static int log2(int n) {
        if (n == 0)
            return 0;

        return log2(n / 2) + 1;
    }
}