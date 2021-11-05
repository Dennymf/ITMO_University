import java.io.*;
import java.util.*;

public class laba {
    public static void main(String[] args) throws IOException {
        InputReader in = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(System.out);

        int n = in.nextInt();

        Tree tree = new Tree(n, in);

        int m = in.nextInt();

        for (int i = 0; i < m; i++) {
            int v = in.nextInt();
            int u = in.nextInt();

            out.write(Integer.toString(tree.lca(v, u)).getBytes());
            out.write("\n".getBytes());
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
        int[] p;
        int[] d;
        int[][] dp;
        int r = 1;
        int log;
        ArrayList<ArrayList<Integer>> edg;

        Tree(int s, InputReader in) {
            this.s = s;
            this.p = new int[s + 100];
            this.d = new int[s + 100];
            this.dp = new int[s + 100][log2(s) + 5];
            this.edg = new ArrayList<>();
            
            for (int i = 0; i <= s; i++)
                edg.add(new ArrayList<>());

            log = log2(s);

            get_parents(in);
            DP();
            dfs(r, 0);
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

            return p[v];
        }

        private void dfs(int v, int d) {
            this.d[v] = d;
            for (int i = 0; i < edg.get(v).size(); i++) {
                int to = edg.get(v).get(i);
                dfs(to, d + 1);
            }
        }

        private void get_parents(InputReader in) {

            for (int i = 2; i <= s; i++) {
                int tmp = in.nextInt();
                p[i] = tmp;
                edg.get(tmp).add(i);
            }
        }

        private void DP() {
            for (int i = 1; i <= s; i++)
                dp[i][0] = p[i];

            for (int j = 1; j <= log; j++)
                for (int i = 1; i <= s; i++)
                    dp[i][j] = dp[dp[i][j - 1]][j - 1];

        }
    }

    public static int log2(int n) {
        if (n == 0)
            return 0;

        return log2(n / 2) + 1;
    }
}