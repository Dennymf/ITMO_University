import java.io.*;
import java.util.*;

public class laba {
    public static void main(String[] args) throws IOException {
        InputReader in = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(System.out);

        int n = in.nextInt();

        Tree tree = new Tree(n, in);

        int m = in.nextInt();

        for(int i = 0; i < m; i++){
            int x = in.nextInt();
            int y = in.nextInt();

            tree.query(x, y);
        }

        System.out.println(tree.count_ussless_edges()) ;
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
        int[] p, d, jump;
        int[][] dp;
        int r = 1, log;
        ArrayList<Pair> mJump;
        ArrayList<ArrayList<Integer>> edg;

        Tree(int s, InputReader in) {
            mJump = new ArrayList<>();

            for(int i = 0; i < s + 10; i++)
                mJump.add(new Pair(i, -Integer.MAX_VALUE));

            jump = new int[s + 100];
            this.s = s;
            this.p = new int[s + 100];
            this.d = new int[s + 100];
            this.dp = new int[s + 100][log2(s) + 5];
            this.edg = new ArrayList<>();

            for(int i = 0; i < s + 10; i++)
                edg.add(new ArrayList<>());

            log = log2(s);

            read(in);

            set_parents();
            setDP();
            dfs(r, 1 , 0);
        }

        private void read(InputReader in) {
            for(int i = 0; i < s - 1; i++)
            {
                int x = in.nextInt();
                int y = in.nextInt();

                edg.get(x).add(y);
                edg.get(y).add(x);
            }
        }

        public int count_ussless_edges() {
            return s - 1 - count_paths(1, 0);
        }

        private int count_paths(int v, int p) {
            int cnt = 0;
            for (Integer t : edg.get(v)) {
                if (t == p) {
                    continue;
                }
                cnt += count_paths(t, v);
                mJump.set(v, new Pair(v, Math.max(mJump.get(v).value, mJump.get(t).value - 1)));
            }

            if (mJump.get(v).value > 0) {
                return ++cnt;
            } else {
                return cnt;
            }
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

        private void query(int v, int u){
            int lca = lca(v, u);
            get_max_jump(v, lca);
            get_max_jump(u, lca);
        }

        private void get_max_jump(int v, int p){
            mJump.set(v, (new Pair(v, Math.max(Math.abs(d[v] - d[p]), mJump.get(v).value))));
        }
        private void dfs(int v, int prev, int dep) {
            d[v] = dep;

            for (int i = 0; i < edg.get(v).size(); i++) {
                int to = edg.get(v).get(i);
                if(to != prev)
                    dfs(to, v, dep + 1);
            }
        }

        private void set_parents(){
            p[1] = 0;
            dfs_parent(1, 0);
        }

        private void dfs_parent(int v, int prev){
            for (int i = 0; i < edg.get(v).size(); i++) {
                int to = edg.get(v).get(i);
                if (to != prev) {
                    p[to] = v;
                    dfs_parent(to, v);
                }
            }
        }
        private void setDP() {
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

final class Pair implements Comparable{
    public int key;
    public int value;

    public Pair(int key, int value) {
        this.key = key;
        this.value = value;
    }

    @Override
    public int compareTo(Object o) {
        Pair p2 = (Pair)o;
        if (this.key == p2.key) {
            if (this.value < p2.value) {
                return 1;
            } else if (this.value == p2.value) {
                return 0;
            } else {
                return -1;
            }
        } else {
            if (this.key < p2.key) {
                return 1;
            } else if (this.key == p2.key) {
                return 0;
            } else {
                return -1;
            }
        }
    }
}
