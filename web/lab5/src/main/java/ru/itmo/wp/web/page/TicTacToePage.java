package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.*;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class TicTacToePage {
    private State field;

    private void action(HttpServletRequest request, Map<String, Object> view) {
        fillField(request);
        view.put("state", field);
    }

    private void onMove(HttpServletRequest request, Map<String, Object> view) {
        fillField(request);
        List<String> parameterNames = new ArrayList<>(request.getParameterMap().keySet());
        if (parameterNames.size() >= 2) {
            String cl = parameterNames.get(1);
            int x = cl.charAt(5) - 48;
            int y = cl.charAt(6) - 48;
            if (field.makeMove(x, y)) {
                HttpSession session = request.getSession();
                session.setAttribute("cell_" + x + y, field.crossesMove ? 'O' : 'X');
                session.setAttribute("crossesMove", field.crossesMove);
                session.setAttribute("phase", field.phase);
            }
        }
        view.put("state", field);
    }

    private void newGame(HttpServletRequest request, Map<String, Object> view) {
        if (field == null) {
            field = new State();
        }
        HttpSession session = request.getSession();
        for (int i = 0; i < field.size; i++) {
            for (int j = 0; j < field.size; j++) {
                if (session.getAttribute("cell_" + i + j) != null) {
                    session.removeAttribute("cell_" + i + j);
                }
            }
        }
        session.removeAttribute("crossesMove");
        session.removeAttribute("phase");
        fillField(request);
        view.put("state", field);
    }

    void fillField(HttpServletRequest request) {
        if (field == null) {
            field = new State();
        }
        HttpSession session = request.getSession();
        for (int i = 0; i < field.size; i++) {
            for (int j = 0; j < field.size; j++) {
                if (session.getAttribute("cell_" + i + j) != null) {
                    field.cells[i][j] = (Character)session.getAttribute("cell_" + i + j);
                }
            }
        }

        if (session.getAttribute("crossesMove") != null) {
            field.crossesMove = (boolean) session.getAttribute("crossesMove");
        } else {
            field.crossesMove = true;
        }

        if (session.getAttribute("phase") != null) {
            field.phase = (String) session.getAttribute("phase");
        } else {
            field.phase = "RUNNING";
        }
    }

    public class State {
        private int size = 3;
        private Character[][] cells = new Character[size][size];
        private String phase = "RUNNING";
        private boolean crossesMove = true;

        public int getSize() {
            return size;
        }

        public Character[][] getCells() {
            return cells;
        }

        public String getPhase() {
            return phase;
        }

        public boolean getCrossesMove() {
            return crossesMove;
        }

        public boolean makeMove(int x, int y) {
            if (x < 0 || x >= field.size || y < 0 || y >= field.size || cells[x][y] != null || !phase.equals("RUNNING")) {
                return false;
            }
            cells[x][y] = crossesMove ? 'X' : 'O';
            crossesMove ^= true;
            checkWin();
            return true;
        }

        private void checkWin() {
            int free = field.size * field.size * 4;
            int winLen = field.size;
            int[] di = {-1, 0, 1, 1};
            int[] dj = {1, 1, 1, 0};
            for (int m = 0; m < 4; m++) {
                for (int i = 0; i < field.size; i++) {
                    for (int j = 0; j < field.size; j++) {
                        if (field.cells[i][j] != null){
                            free--;
                        }
                        int ii = i, jj = j;
                        int cnt = 0;
                        Character now = field.cells[i][j];
                        while (ii >= 0 && ii < field.size && jj >= 0 && jj < field.size && field.cells[ii][jj] != null && field.cells[ii][jj] == now) {
                            ii += di[m];
                            jj += dj[m];
                            cnt++;
                        }
                        if (cnt == winLen) {
                            if (now == 'X') {
                                phase = "WON_X";
                            } else {
                                phase = "WON_O";
                            }
                            return;
                        }
                    }
                }
            }
            if (free == 0) {
                phase = "DRAW";
            }
        }
    }
}
