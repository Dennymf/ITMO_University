package ru.itmo.wp.servlet;

import com.google.gson.Gson;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class MessageServlet extends HttpServlet {
    List<String> messages = new ArrayList<>();

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String uri = request.getRequestURI();
        HttpSession session = request.getSession();

        response.setContentType("application/json");


        if (uri.endsWith("auth")) {
            String userName = request.getParameter("user");
            session.setAttribute("name", userName);

            response.getWriter().print(new Gson().toJson(userName));
            response.getWriter().flush();
        }

        if (uri.endsWith("findAll")) {
            response.getWriter().print(messages);
            response.getWriter().flush();
        }

        if (uri.endsWith("add")) {
            String name = (String) session.getAttribute("name");
            String text = request.getParameter("text");
            messages.add(new Gson().toJson(new Message(name, text)));
        }

    }

}

class Message {
    public Message(final String user, final String text) {
        this.user = user;
        this.text = text;
    }
    private final String user;
    private final String text;
}
