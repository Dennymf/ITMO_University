package ru.itmo.wp.servlet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;

public class StaticServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String uri = request.getRequestURI();
        String[] allUri = uri.split("\\+");
        String allType = getContentTypeFromName(allUri[0]);

        for(String tempUri : allUri) {
            File file = new File("С:/Educate/WEB/lab3/src/main/webapp/static/" + tempUri);
            if (file.isFile()) {
                response.setContentType(allType);
                OutputStream outputStream = response.getOutputStream();
                Files.copy(file.toPath(), outputStream);
                outputStream.flush();
            } else {
                File file1 = new File(getServletContext().getRealPath("/static" + tempUri));
                if (file1.isFile()) {
                    response.setContentType(allType);
                    OutputStream outputStream = response.getOutputStream();
                    Files.copy(file1.toPath(), outputStream);
                    outputStream.flush();
                } else {
                    response.sendError(HttpServletResponse.SC_NOT_FOUND);
                }
            }
        }

    }



    private String getContentTypeFromName(String name) {
        name = name.toLowerCase();

        if (name.endsWith(".png")) {
            return "image/png";
        }

        if (name.endsWith(".jpg")) {
            return "image/jpeg";
        }

        if (name.endsWith(".html")) {
            return "text/html";
        }

        if (name.endsWith(".css")) {
            return "text/css";
        }

        if (name.endsWith(".js")) {
            return "application/javascript";
        }

        throw new IllegalArgumentException("Can't find content type for '" + name + "'.");
    }
}
