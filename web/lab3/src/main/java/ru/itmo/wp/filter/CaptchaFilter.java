package ru.itmo.wp.filter;

import ru.itmo.wp.util.ImageUtils;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.ServletException;
import javax.servlet.RequestDispatcher;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

public class CaptchaFilter implements Filter {
    private FilterConfig filterConfig;


    @Override
    public void destroy() {
        filterConfig = null;
    }

    @Override
    public void init(FilterConfig fConfig) {
        filterConfig = fConfig;
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {
        if (filterConfig.getInitParameter("active").equalsIgnoreCase("true")) {
            HttpServletRequest request = (HttpServletRequest) servletRequest;
            HttpSession session = request.getSession();
            int number = (int) (Math.random() * 900 + 100);
            byte[] captcha = ImageUtils.toPng(Integer.toString(number));
            session.setAttribute("number", number);



        }
    }


}
