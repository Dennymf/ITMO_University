package ru.itmo.wp.web.page;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Notice;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;
import ru.itmo.wp.model.service.NoticeService;
import ru.itmo.wp.model.service.UserService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class Page {
    private final NoticeService noticeService = new NoticeService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
    }

    private final UserRepository userRepository = new UserRepositoryImpl();

    public void before(HttpServletRequest request, Map<String, Object> view) {
        setUser(request, view);
        view.put("notices", noticeService.findAll());
        view.put("userCount", userRepository.findCount());
    }
    public void after(HttpServletRequest request, Map<String, Object> view) {
        view.put("userCount", userRepository.findCount());
    }

    protected void setMessage(HttpServletRequest request, Map<String, Object> view, String message) {
        request.getSession().setAttribute("message", message);
    }
    protected void setUser(HttpServletRequest request, Map<String, Object> view) {
        User user = (User) request.getSession().getAttribute("user");
        if (user != null) {
            view.put("user", user);
        }
    }

}
