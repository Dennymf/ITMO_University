package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.sql.SQLException;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class RegisterPage extends Page {
    private final UserService userService = new UserService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    private void register(HttpServletRequest request, Map<String, Object> view) throws ValidationException, SQLException {
        User user = new User();
        UserRepository userRepository = new UserRepositoryImpl();
        user.setLogin(request.getParameter("login"));
        user.setEmail(request.getParameter("email"));
        String password = request.getParameter("password");
        String passwordConfirmation = request.getParameter(("passwordConfirmation"));

        userService.validateRegistration(user, password, passwordConfirmation);
        userService.register(user, password);

        request.getSession().setAttribute("message", "You are successfully registered!");
        throw new RedirectException("/index");
    }
}
