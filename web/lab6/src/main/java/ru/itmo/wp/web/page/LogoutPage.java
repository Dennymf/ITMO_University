package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.EventType;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.EventService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class LogoutPage extends Page {
    public void action(HttpServletRequest request, Map<String, Object> view) {
        final EventService eventService = new EventService();
        User user = (User) view.get("user");
        Event event = eventService.getEvent(user.getId(), EventType.LOGOUT);
        eventService.sendEvent(event);

        request.getSession().removeAttribute("user");

        setMessage(request, view, "Good bye. Hope to see you soon!");
        throw new RedirectException("/index");
    }



}
