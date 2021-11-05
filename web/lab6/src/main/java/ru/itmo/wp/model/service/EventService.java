package ru.itmo.wp.model.service;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.EventType;
import ru.itmo.wp.model.repository.EventRepository;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.EventRepositoryImpl;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

public class EventService {

    private final EventRepository eventRepository = new EventRepositoryImpl();

    public Event getEvent(long userId, EventType type) {
        Event event = new Event();
        event.setUserId(userId);
        event.setType(type);
        return event;
    }

    public void sendEvent(Event event) {
        eventRepository.save(event);
    }
}
