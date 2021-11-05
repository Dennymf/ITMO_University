package ru.itmo.wp.model.service;

import ru.itmo.wp.model.domain.Notice;
import ru.itmo.wp.model.repository.NoticeRepository;
import ru.itmo.wp.model.repository.impl.NoticeRepositoryImpl;

import java.util.List;

public class NoticeService {
    private final NoticeRepository noticeRepository = new NoticeRepositoryImpl();

    public List<Notice> findAll(){
        return noticeRepository.findAll();
    }
}
