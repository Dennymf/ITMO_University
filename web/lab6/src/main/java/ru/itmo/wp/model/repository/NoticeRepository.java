package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Notice;

import java.util.List;

public interface NoticeRepository {
    List<Notice> findAll();
}
