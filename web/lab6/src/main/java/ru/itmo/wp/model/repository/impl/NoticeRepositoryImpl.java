package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Notice;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.NoticeRepository;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class NoticeRepositoryImpl implements NoticeRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    public List<Notice> findAll() {
        List<Notice> notices = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM Notice WHERE hidden=0 ORDER BY id DESC")) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    Notice notice;
                    while ((notice = toNotice(statement.getMetaData(), resultSet)) != null) {
                        notices.add(notice);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find Notice.", e);
        }

        return notices;
    }

    private Notice toNotice(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Notice notice = new Notice();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    notice.setId(resultSet.getLong(i));
                    break;
                case "text":
                    notice.setText(resultSet.getString(i));
                    break;
                case "hidden":
                    notice.setHidden(resultSet.getBoolean(i));
                    break;
                default:
                    // No operations.
            }
        }

        return notice;
    }
}