package ru.itmo.web.lesson4.util;

import ru.itmo.web.lesson4.model.Color;
import ru.itmo.web.lesson4.model.Post;
import ru.itmo.web.lesson4.model.User;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class DataUtil {
    private static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirzayanov", "Mike Mirzayanov", Color.RED),
            new User(6, "pashka", "Pavel Mavrin", Color.BLUE),
            new User(9, "geranazarov555", "Georgiy Nazarov", Color.GREEN),
            new User(11, "tourist", "Gennady Korotkevich", Color.RED)
    );

    private static final List<Post> POSTS = Arrays.asList(
            new Post(1, "Трансляции на Codeforces", "Всем привет!\n" +
                    "\n" +
                    "Сегодня на Codeforces появилась поддержка трансляций (стримов)! Спасибо Геннадию tourist Короткевичу за отличную идею!\n" +
                    "\n" +
                    "Поддерживаются две популярные платформы — Twitch и YouTube. Пока возможность добавлять трансляции на Codeforces открыта красным участникам и по индивидуальным приглашениям. Позднее, мы, возможно, откроем эту возможность большему числу пользователей.\n" +
                    "\n" +
                    "Идея состоит в том, чтобы стримеры могли анонсировать свои трансляции на аудиторию Codeforces. Для этого трансляцию нужно добавить на сайт, используя специальный раздел в профиле. Обратите внимание, что при добавлении Twitch-трансляции вам надо указать просто URL вашего канала, а при добавлении YouTube-трансляции — её уникальную короткую ссылку (ссылку на видео youtu.be/…).\n" +
                    "\n" +
                    "Незадолго до старта трансляции в сайдбаре справа появится уведомление о предстоящей трансляции, которое будут видеть все посетители сайта.\n" +
                    "\n" +
                    "Если вы указали название трансляции только на русском языке, то подразумевается, что рабочий язык трансляции — русский, и она будет видна исключительно в русском интерфейсе.\n" +
                    "\n" +
                    "Например, прямо сейчас в русскоязычном интерфейсе есть анонс лекции Павла pashka Маврина в рамках его занятий в ИТМО по теме \"игры на графах\".",
                    9),
            new Post(2, "Первая командная интернет-олимпиада, Сезон 2020-21", "Всем привет!\n" +
                    "\n" +
                    "18 октября 2020 года в 11:00 состоится первая командная интернет-олимпиада для школьников. Приглашаем вас принять в ней участие!\n" +
                    "\n" +
                    "Продолжительность олимпиады — 5 часов. Не забудьте зарегистрироваться на цикл командных интернет-олимпиад в этом сезоне перед началом олимпиады. Обратите внимание, что для участия в командных олимпиадах, нужно зарегистрировать команду (по ссылке \"Новая команда\"). Команда может содержать от 1 до 3 человек. Дополнительную информацию, а также расписание всех предстоящих командных интернет-олимпиад можно посмотреть на страничке интернет-олимпиад.\n" +
                    "\n" +
                    "Условия появятся на сайте в момент начала олимпиады. Тестирующая система находится по адресу pcms.itmo.ru.\n" +
                    "\n" +
                    "Олимпиаду для вас подготовили Николай Будин (Nebuchadnezzar), Михаил Иванов (orz), Михаил Анопренко (manoprenko), Даниил Орешников (doreshnikov), Дмитрий Гнатюк (ima_ima_go), Ильдар Загретдинов (SemBarner22).",
                    6),
            new Post(3, "Educational Codeforces Round 96 [рейтинговый для Div. 2]", "Привет, Codeforces!\n" +
                    "\n" +
                    "В воскресенье, 11 октября 2020 г. в 12:05 состоится Educational Codeforces Round 96 (рейтинговый для Див. 2). Обратите внимание на необычное время старта раунда.\n" +
                    "\n" +
                    "Продолжается серия образовательных раундов в рамках инициативы Harbour.Space University! Подробности о сотрудничестве Harbour.Space University и Codeforces можно прочитать в посте.\n" +
                    "\n" +
                    "Этот раунд будет рейтинговым для участников с рейтингом менее 2100. Соревнование будет проводиться по немного расширенным правилам ICPC. Штраф за каждую неверную посылку до посылки, являющейся полным решением, равен 10 минутам. После окончания раунда будет период времени длительностью в 12 часов, в течение которого вы можете попробовать взломать абсолютно любое решение (в том числе свое). Причем исходный код будет предоставлен не только для чтения, но и для копирования.\n" +
                    "\n" +
                    "Вам будет предложено 6 или 7 задач на 2 часа. Мы надеемся, что вам они покажутся интересными.\n" +
                    "\n" +
                    "Задачи вместе со мной придумывали и готовили Адилбек adedalic Далабаев и Александр fcspartakm Фролов. Также большое спасибо Михаилу MikeMirzayanov Мирзаянову за системы Polygon и Codeforces.\n" +
                    "\n" +
                    "Удачи в раунде! Успешных решений!",
                    1),
            new Post(4, "Codeforces Global Round 11", "Hi!\n" +
                    "\n" +
                    "On суббота, 10 октября 2020 г. в 17:50 we will host Codeforces Global Round 11.\n" +
                    "\n" +
                    "This is the fifth round of the 2020 series of Codeforces Global Rounds. The rounds are open and rated for everybody.\n" +
                    "\n" +
                    "The prizes for this round are as follows:\n" +
                    "\n" +
                    "30 best participants get a t-shirt.\n" +
                    "20 t-shirts are randomly distributed among those with ranks between 31 and 500, inclusive.\n" +
                    "The prizes for the 6-round series in 2020:\n" +
                    "\n" +
                    "In each round top-100 participants get points according to the table.\n" +
                    "The final result for each participant is equal to the sum of points he gets in the four rounds he placed the highest.\n" +
                    "The best 20 participants over all series get sweatshirts and place certificates.\n" +
                    "Thanks to XTX, which in 2020 supported the global rounds initiative!\n" +
                    "\n" +
                    "Problems for this round are set by me. Thanks a lot to the coordinator antontrygubO_o, to the testers dacin21, Giada, H4CKOM, DimmyT, postscript, oolimry, RedDreamer, PM11, Tlatoani, growup974, nvmdava, bhaskarjoshi2001, dorijanlendvaj, and to MikeMirzayanov for the Codeforces and Polygon platforms.\n" +
                    "\n" +
                    "The round will have 8 problems and will last 180 minutes.",
                    11)
    );

    public static void addData(HttpServletRequest request, Map<String, Object> data) {
        data.put("users", USERS);
        data.put("posts", POSTS);

        for (User user : USERS) {
            if (Long.toString(user.getId()).equals(request.getParameter("logged_user_id"))) {
                data.put("user", user);
            }
        }

    }
}
