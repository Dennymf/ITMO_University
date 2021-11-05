<#-- @ftlvariable name="posts" type="ru.itmo.web.lesson4.model.Post[]" -->
<#-- @ftlvariable name="user_id" type="java.lang.String" -->
<#import "./common.ftlh" as c>

<@c.page>
        <#list posts?reverse as p>
            <#assign u = c.findBy(users, "id", p.user_id)!/>
            <article>
                <div class="title">${p.title}</div>
                <div class="information">By ${u.name}, 2 days ago, translation</div>
                <div class="body">
                   ${p.text}
                </div>
                <ul class="attachment">
                    <li>Announcement of <a>Codeforces Round #510 (Div. 1)</a></li>
                    <li>Announcement of <a>Codeforces Round #510 (Div. 2)</a></li>
                </ul>
                <div class="footer">
                    <div class="left">
                        <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                        <span class="positive-score">+173</span>
                        <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
                    </div>
                    <div class="right">
                        <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                        2 days ago
                        <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                        <a href="#">68</a>
                    </div>
                </div>
            </article>
        </#list>

</@c.page>
