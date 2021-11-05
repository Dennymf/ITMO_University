<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#-- @ftlvariable name="user_id" type="java.lang.String" -->

<#import "./common.ftlh" as c>

<#assign u = 0/>
<@c.page>
    <#if handle??>
        <#assign u = c.findBy(users, "handle", handle)!/>
        <@f u/>
    <#elseif user_id?? && user_id?is_number>
        <#assign u = c.findBy(users, "id", user_id)!/>
        <@f u/>
    <#else>
        No such user
    </#if>
</@c.page>


<#macro f u>
    <#if u?has_content>
            <@userInfo u users?seq_index_of(u)/>
        <#else>
            No such user
        </#if>
</#macro>

<#macro userInfo u i>
    <div class="user">
        <main>
            <header>
                <a href="#">${u.name}</a>
            </header>
            <ul>
                <li>id : ${u.id}</li>
                <li>handle : ${u.handle}</li>
                <li>name : ${u.name}</li>
            </ul>
        </main>
        <img src="../../../img/cfUser.jpg">
    </div>
    <div class="arrows">
        <#if i == 0>
           <div class="arrow non_arrow">←</div>
        <#else>
            <#assign prevU = users[i - 1]!/>
            <a href="../user?handle=${prevU.handle}" class="arrow">←</a>
        </#if>
        <#if i == users?size - 1>
            <div class="arrow non_arrow">→</div>
        <#else>
            <#assign nextU = users[i + 1]!/>
            <a href="../user?handle=${nextU.handle}" class="arrow">→</a>
        </#if>
    </div>
</#macro>