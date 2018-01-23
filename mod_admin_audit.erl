%%
%%
%%

-module(mod_admin_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Admin audit functionality").
-mod_description("Support audit log of important actions.").
-mod_prio(1).
-mod_schema(3).
-mod_depends([admin, menu]).
-mod_provides([audit]).

-export([manage_schema/2, datamodel/0]).

-export([
    observe_auth_logon_done/2,
    observe_auth_logoff_done/2,

    observe_auth_logon_error/3,

    observe_rsc_insert/3,
    observe_rsc_update_done/2,
    observe_rsc_delete/2,

    observe_audit_log/2,

    observe_search_query/2
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

observe_audit_log({audit_log, EventCategory, Props}, Context) ->
    m_audit:log(EventCategory, Props, Context);
observe_audit_log({audit_log, EventCategory, Props, ContentGroupId}, Context) ->
    m_audit:log(EventCategory, Props, z_acl:user(Context), ContentGroupId, Context).

observe_search_query(#search_query{search={audit_summary, Args}}, Context) ->
    audit_query("count(*) as count", Args, Context);

observe_search_query(#search_query{search={audit_search, Args}}, Context) ->
    audit_query("array_agg(audit.id) as audit_ids", Args, Context);

observe_search_query(#search_query{search={audit, Args}}, Context) ->
    audit_query("audit.id", Args, Context);

observe_search_query(#search_query{}, _Context) ->
    undefined.

audit_query(Select, Args, Context) ->
    {date_start, DateStart} = proplists:lookup(date_start, Args),
    {date_end, DateEnd}  = proplists:lookup(date_end, Args),

    Assoc = proplists:get_value(assoc, Args, true),

    Where = "audit.created >= $1 AND audit.created <= $2",

    {Where1, QueryArgs} = case content_groups(Context) of
        all -> {Where, [DateStart, DateEnd]};
        Ids -> {[Where, "AND audit.content_group_id in (SELECT(unnest($3::int[])))"], [DateStart, DateEnd, Ids]}
    end,

    CatExact = get_cat_exact(Args),

    case proplists:get_value(group_by, Args) of
        undefined ->
            #search_sql{select=Select,
                from="audit audit",
                order="audit.created ASC",
                tables=[{audit, "audit"}],
                cats_exact=CatExact,
                assoc=Assoc,
                where=Where1,
                args=QueryArgs
            };
        GroupBy ->
            {PeriodName, GroupPeriod} = group_period(GroupBy, Context),
            #search_sql{select=Select ++ ", " ++ GroupPeriod,
                from="audit audit",
                group_by=PeriodName,
                order=PeriodName ++ " ASC",
                tables=[{audit, "audit"}],
                cats_exact=CatExact,
                assoc=Assoc,
                where=Where1,
                args=QueryArgs
            }
    end.

get_cat_exact(Args) ->
    case proplists:get_all_values(cat_exact, Args) of
        [] -> [];
        Cats -> [{"audit", Cats}]
    end.

%%
%%
content_groups(Context) ->
    case z_acl:is_admin(Context) of
       true -> all;
       false ->
           ContentGroupId = m_rsc:p_no_acl(z_acl:user(Context), content_group_id, Context),
           Children = m_hierarchy:children(content_group, ContentGroupId, Context),
           [ContentGroupId | Children]
    end.

%%
%% Users
%%

observe_auth_logon_done(Event, Context) -> audit(Event, Context), undefined.
observe_auth_logoff_done(Event, Context) -> audit(Event, Context), undefined.

observe_auth_logon_error(Event, FoldContext, Context) ->
    ?DEBUG(hier),
    audit(Event, Context),
    FoldContext.

%%
%% Resource mutation
%%

observe_rsc_insert(Event, Args, Context) -> audit({Event, Args}, Context), Args.
observe_rsc_update_done(Event, Context) -> audit(Event, Context), undefined.
observe_rsc_delete(Event, Context) -> audit(Event, Context), undefined.

%%
%%
%%
audit(auth_logon_done, Context) -> m_audit:log(logon, Context);
audit(auth_logoff_done, Context) -> m_audit:log(logoff, Context);
audit(#auth_logon_error{reason="pw"}, Context) ->
    Username = z_context:get_q(username, Context),
    case m_identity:lookup_by_username(Username, Context) of
        undefined ->
            %% Probably a wrong password.
            m_audit:log(logon_error, [{username, Username}, {reason, no_user}], Context);
        IdentityProps ->
            Id = proplists:get_value(rsc_id, IdentityProps),
            ContentGroupId = m_rsc:p_no_acl(Id, content_group_id, Context),
            m_audit:log(logon_error, [{reason, password}], Id, ContentGroupId, Context)
    end;

audit(_Event, _Context) ->
    ok.


%%
%% Database
%%

manage_schema(Version, Context) ->
    m_audit:manage_schema(Version, Context),
    datamodel().

datamodel() ->
    #datamodel{
       categories = [ 
           {audit_event, meta, [{title, <<"Audit Event">>}]},
           {auth_event, audit_event, [{title, <<"Authorization Event">>}]},
           {logon, auth_event, [{title, <<"Login">>}]},
           {logoff, auth_event, [{title, <<"Logoff">>}]},
           {logon_error, auth_event, [{title, <<"Logon Error">>}]}
       ],
       resources = [ ]
    }.

%%
%% Helpers
%%

group_period(user, _Context)  ->
    {"user_id", "user_id"};
group_period(Period, Context)  ->
    group_period_at_tz(Period, z_convert:to_list(z_context:tz(Context))).


group_period_at_tz(day, TZ) when is_list(TZ) ->
     {"iso_date", "(extract(year from (created at time zone '" ++ TZ ++ "'))::int, extract(month from (created at time zone '" ++ TZ ++ "'))::int, extract(day from created)::int) as iso_date"} ;
group_period_at_tz(week, TZ) when is_list(TZ) ->
     {"iso_week", "(extract(isoyear from (created at time zone '" ++ TZ ++ "'))::int, extract(week from (created at time zone '" ++ TZ ++ "'))::int) as iso_week"} ;
group_period_at_tz(month, TZ) when is_list(TZ) ->
    {"iso_month", "(extract(year from (created at time zone '" ++ TZ ++ "'))::int, extract(month from (created at time zone '" ++ TZ ++ "'))::int) as iso_month"}.



