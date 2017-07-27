%%
%%
%%

-module(mod_admin_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Admin audit functionality").
-mod_description("Support audit log of important actions.").
-mod_prio(1).
-mod_schema(1).
-mod_depends([admin, menu]).
-mod_provides([audit]).


-export([manage_schema/2, datamodel/0]).

-export([
    observe_auth_logon_done/2,
    observe_auth_logoff_done/2,

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


observe_search_query(#search_query{search={audit_summary, Args}}, _Context) ->
    %% The number of weeks we have to look back.
    {PeriodName, GroupPeriod} = group_period(proplists:get_value(group_by, Args)),

    #search_sql{select="count(*) as count, " ++ GroupPeriod,
        from="audit audit",
        group_by=PeriodName,
        order=PeriodName ++ " ASC",
        tables=[{rsc, "audit"}],
        assoc=true
    };

observe_search_query(#search_query{search={audit_search, Args}}, Context) ->
    User = z_acl:user(Context),
    ?DEBUG(m_rsc:p_no_acl(User, content_group_id, Context)),
    ?DEBUG(Args),

    {PeriodName, GroupPeriod} = group_period(proplists:get_value(group_by, Args)),

    Q = #search_sql{select="array_agg(audit.id) as audit_ids, " ++ GroupPeriod,
        from="audit audit",
        group_by=PeriodName,
        order=PeriodName ++ " ASC",
        tables=[{audit, "audit"}],
        assoc=true
    },

    Q;

    %% ?DEBUG(search_query:parse_query([{cat_exact, conversation_closed}], Context, Q));

observe_search_query(#search_query{}, _Context) ->
    undefined.


%%
%% Users
%%

observe_auth_logon_done(Event, Context) ->
    audit(Event, Context),
    undefined.

observe_auth_logoff_done(Event, Context) ->
    audit(Event, Context),
    undefined.

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
           {logoff, auth_event, [{title, <<"Logoff">>}]}
       ],
       resources = [ ]
    }.

%%
%% Helpers
%%

group_period(week) ->
     {"iso_week", "(extract(year from created)::int, extract(week from created)::int) as iso_week"} ;
group_period(month) ->
    {"iso_month", "(extract(year from created)::int, extract(month from created)::int) as iso_month"}.



