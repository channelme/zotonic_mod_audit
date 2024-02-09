%%
%%
%%

-module(mod_admin_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Admin audit functionality").
-mod_description("Support audit log of important actions.").
-mod_prio(1).
-mod_schema(4).
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

    observe_search_query/2,

    observe_tick_1h/2
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

observe_audit_log({audit_log, EventCategory, Props}, Context) ->
    m_audit:log(EventCategory, Props, Context);
observe_audit_log({audit_log, EventCategory, Props, ContentGroupId}, Context) ->
    m_audit:log(EventCategory, Props, z_acl:user(Context), ContentGroupId, Context).

% Search queries
observe_search_query(#search_query{search={audit_unique_logons, Args}}, Context) ->
    audit_unique_logons(Args, Context);

observe_search_query(#search_query{search={audit_summary, Args}}, Context) ->
    audit_query("count(*) as count", Args, Context);

observe_search_query(#search_query{search={audit_search, Args}}, Context) ->
    audit_query("array_agg(audit.id) as audit_ids", Args, Context);

observe_search_query(#search_query{search={audit, Args}}, Context) ->
    audit_query("audit.id", Args, Context);

observe_search_query(#search_query{}, _Context) ->
    undefined.

observe_tick_1h(tick_1h, Context) ->
    m_audit:periodic_cleanup(Context).

audit_unique_logons(Args, _Context) ->
    {date_start, DateStart} = proplists:lookup(date_start, Args),
    {date_end, DateEnd}  = proplists:lookup(date_end, Args),

    #search_sql{select="distinct user_id",
	from="audit audit",
        tables=[{audit, "audit"}],
	where="audit.created >= $1 AND audit.created <= $2",
        args=[DateStart, DateEnd]
    }.

audit_query(Select, Args, Context) ->
    {date_start, DateStart} = proplists:lookup(date_start, Args),
    {date_end, DateEnd}  = proplists:lookup(date_end, Args),

    FilterGroupIds = proplists:get_value(filter_content_groups, Args, []),

    Assoc = proplists:get_value(assoc, Args, true),

    Where = "audit.created >= $1 AND audit.created <= $2",
	
    {Where1, QueryArgs} = case content_groups(FilterGroupIds, Context) of
        all -> {Where, [DateStart, DateEnd]};
        Ids -> {[Where, "AND audit.content_group_id in (SELECT(unnest($3::int[])))"], [DateStart, DateEnd, Ids]}
    end,

    CatExact = get_cat_exact(Args),

    case proplists:get_value(group_by, Args) of
        undefined ->
            Order = get_order(proplists:get_value(sort, Args)),

            #search_sql{select=Select,
                from="audit audit",
                order=Order,
                tables=[{audit, "audit"}],
                cats_exact=CatExact,
                assoc=Assoc,
                where=Where1,
                args=QueryArgs
            };
        GroupBy ->
            {PeriodName, GroupPeriod} = group_parameters(GroupBy, Context),
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

get_order(undefined) -> "audit.created ASC";
get_order("+" ++ Field) -> Field ++ " ASC";
get_order("-" ++ Field) -> Field ++ " DESC".

get_cat_exact(Args) ->
    case proplists:get_all_values(cat_exact, Args) of
        [] -> [];
        Cats -> [{"audit", Cats}]
    end.

%%
%%
content_groups(FilterIds, Context) ->
    R = case z_acl:is_admin(Context) of
       true -> all;
       false ->
           ContentGroupId = m_rsc:p_no_acl(z_acl:user(Context), content_group_id, Context),
           Children = m_hierarchy:children(content_group, ContentGroupId, Context),
           [ContentGroupId | Children]
    end,

    case FilterIds of
        [] -> R;
        _ -> 
	    case R of
	        all -> FilterIds;
                _ -> 
		    S1 = sets:from_list(R),
		    S2 = sets:from_list(FilterIds),
                    sets:to_list(sets:intersection(S1, S2))
            end
    end.


%%
%% Users
%%

observe_auth_logon_done(Event, Context) -> 
    try
        audit(Event, Context)
    catch
        Exception:Reason -> ?LOG("Unexpected error auditing logon_done. ~p:~p", [Exception, Reason])
    end,
    undefined.
observe_auth_logoff_done(Event, Context) -> 
    try
        audit(Event, Context)
    catch
        Exception:Reason -> ?LOG("Unexpected error auditing logoff_done. ~p:~p", [Exception, Reason])
    end,
    undefined.

observe_auth_logon_error(Event, FoldContext, Context) ->
    try
        audit(Event, Context)
    catch
        Exception:Reason -> ?LOG("Unexpected error auditing logon_error. ~p:~p", [Exception, Reason])
    end,
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

group_parameters(user, _Context)  ->
    {"user_id", "user_id"};
group_parameters(Period, Context) when Period =:= day orelse Period =:= week orelse Period =:= month  ->
    group_period_at_tz(Period, z_convert:to_list(z_context:tz(Context)));
group_parameters(GroupOn, _Context) when is_atom(GroupOn)  ->
    case is_column_name(GroupOn) of
        true ->
            S = z_convert:to_list(GroupOn),
            {S, S};
        false ->
            %% This is not one of the column names. Assume it is a property in the
            %% props_json field.
            GroupOnName = z_convert:to_list(GroupOn),
            S = "props_json->'" ++ GroupOnName ++ "'",
            {S, S ++ " as " ++ GroupOnName}
    end.

is_column_name(user_id) -> true;
is_column_name(category_id) -> true;
is_column_name(content_group_id) -> true;
is_column_name(ua_id) -> true;
is_column_name(ip_address) -> true;
is_column_name(_) -> false.


group_period_at_tz(day, TZ) when is_list(TZ) ->
     {"iso_date", "(extract(year from (created at time zone '" ++ TZ ++ "'))::int, extract(month from (created at time zone '" ++ TZ ++ "'))::int, extract(day from created)::int) as iso_date"} ;
group_period_at_tz(week, TZ) when is_list(TZ) ->
     {"iso_week", "(extract(isoyear from (created at time zone '" ++ TZ ++ "'))::int, extract(week from (created at time zone '" ++ TZ ++ "'))::int) as iso_week"} ;
group_period_at_tz(month, TZ) when is_list(TZ) ->
    {"iso_month", "(extract(year from (created at time zone '" ++ TZ ++ "'))::int, extract(month from (created at time zone '" ++ TZ ++ "'))::int) as iso_month"}.

