-module(m_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").


-export([
     m_find_value/3,
     m_to_list/2,
     m_value/2
]).

-export([
    user_agent_id/2,
    manage_schema/2,
    log/2, log/3, log/5
]).

-export([
    p/3,
    p_no_acl/3,
    is_a/3,
    get/2,
    get_visible/2,
    get_raw/2
]).

-include_lib("zotonic.hrl").

-define(MAXAGE_UA_STRING, 7200).

%% @doc Fetch the value for the key from a model source %% @spec m_find_value(Key, Source, Context) -> term() m_find_value(_Id, _Context) -> undefined.
m_find_value(Id, #m{value=undefined}=M, _Context) ->
    M#m{value=Id};
m_find_value(Key, #m{value=Id}, Context) when is_integer(Id) ->
    p(Id, Key, Context).

%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined;
m_value(#m{value=Id}, Context) ->
    get_visible(Id, Context).

%%
%% Api
%%
p(Id, Predicate, Context) ->
    case z_acl:is_allowed(view_audit_event, Id, Context) of
        true -> p_no_acl(Id, Predicate, Context);
        _ -> undefined
    end.

p_no_acl(Id, Predicate, Context) ->
    proplists:get_value(Predicate, get(Id, Context), undefined).

get(Id, Context) when is_integer(Id) ->
    z_depcache:memo(fun() -> get_raw(Id, Context) end, {audit, Id}, ?DAY, Context).

get_raw(Id, Context) ->
    SQL = <<"SELECT * FROM audit WHERE id = $1">>,
    z_db:assoc_props_row(SQL, [Id], Context).

get_visible(Id, Context) ->
    case z_acl:is_allowed(view_audit_event, Id, Context) of
        true -> get(Id, Context);
        _ -> []
    end.

is_a(Id, CatId, Context) ->
     p_no_acl(Id, category_id, Context) =:= CatId.

%%
%% Log audit events
%%

log(EventCategory, Context) ->
    log(EventCategory, [], Context).

log(EventCat, Props, Context) ->
    {UserId, ContentGroupId} = case z_acl:user(Context) of
        undefined ->
            SystemContentGroupId = m_rsc:rid(system_content_group, Context),
            {undefined, SystemContentGroupId};
        Id ->
            CGId = m_rsc:p_no_acl(Id, content_group_id, Context),
            {Id, CGId}
    end,

    log(EventCat, Props, UserId, ContentGroupId, Context).

log(EventCat, Props, UserId, ContentGroupId, #context{}=Context) when not is_integer(EventCat) ->
    case EventCatId = m_rsc:rid(EventCat, Context) of
        undefined ->
            lager:info("Category ~p not defined, action not audited.", [EventCat]);
        EventCatId -> log(EventCatId, Props, UserId, ContentGroupId, Context)
    end;
log(EventCatId, Props, UserId, ContentGroupId, #context{}=Context) ->
    IpAddress = ip_address(Context),
    UserAgent = z_context:get_req_header("user-agent", Context),
    UaId = user_agent_id(UserAgent, Context),

    {ok, _} = z_db:insert(audit, [{category_id, EventCatId},
                        {user_id, UserId},
                        {content_group_id, ContentGroupId},
                        {ip_address, IpAddress},
                        {ua_id, UaId} | Props], Context).

ip_address(Context) ->
    case z_context:get_reqdata(Context) of
       undefined -> undefined;
       ReqData -> wrq:peer(ReqData)
    end.

user_agent_id(undefined, _Context) -> undefined;
user_agent_id(UserAgent, Context) ->
    BUA = z_convert:to_binary(UserAgent),
    F = fun(Ctx) ->
        case z_db:q("select id from user_agent where text = $1", [BUA], Ctx) of
            [] ->
                {ok, Id} = z_db:insert(user_agent, [{text, BUA}], Ctx),
                Id;
            [{Id}] -> Id
       end
    end,
    Transaction = fun() -> z_db:transaction(F, Context) end,
    z_depcache:memo(Transaction, {user_agent_id, BUA}, ?MAXAGE_UA_STRING, Context).

manage_schema({upgrade, 2}, Context) ->
    {ok, _, _} = z_db:equery("create index fki_audit_category_id on audit(category_id)", Context),
    {ok, _, _} = z_db:equery("create index fki_audit_user_id on audit(user_id)", Context),
    {ok, _, _} = z_db:equery("create index fki_audit_content_group_id on audit(content_group_id)", Context),
    {ok, _, _} = z_db:equery("create index fki_audit_created on audit(created)", Context),

    ok;

manage_schema(install, Context) ->
    ok = z_db:create_table(user_agent, [
        #column_def{name=id, type="serial", is_nullable=false, primary_key=true},
        #column_def{name=text, type="character varying(500)", is_nullable=false},
        #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
    ], Context),

    {ok, _, _} = z_db:equery("alter table user_agent add constraint uc_text unique (text)", Context),
    {ok, _, _} = z_db:equery("create index ua_text on user_agent(text)", Context),

    ok = z_db:create_table(audit, [
        #column_def{name=id, type="serial", is_nullable=false, primary_key=true},
        #column_def{name=category_id, type="integer", is_nullable=false},
        #column_def{name=props, type="bytea", is_nullable=true}, 
        #column_def{name=user_id, type="integer", is_nullable=true},
        #column_def{name=content_group_id, type="integer", is_nullable=false},
        #column_def{name=ua_id, type="integer", is_nullable=true},
        #column_def{name=ip_address, type="character varying(40)", is_nullable=true},
        #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
    ], Context),

    {ok, _, _} = z_db:equery("alter table audit add constraint fk_audit_category_id foreign key (category_id) references rsc(id) on update cascade on delete cascade", Context),
    {ok, _, _} = z_db:equery("alter table audit add constraint fk_audit_user_id foreign key (user_id) references rsc(id) on update cascade on delete set null", Context),
    {ok, _, _} = z_db:equery("alter table audit add constraint fk_audit_content_group_id foreign key (content_group_id) references rsc(id) on update cascade on delete cascade", Context),
    {ok, _, _} = z_db:equery("alter table audit add constraint fk_audit_ua_id foreign key (ua_id) references user_agent(id) on update cascade on delete set null", Context),

    %% Create the missing indexes
    ok = manage_schema({upgrade, 2}, Context),

    ok.
