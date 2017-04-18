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
    log/2, log/3
]).

-include_lib("zotonic.hrl").

-define(MAXAGE_UA_STRING, 7200).

%% @doc Fetch the value for the key from a model source %% @spec m_find_value(Key, Source, Context) -> term() m_find_value(_Id, _Context) -> undefined.
m_find_value(Id, #m{value=undefined}=M, _Context) ->
    M#m{value=Id};
m_find_value(_Key, #m{value=Id}=_M, _Context) when is_integer(Id) ->
    undefined.

%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined;
<<<<<<< HEAD
m_value(#m{value=Id}, Context) ->
    get_visible(Id, Context).
=======
m_value(#m{value=Id}, _Context) ->
    undefined. %get_visible(Id, Context);
>>>>>>> 4d244c66a163798bc9c631c3c6da5fb350605a88


%%
%% Api
%%

<<<<<<< HEAD
get_visible(_Id, _Context) ->
    %% TODO, return the visible properties of this audit item.
    [].
=======
p(Id, Property, Context) ->
    Property.
>>>>>>> 4d244c66a163798bc9c631c3c6da5fb350605a88

log(EventCategory, Context) ->
    log(EventCategory, [], Context).

log(EventCategory, Props, Context) when not is_integer(EventCategory) ->
    case EventCatId = m_rsc:rid(EventCategory, Context) of
        undefined ->
            lager:info("Category ~p not defined, action not audited.", [EventCategory]);
        EventCatId -> log(EventCatId, Props, Context)
    end;

log(EventCatId, Props, Context) when is_integer(EventCatId) ->
    {UserId, ContentGroupId} = case z_acl:user(Context) of
        undefined -> {undefined, undefined};
        Id -> 
            CGId = m_rsc:p_no_acl(Id, content_group_id, Context),
            {Id, CGId}
    end,

    IpAddress = ip_address(Context),
    UserAgent = z_context:get_req_header("user-agent", Context),
    UaId = user_agent_id(UserAgent, Context),

    z_db:insert(audit,[{category_id, EventCatId}, 
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

manage_schema(install, Context) ->
    z_db:create_table(user_agent, [
        #column_def{name=id, type="serial", is_nullable=false, primary_key=true},
        #column_def{name=text, type="character varying(500)", is_nullable=false},
        #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
    ], Context),

    z_db:equery("alter_table user_agent add constraint unique (text)", Context),
    z_db:equery("create index ua_text on user_agent(text)", Context),

    z_db:create_table(audit, [
        #column_def{name=id, type="serial", is_nullable=false, primary_key=true},
        #column_def{name=category_id, type="integer", is_nullable=false},
        #column_def{name=props, type="bytea", is_nullable=true}, 
        #column_def{name=user_id, type="integer", is_nullable=true},
        #column_def{name=content_group_id, type="integer", is_nullable=false},
        #column_def{name=ua_id, type="integer", is_nullable=true},
        #column_def{name=ip_address, type="character varying(40)", is_nullable=true},
        #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
    ], Context),

    z_db:equery("alter_table audit add constraint foreign key (category_id) references rsc(id) on update cascade on delete cascade", Context),

    z_db:equery("alter_table audit add constraint foreign key (user_id) references rsc(id) on update cascade on delete set null", Context),
    z_db:equery("alter_table audit add constraint foreign key (content_group_id) references rsc(id) on update cascade on delete cascade", Context),
    z_db:equery("alter_table audit add constraint foreign key (ua_id) references user_agent(id) on update cascasde on delete set null", Context),

    ok.
