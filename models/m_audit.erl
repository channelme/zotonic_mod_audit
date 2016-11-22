-module(m_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").


-export([
     m_find_value/3,
     m_to_list/2,
     m_value/2
]).

-export([
    user_agent_id/2,
    manage_schema/2
]).

-include_lib("zotonic.hrl").

-define(MAXAGE_UA_STRING, 7200).

%% @doc Fetch the value for the key from a model source %% @spec m_find_value(Key, Source, Context) -> term() m_find_value(_Id, _Context) -> undefined.
m_find_value(_Id, _Source, _Context) ->
    undefined.

%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.

%%
%% Api
%%


user_agent_id(undefined, Context) -> undefined;
user_agent_id(UserAgent, Context) ->
    BUA = z_convert:to_binary(UserAgent),
    F = fun() ->
        case z_db:q("select id from user_agent where text = $1", [BUA], Context) of
            [] ->
                {ok, Id} = z_db:insert(user_agent, [{text, BUA}], Context),
                Id;
            [{Id}] -> Id
       end
    end,
    z_depcache:memo(F, {user_agent_id, BUA}, ?MAXAGE_UA_STRING, Context).

manage_schema(install, Context) ->
    z_db:create_table(user_agent, [
        #column_def{name=id, type="serial", is_nullable=false, primary_key=true},
        #column_def{name=text, type="character varying(500)", is_nullable=false},
        #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
    ], Context),

    z_db:equery("alter_table user_agent add constraint unique (text)", Context),
    z_db:equery("create index ua_text on user_agent(text)", Context),

    z_db:create_table(audit_trail, [
        #column_def{name=id, type="serial", is_nullable=false, primary_key=true},
        #column_def{name=props, type="bytea", is_nullable=false}, 
        #column_def{name=user_id, type="integer", is_nullable=true}
        #column_def{name=ua_id, type="integer", is_nullable=true},
        #column_def{name=ip_address, type="character varying(40)", is_nullable=true},
        #column_def{name=created, type="timestamp with time zone", is_nullable=false}
    ], Context),

    z_db:equery("alter_table audit_trail add constraint foreign key (user_id) references rsc(id) on update cascade on delete set null", Context),
    z_db:equery("alter_table audit_trail add constraint foreign key (ua_id) references user_agent(id) on update delete on delete set null", Context),

    ok.
