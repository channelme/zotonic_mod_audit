-module(m_ua).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").


-export([
     m_find_value/3,
     m_to_list/2,
     m_value/2
]).
-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source %% @spec m_find_value(Key, Source, Context) -> term() m_find_value(_Id, _Context) -> undefined.
m_find_value(Id, #m{value=undefined}, Context) ->
    get_ua(Id, Context).

%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.

%%
%% Api
%%

get_ua(Id, Context) ->
    z_depcache:memo(fun() -> get_raw(Id, Context) end,
                    {get_ua_props, Id}, ?HOUR, [], Context).

get_raw(Id, Context) ->
    SQL = <<"SELECT * FROM user_agent WHERE id = $1">>,
    z_db:assoc_props_row(SQL, [Id], Context).


