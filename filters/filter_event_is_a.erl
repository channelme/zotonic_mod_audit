%%
%%
%%

-module(filter_event_is_a).

-export([event_is_a/3]).

event_is_a([], _Cat, _Context) -> [];
event_is_a(Arg, Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            z_list_of_ids_filter:filter(Arg, fun(Id) ->
                    m_audit:is_a(Id, CatId, Context)
                end, Context);
        {error, _Reason} when is_integer(Arg) -> false;
        {error, _Reason} when is_list(Arg) -> []
    end.