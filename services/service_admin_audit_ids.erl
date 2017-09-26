%%
%%
%%

-module(service_admin_audit_ids).
-author("Maas-Maarten Zeeman <maas@channel.me>").

-svc_title("Retrieve the list of all audit events in the system.").
-svc_needauth(true).

-export([process_get/2]).

-include_lib("zotonic.hrl").

-define(IDS_PAGE_LENGTH, 1000).

process_get(_ReqData, Context) ->
    DateStart = get_date_start(Context),
    DateEnd = get_date_end(Context),
    PageNr = get_page_nr(Context),
    AuditEventCats = get_cat_exacts(Context),
    Ids = get_ids(DateStart, DateEnd, AuditEventCats, PageNr, Context),
    z_convert:to_json({array, Ids}).

get_ids(DateStart, DateEnd, Props, Page, Context) ->
    Offset = (Page - 1) * ?IDS_PAGE_LENGTH + 1,
    R = z_search:search({audit, [{date_start, DateStart}, {date_end, DateEnd}, {assoc, false} | Props]}, {Offset, ?IDS_PAGE_LENGTH}, Context),
    R#search_result.result.

get_date_start(Context) ->
    case z_context:get_q("date_start", Context) of
        "" -> week_ago();
        <<>>  -> week_ago();
        undefined -> week_ago();
        D ->
            filter_to_datetime:to_datetime(D, Context)
    end.

get_date_end(Context) ->
    case z_context:get_q("date_end", Context) of
        "" -> today();
        <<>>  -> today();
        undefined -> today();
        D ->
            filter_end_of_day:end_of_day(
                filter_to_datetime:to_datetime(D, Context), Context)
    end.

get_page_nr(Context) ->
    case z_context:get_q("page", Context) of
        "" -> 1;
        <<>> -> 1;
        undefined -> 1;
        N ->
            try
                erlang:max(1, z_convert:to_integer(N))
            catch
                _:_ ->
                    1
            end
    end.

get_cat_exacts(Context) ->
    Cats = z_context:get_q_all("cat_exact", Context),
    L = [get_cat_id(N, Context) || N <- Cats],
    [{cat_exact, I} || I <- L, I =/= undefined].


get_cat_id(Name, Context) ->
    case m_category:name_to_id(Name, Context) of
        {ok, Id} -> Id;
        {error, _} -> undefined
    end.

week_ago() ->
    lists:foldl(fun(_X, D) -> z_datetime:prev_day(D) end, today(), lists:seq(1, 7)).

today() ->
    calendar:universal_time().