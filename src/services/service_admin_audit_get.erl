-module(service_admin_audit_get).
-author("Maas-Maarten Zeeman <maas@channel.me>").

-svc_title("Retrieve the details of an audit item").
-svc_needauth(true).

-export([process_get/2]).

-include_lib("zotonic.hrl").

process_get(_ReqData, Context) ->
    case z_context:get_q("id", Context) of
        undefined -> {error, missing_arg, "id"};
        [] -> {error, missing_arg, "id"};
        <<>> -> {error, missing_arg, "id"};
        Id ->
            Data = m_audit:get_visible(z_convert:to_integer(Id), Context),
            jsx:encode(Data)
    end.
