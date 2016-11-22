

-module(mod_admin_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Admin audit functionality").
-mod_description("Support audit log of important actions.").
-mod_prio(1).
-mod_schema(1).
-mod_depends([admin, menu]).
-mod_provides([audit]).


-export([manage_schema/2]).

-export([
    observe_logon_submit/2,
    observe_logon_ready_page/2,

    observe_rsc_insert/3,
    observe_rsc_update_done/2,
    observe_rsc_delete/2,

    observe_module_activate/2,
    observe_module_deactivate/2
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

%%
%% Users
%%

observe_logon_submit(Event, Context) ->
    ?DEBUG({Event, info(Context)}),
    undefined.

observe_logon_ready_page(Event, Context) ->
    ?DEBUG({Event, info(Context)}),
    undefined.

%%
%% Resource mutation
%%

observe_rsc_insert(Event, Args, Context) -> audit({Event, Args}, Context), Args.
observe_rsc_update_done(Event, Context) -> audit(Event, Context), undefined.
observe_rsc_delete(Event, Context) -> audit(Event, Context), undefined.

%%
%% Module activation and de-activation
%%

observe_module_activate(Event, Context) -> audit(Event, Context), undefined.
observe_module_deactivate(Event, Context) -> audit(Event, Context), undefined.

%%
%%
%%
audit(Event, Context) ->
    ?DEBUG({Event, info(Context)}).

%%
%% Helpers
%%

info(Context) ->
    Info = [{user_id, z_acl:user(Context)}],

    case z_context:get_reqdata(Context) of
        undefined -> Info;
        ReqData ->
            ?DEBUG(m_audit:user_agent_id(client(Context), Context)),
            [{ip_address, wrq:peer(ReqData)}, 
            {user_agent, client(Context)}|Info]
    end.

%%
%% Get the user-agent header
%%

client(Context) ->
    case z_context:get_reqdata(Context) of
        undefined -> undefined;
        _ReqData ->  z_context:get_req_header("user-agent", Context)
    end.


%%
%% Database
%%

manage_schema(Version, Context) ->
    m_audit:manage_schema(Version, Context).
