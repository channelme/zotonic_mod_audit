

-module(mod_admin_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Admin audit functionality").
-mod_description("Support audit log of important actions.").
-mod_prio(1).
-mod_depends([admin, menu]).


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
    ?DEBUG({Event, who(Context), with_what(Context), from(Context)}),
    undefined.

observe_logon_ready_page(Event, Context) ->
    ?DEBUG({Event, who(Context), with_what(Context), from(Context)}),
    undefined.

%%
%% Resources
%%

observe_rsc_insert(Event, Args, Context) ->
    ?DEBUG({Event, Args}),
    Args.

observe_rsc_update_done(Event, Context) ->
    ?DEBUG({Event, who(Context)}),
    undefined.

observe_rsc_delete(Event, Context) ->
    ?DEBUG(Event),
    undefined.

%%
%% Modules
%%

observe_module_activate(Event, Context) ->
    ?DEBUG({Event, who(Context), with_what(Context), from(Context)}),
    undefined.

observe_module_deactivate(Event, Context) ->
    ?DEBUG({Event, who(Context), with_what(Context), from(Context)}),
    undefined.

%%
%% Helpers
%%

%% Who is logged on.
%%
who(Context) ->
    z_acl:user(Context).

%% console or ip address 
%%
from(Context) ->
    case z_context:get_reqdata(Context) of
        undefined -> console;
        ReqData -> wrq:peer(ReqData)
    end.

%%
%% Get the user-agent header
%%
with_what(Context) ->
    z_context:get_req_header("user-agent", Context).
