

-module(mod_admin_audit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Admin audit functionality").
-mod_description("Support audit log of important actions.").
-mod_prio(1).
-mod_depends([admin, menu]).


-export([
    observe_logon_submit/2,

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
    ?DEBUG(Event),
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
    ?DEBUG({Event, Context}),
    undefined.

observe_module_deactivate(Event, Context) ->
    ?DEBUG({Event, Context}),
    undefined.

%% 
%%
who(Context) ->
    z_acl:user(Context).

%%
%% console or ip address 
from(Context) ->
    todo.
    
    
    
        


