%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the grocery_list application.

-module(grocery_list_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for grocery_list.
start(_Type, _StartArgs) ->
    grocery_list_deps:ensure(),
    grocery_list_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for grocery_list.
stop(_State) ->
    ok.
