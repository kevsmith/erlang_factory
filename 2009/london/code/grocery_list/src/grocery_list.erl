%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(grocery_list).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the grocery_list server.
start() ->
    grocery_list_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(grocery_list).

%% @spec stop() -> ok
%% @doc Stop the grocery_list server.
stop() ->
    Res = application:stop(grocery_list),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
