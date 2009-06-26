-module(grocery_list_resource).

-export([init/1, allowed_methods/2, generate_etag/2]).
-export([resource_exists/2, content_types_provided/2, content_types_accepted/2]).
-export([to_json/2, from_json/2, delete_resource/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, []}.

allowed_methods(Req, State) ->
  {['GET', 'PUT', 'DELETE'], Req, State}.

resource_exists(Req, State) ->
  case wrq:path_info(list, Req) of
    undefined ->
      {true, Req, [grocery_util:ib(Name) || Name <- grocery_manager:list_names()]};
    ListName ->
      case grocery_manager:get_list(ListName) of
        {error, _} ->
          {false, Req, State};
        List ->
          {true, Req, List}
      end
  end.

content_types_provided(Req, State) ->
  {[{"application/json", to_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{"application/json", from_json}], Req, State}.

delete_resource(Req, State) ->
  ListName = wrq:path_info(list, Req),
  case grocery_manager:delete_list(ListName) of
    ok ->
      {true, Req, State};
    {error, _} ->
      {false, Req, State}
  end.

to_json(Req, State) ->
  case wrq:path_info(list, Req) of
    undefined ->
      {mochijson2:encode(State), Req, State};
    _ ->
      {mochijson2:encode({struct, format_list(dict:to_list(State))}), Req, State}
  end.

from_json(Req, State) ->
  case wrq:path_info(list, Req) of
    undefined ->
      {false, Req, State};
    ListName ->
      case grocery_manager:new_list(ListName) of
        ok ->
          %% Let's shut up RestClient :(
          Req1 = wrq:set_resp_header("Content-Type", "application/json", Req),
          Req2 = wrq:append_to_response_body(mochijson2:encode([]), Req1),
          {true, Req2, State};
        {error, _} ->
          {false, Req, State}
      end
  end.

generate_etag(Req, State) ->
  {mochihex:to_hex(crypto:md5(term_to_binary(State))), Req, State}.

%% Internal functions
format_list(List) ->
  [{grocery_util:ib(ItemName), Qty} || {ItemName, Qty} <- List].
