-module(grocery_list_entry_resource).

-export([init/1, allowed_methods/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([delete_resource/2]).
-export([from_json/2, to_json/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, []}.

allowed_methods(Req, State) ->
  {['GET', 'PUT', 'DELETE'], Req, State}.

content_types_provided(Req, State) ->
  {[{"application/json", to_json}], Req, State}.

resource_exists(Req, State) ->
  ListName = wrq:path_info(list, Req),
  ItemName = wrq:path_info(item, Req),
  case grocery_manager:get_list(ListName) of
    {error, _} ->
      {false, Req, State};
    List ->
      case dict:find(ItemName, List) of
        error ->
          {false, Req, State};
        {ok, Qty} ->
          {true, Req, Qty}
      end
  end.

delete_resource(Req, State) ->
  List = wrq:path_info(list, Req),
  ItemName = wrq:path_info(item, Req),
  case grocery_manager:remove_from_list(List, ItemName) of
    ok ->
      %% Let's shut up RestClient :(
      Req1 = wrq:set_resp_header("Content-Type", "application/json", Req),
      Req2 = wrq:append_to_response_body(mochijson2:encode({struct, [{<<"count">>, 0}]}), Req1),
      {true, Req2, State};
    {error, _} ->
      {false, Req, State}
  end.

from_json(Req, State) ->
  List = wrq:path_info(list, Req),
  ItemName = wrq:path_info(item, Req),
  Qty = case mochijson2:decode(wrq:req_body(Req)) of
          [] ->
            1;
          V ->
            V
        end,
  case grocery_manager:add_to_list(List, ItemName, Qty) of
    ok ->
      %% Let's shut up RestClient :(
      Req1 = wrq:set_resp_header("Content-Type", "application/json", Req),
      Req2 = wrq:append_to_response_body(mochijson2:encode({struct, [{<<"count">>, Qty}]}), Req1),
      {true, Req2, State};
    {error, _} ->
      {false, Req, State}
  end.

to_json(Req, Qty) ->
  {mochijson2:encode({struct, [{<<"count">>, Qty}]}), Req, Qty}.

content_types_accepted(Req, State) ->
  {[{"application/json", from_json}], Req, State}.
