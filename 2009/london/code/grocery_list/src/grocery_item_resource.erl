-module(grocery_item_resource).

-export([init/1, content_types_provided/2]).
-export([resource_exists/2, content_types_accepted/2]).
-export([to_json/2, from_json/2]).
-export([generate_etag/2]).
-export([allowed_methods/2, delete_resource/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, []}.

allowed_methods(Req, State) ->
  {['GET', 'PUT', 'DELETE'], Req, State}.

%% GETs
content_types_provided(Req, State) ->
  {[{"application/json", to_json}], Req, State}.

%% PUTs etc
content_types_accepted(Req, State) ->
  {[{"application/json", from_json}], Req, State}.

resource_exists(Req, State) ->
  case wrq:path_info(item, Req) of
    undefined ->
      Items = [[grocery_util:ib(ItemName),
                grocery_util:ib(ItemDesc),
                ItemPrice] || {ItemName, {ItemDesc, ItemPrice}} <- grocery_manager:dump_items()],
      {true, Req, Items};
    Item ->
      io:format("Item: ~p~n", [Item]),
      case grocery_manager:get_item(Item) of
        {error, _} ->
          {false, Req, State};
        ItemData ->
          {true, Req, ItemData}
      end
  end.

delete_resource(Req, State) ->
  Item = wrq:path_info(item, Req),
  Result = case grocery_manager:delete_item(Item) of
             ok ->
               true;
             {error, _} ->
               false
           end,
  {Result, Req, State}.

to_json(Req, {ItemName, ItemDesc, Price}=State) ->
  {mochijson2:encode([grocery_util:ib(ItemName),
                      grocery_util:ib(ItemDesc),
                      Price]), Req, State};
to_json(Req, Items) ->
  {mochijson2:encode(Items), Req, Items}.

from_json(Req, State) ->
  Body = wrq:req_body(Req),
  ItemName = wrq:path_info(item, Req),
  [ItemDesc, ItemPrice] = mochijson2:decode(Body),
  case grocery_manager:update_item(ItemName, ItemDesc, ItemPrice) of
    ok ->
      %% Let's shut up RestClient :(
      Req1 = wrq:set_resp_header("Content-Type", "application/json", Req),
      Req2 = wrq:append_to_response_body(mochijson2:encode([grocery_util:ib(ItemName),
                                                            grocery_util:ib(ItemDesc),
                                                            ItemPrice]), Req1),
      {true, Req2, {ItemName, ItemDesc, ItemPrice}};
    _ ->
      {false, Req, State}
  end.

generate_etag(Req, State) ->
  {mochihex:to_hex(crypto:md5(term_to_binary(State))), Req, State}.
