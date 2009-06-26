-module(grocery_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, new_list/1, get_list/1, delete_list/1]).
-export([add_to_list/3, remove_from_list/2, list_names/0]).
-export([delete_item/1, get_item/1, update_item/3, dump_items/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {lists=dict:new(),
                items=dict:new()}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_list(ListName) ->
  gen_server:call(?SERVER, {new_list, ListName}).

get_list(ListName) ->
  gen_server:call(?SERVER, {fetch_list, ListName}).

delete_list(ListName) ->
  gen_server:call(?SERVER, {delete_list, ListName}).

list_names() ->
  gen_server:call(?SERVER, list_names).

add_to_list(ListName, ItemName, Qty) ->
  gen_server:call(?SERVER, {add_to_list, ListName, ItemName, Qty}).

remove_from_list(ListName, ItemName) ->
  gen_server:call(?SERVER, {remove_from_list, ListName, ItemName}).

update_item(ItemName, ItemDescription, ItemPrice) ->
  gen_server:call(?SERVER, {update_item, ItemName, ItemDescription, ItemPrice}).

delete_item(ItemName) ->
  gen_server:call(?SERVER, {delete_item, ItemName}).

get_item(ItemName) ->
  gen_server:call(?SERVER, {get_item, ItemName}).

dump_items() ->
  gen_server:call(?SERVER, dump_items).

init([]) ->
  {ok, #state{}}.

handle_call({new_list, ListName}, _From, #state{lists=Lists}=State) ->
  case dict:is_key(ListName, Lists) of
    false ->
      {reply, ok, State#state{lists=dict:store(ListName, dict:new(), Lists)}};
    true ->
      {reply, {error, already_exists}, State}
  end;

handle_call({fetch_list, ListName}, _From, #state{lists=Lists}=State) ->
  case dict:find(ListName, Lists) of
    error ->
      {reply, {error, unknown_list}, State};
    {ok, List} ->
      {reply, List, State}
  end;

handle_call({delete_list, ListName}, _From, #state{lists=Lists}=State) ->
  {reply, ok, State#state{lists=dict:erase(ListName, Lists)}};

handle_call(list_names, _From, #state{lists=Lists}=State) ->
  {reply, dict:fetch_keys(Lists), State};

handle_call({add_to_list, ListName, ItemName, Qty}, _From, #state{lists=Lists, items=Items}=State) ->
  case dict:is_key(ItemName, Items) of
    true ->
      case dict:find(ListName, Lists) of
        {ok, List} ->
          case dict:is_key(ItemName, List) of
            false ->
              {reply, ok, State#state{lists=dict:store(ListName, dict:store(ItemName, Qty, List), Lists)}};
            true ->
              {reply, {error, duplicate_entry}, State}
          end;
        error ->
          {reply, {error, unknown_list}, State}
      end;
    false ->
      {reply, {error, unknown_item}, State}
  end;

handle_call({remove_from_list, ListName, ItemName}, _From, #state{lists=Lists, items=Items}=State) ->
  case dict:is_key(ItemName, Items) of
    true ->
      case dict:find(ListName, Lists) of
        {ok, List} ->
          case dict:is_key(ItemName, List) of
            true ->
              {reply, ok, State#state{lists=dict:store(ListName, dict:erase(ItemName, List), Lists)}};
            false ->
              {reply, {error, missing_entry}, State}
          end;
        error ->
          {reply, {error, unknown_list}, State}
      end;
    false ->
      {reply, {error, unknown_item}, State}
  end;

handle_call({update_item, ItemName, ItemDescription, ItemPrice}, _From, #state{items=Items}=State) ->
  {reply, ok, State#state{items=dict:store(ItemName, {ItemDescription, ItemPrice}, Items)}};

handle_call({delete_item, ItemName}, _From, #state{items=Items}=State) ->
  case dict:is_key(ItemName, Items) of
    false ->
      {reply, {error, unknown_item}, State};
    true ->
      {reply, ok, State#state{items=dict:erase(ItemName, Items)}}
  end;

handle_call({get_item, ItemName}, _From, #state{items=Items}=State) ->
  case dict:find(ItemName, Items) of
    error ->
      {reply, {error, unknown_item}, State};
    {ok, {ItemDescription, ItemPrice}} ->
      {reply, {ItemName, ItemDescription, ItemPrice}, State}
  end;

handle_call(dump_items, _From, #state{items=Items}=State) ->
  {reply, dict:to_list(Items), State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
