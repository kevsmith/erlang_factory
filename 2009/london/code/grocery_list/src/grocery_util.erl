-module(grocery_util).

-export([ib/1]).

ib(Text) when is_list(Text) ->
  list_to_binary(Text);
ib(Text) when is_binary(Text) ->
  Text.
