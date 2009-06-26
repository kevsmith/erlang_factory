-module(hello_world_resource).
-export([init/1, to_html/2, generate_etag/2]).
-export([encodings_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, "Hello, world"}.

to_html(Req, State) ->
  {io_lib:format("<html><head><title>~s</title></head><body>~s</body></html>", [State, State]), Req, State}.

generate_etag(Req, State) ->
  {mochihex:to_hex(crypto:md5(State)), Req, State}.

encodings_provided(Req, State) ->
  {[{"gzip", fun(X) -> zlib:gzip(X) end}], Req, State}.
