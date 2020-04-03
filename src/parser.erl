-module(parser).

%% API
-export([
         encode/1,
         decode/1
        ]).

-type stanza() :: #{}.

-spec encode(stanza()) -> binary() | invalid_stanza.
encode(Msg) ->
    try jiffy:encode(Msg)
    catch _ -> invalid_stanza end.

-spec decode(binary()) -> stanza() | invalid_message.
decode(_Msg) ->
    #{}.

