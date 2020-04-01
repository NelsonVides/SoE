-module(parser).

%% API
-export([
         encode/1,
         decode/1
        ]).

-type stanza() :: #{}.

-spec encode(stanza()) -> binary() | invalid_stanza.
encode(_Msg) ->
    <<"">>.

-spec decode(binary()) -> stanza() | invalid_message.
decode(_Msg) ->
    #{}.

