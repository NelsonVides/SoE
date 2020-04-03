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
decode(Msg) ->
    JSON = try jiffy:decode(Msg, [return_maps])
           catch _ -> invalid_message end,
    case JSON of
        #{<<"type">> := <<"message">>} -> parse_message(JSON);
        #{<<"type">> := <<"status">>} -> parse_status(JSON);
        _ -> invalid_binary
    end.

-spec parse_message(map()) -> stanza().
parse_message(JSON) ->
    JSON.

-spec parse_status(map()) -> stanza().
parse_status(JSON) ->
    JSON.
