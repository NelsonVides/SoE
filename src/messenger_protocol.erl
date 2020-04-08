-module(messenger_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Socket, Transport, login).

loop(Socket, Transport, login) ->
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} when Data =/= <<4>> ->
            Msg = parser:decode(Data),
            case do_login(Msg) of
                {success, Nickname} ->
                    Transport:send(Socket, parser:encode(success(Nickname))),
                    loop(Socket, Transport, chat);
                {fail, Error} ->
                    Transport:send(Socket, parser:encode(Error)),
                    ok = Transport:close(Socket)
            end;
        _ ->
            ok = Transport:close(Socket)
    end;
loop(Socket, Transport, chat) ->
    ok = Transport:close(Socket).

do_login(_) -> {fail, #{}}.
success(_) -> #{}.
