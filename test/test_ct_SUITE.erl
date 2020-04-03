%%%-------------------------------------------------------------------
%%% @author nelsonvides
%%% @copyright (C) 2020, nelsonvides
%%% @doc
%%%
%%% @end
%%% Created : 2020-02-24 11:58:57.704068
%%%-------------------------------------------------------------------
-module(test_ct_SUITE).


%% API
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         echo_protocol_does_echo/1,
         echo_protocol_respects_EOT/1,
         login_successful/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     {group, echo_protocol},
     {group, messenger_protocol}
    ].

groups() ->
    [
     {echo_protocol, [], [echo_protocol_does_echo, echo_protocol_respects_EOT]},
     {messenger_protocol, [], [login_successful]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    ok = application:ensure_started(ranch),
    Config.

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
init_per_group(echo_protocol, Config) ->
    {ok, _Listener} = ranch:start_listener(echo_listener,
                                           ranch_tcp, #{socket_opts => [{port, 5555}]},
                                           basic_protocol, []),
    Config;
init_per_group(messenger_protocol, Config) ->
    {ok, _Listener} = ranch:start_listener(echo_listener,
                                           ranch_tcp, #{socket_opts => [{port, 5556}]},
                                           messenger_protocol, []),
    Config.

end_per_group(echo_protocol, _Config) ->
    ranch:stop_listener(echo_listener),
    ok;
end_per_group(messenger_protocol, _Config) ->
    ranch:stop_listener(messenger_protocol),
    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(TC, Config) when TC == echo_protocol_does_echo;
                                   TC == echo_protocol_respects_EOT ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}]),
    [{socket, Sock} | Config];
init_per_testcase(login_successful, Config) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5556, [binary, {packet, 0}]),
    [{socket, Sock} | Config].

end_per_testcase(TC, Config) when TC == echo_protocol_does_echo;
                                  TC == echo_protocol_respects_EOT;
                                  TC == login_successful ->
    ok = gen_tcp:close(?config(socket, Config)),
    Config;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

echo_protocol_does_echo(Config) ->
    Sock = ?config(socket, Config),
    F = fun(Msg) ->
                ok = gen_tcp:send(Sock, Msg),
                receive
                    {tcp, Sock, X} ->
                        X
                after 1000 ->
                          error
                end
        end,
    ?assert(proper:counterexample(
             ?FORALL(Msg,
                     ?SUCHTHAT(Msg, binary(), Msg =/= <<>> andalso Msg =/= <<4>>),
                     Msg == F(Msg))),
            [{numtests, 100}]).

echo_protocol_respects_EOT(Config) ->
    Sock = ?config(socket, Config),
    ok = gen_tcp:send(Sock, <<4>>),
    receive
        {tcp_closed, Sock} ->
            ok
    after 1000 ->
              error
    end.

login_successful(Config) ->
    Sock = ?config(socket, Config),
    Nickname = <<"Alice">>,
    Expected = parser_helper:login_successful_answer(Nickname),
    ok = do_login(Sock, Nickname, Expected).

do_login(Socket, Nickname, Expected) ->
    LoginMsg = parser_helper:login_stanza(Nickname),
    ok = gen_tcp:send(Socket, parser:encode(LoginMsg)),
    receive
        {tcp, Socket, Answer} ->
            Decoded = parser:decode(Answer),
            ?assertEqual(Expected, Decoded)
    after 1000 ->
              error
    end.
