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
         login_successful/1,
         login_fails_conflicting_nickname/1,
         user_can_login_and_log_out_and_log_in_again/1,
         user_fails_to_chat_to_non_existent_user/1,
         two_users_can_chat/1
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
     {messenger_protocol, [], [{group, messenger_protocol_single_user},
                               {group, messenger_protocol_two_users}]},
     {messenger_protocol_single_user, [], [login_successful,
                                           user_can_login_and_log_out_and_log_in_again,
                                           user_fails_to_chat_to_non_existent_user
                                          ]},
     {messenger_protocol_two_users, [], [login_fails_conflicting_nickname,
                                         two_users_can_chat
                                        ]}
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
    Config;
init_per_group(messenger_protocol_single_user, Config) ->
    [{number_of_clients, 1} | Config];
init_per_group(messenger_protocol_two_users, Config) ->
    [{number_of_clients, 2} | Config].

end_per_group(echo_protocol, _Config) ->
    ranch:stop_listener(echo_listener),
    ok;
end_per_group(messenger_protocol, _Config) ->
    ranch:stop_listener(messenger_protocol),
    ok;
end_per_group(_GN, _Config) ->
    ok.

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(TC, Config) when TC == echo_protocol_does_echo;
                                   TC == echo_protocol_respects_EOT ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}]),
    [{socket, Sock} | Config];
init_per_testcase(TC, Config) ->
    NumberOfClients = ?config(number_of_clients, Config),
    Users = generate_users(TC, NumberOfClients),
    Users ++ Config.

end_per_testcase(TC, Config) when TC == echo_protocol_does_echo;
                                  TC == echo_protocol_respects_EOT ->
    ok = gen_tcp:close(?config(socket, Config));
end_per_testcase(_TC, Config) ->
    NumberOfClients = ?config(number_of_clients, Config),
    GeneratedUsers = get_generated_users(Config, NumberOfClients),
    lists:foreach(fun({Socket, _}) -> ok = gen_tcp:close(Socket) end, GeneratedUsers).

generate_users(TC, N) when is_integer(N), N > 0 ->
    [
     begin
         {ok, Socket} = gen_tcp:connect("localhost", 5556, [binary, {packet, 0}]),
         Nickname = <<"User_", (integer_to_binary(X))/binary, (atom_to_binary(TC, utf8))/binary>>,
         {binary_to_atom(<<"user", (integer_to_binary(X))/binary>>, utf8), {Socket, Nickname}}
     end || X <- lists:seq(1, N)].

get_generated_users(Config, N) when is_integer(N), N > 0 ->
    [begin
         UserAtom = binary_to_atom(<<"user", (integer_to_binary(X))/binary>>, utf8),
         ?config(UserAtom, Config)
     end || X <- lists:seq(1, N)].

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

%% Echo protocol tests
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


%% Messenger protocol tests
login_successful(Config) ->
    {Socket, Nickname} = ?config(user1, Config),
    Expected = parser_helper:login_successful_answer(Nickname),
    ok = do_login(Socket, Nickname, Expected).

login_fails_conflicting_nickname(Config) ->
    {Socket1, Nickname1} = ?config(user1, Config),
    {Socket2, _} = ?config(user2, Config),
    ExpectedSuccess = parser_helper:login_successful_answer(Nickname1),
    ok = do_login(Socket1, Nickname1, ExpectedSuccess),
    ExpectedFailure = parser_helper:login_failed_answer(Nickname1),
    ok = do_login(Socket2, Nickname1, ExpectedFailure).

user_can_login_and_log_out_and_log_in_again(Config) ->
    {Sock1, Nickname} = ?config(user1, Config),
    ok = do_login(Sock1, Nickname, parser_helper:login_successful_answer(Nickname)),
    ok = close_session(Sock1),
    {ok, Sock2} = gen_tcp:connect("localhost", 5556, [binary, {packet, 0}]),
    ok = do_login(Sock2, Nickname, parser_helper:login_successful_answer(Nickname)),
    ok = close_session(Sock2).

user_fails_to_chat_to_non_existent_user(Config) ->
    {Socket, Nickname} = ?config(user1, Config),
    ok = do_login(Socket, Nickname, parser_helper:login_successful_answer(Nickname)),
    BadNickname = <<"Bob_not_existing_yet">>,
    ok = do_send_message(
           Socket, Nickname, BadNickname, <<"Hello">>,
           parser_helper:user_non_existent(BadNickname)).

two_users_can_chat(Config) ->
    {Socket1, Nickname1} = ?config(user1, Config),
    {Socket2, Nickname2} = ?config(user2, Config),
    ok = do_login(Socket1, Nickname1, parser_helper:login_successful_answer(Nickname1)),
    ok = do_login(Socket2, Nickname2, parser_helper:login_successful_answer(Nickname2)),
    Msg = <<"Hello World!">>,
    ok = do_send_message(Socket1, Nickname1, Nickname2, Msg, parser_helper:message_delivered()),
    ok = do_receive_message(Socket2, Nickname1, Nickname2, Msg).


do_login(Socket, Nickname, Expected) ->
    LoginMsg = parser_helper:login_stanza(Nickname),
    do_send_stanza(Socket, LoginMsg, Expected).

do_send_message(Socket, From, To, Body, Expected) ->
    Msg = parser_helper:chat_message(From, To, Body),
    do_send_stanza(Socket, Msg, Expected).

close_session(Socket) ->
    case gen_tcp:send(Socket, <<4>>) of
        ok ->
            receive
                {tcp_closed, Socket} ->
                    gen_tcp:close(Socket),
                    ok
            after 1000 -> {error, timeout} end;
        _ -> ok
    end.

do_send_stanza(Socket, ToSend, ExpectedAnswer) ->
    ok = gen_tcp:send(Socket, parser:encode(ToSend)),
    receive
        {tcp, Socket, Answer} ->
            Decoded = parser:decode(Answer),
            ?assertEqual(ExpectedAnswer, Decoded)
    after 1000 ->
              {error, timeout}
    end.

do_receive_message(Socket, From, To, Body) ->
    ExpectedAnswer = parser_helper:chat_message(From, To, Body),
    receive
        {tcp, Socket, Answer} ->
            Decoded = parser:decode(Answer),
            ?assertEqual(ExpectedAnswer, Decoded)
    after 1000 ->
              {error, timeout}
    end.
