-module(parser_helper).

%% Helper functions
-export([
         login_stanza/1,
         login_successful_answer/1,
         login_failed_answer/0
        ]).

login_stanza(Nickname) ->
    #{<<"type">> => <<"status">>,
      <<"status">> => <<"login">>,
      <<"nickname">> => Nickname
     }.

login_successful_answer(Nickname) ->
    #{<<"type">> => <<"status">>,
      <<"status">> => <<"successful login">>,
      <<"nickname">> => Nickname
     }.

login_failed_answer() ->
    #{<<"type">> => <<"status">>,
      <<"status">> => <<"failed login">>,
      <<"reason">> => <<"nickname in use">>
     }.
