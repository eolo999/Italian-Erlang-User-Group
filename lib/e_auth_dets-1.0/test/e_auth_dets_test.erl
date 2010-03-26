-module(e_auth_dets_test).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).
-export([prop_command/0]).

-record(user, {username::string(), password::string(), groups = []::[string()]}).
-record(state, {users = [] ::[#user{}], groups = [] :: [string()]}).

prop_command() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                application:start(crypto),
                file:delete("users"),
                file:delete("groups"),
                e_auth_dets:dependencies(),
                e_auth_dets:install([]),
                {History,State,Result} = run_commands(?MODULE, Cmds),
                e_auth_dets:uninstall([]),
                aggregate(command_names(Cmds),
                          ?WHENFAIL(io:format("Not ok: ~p~nState:~n~p~nHistory:~n~p~n", [Result, State, History]),
                                    Result == ok))
            end).

initial_state() ->
    #state{}.

string() ->
    [noshrink(choose($A, $I))].

command(#state{users = Users, groups = Groups}) ->
    fault_rate(1, 10,
               oneof(
                 [{call, e_auth_dets, add_user, [string(), string()]},
                  {call, e_auth_dets, add_group, [string()]},
                  {call, e_auth_dets, get_groups, []},
                  {call, e_auth_dets, get_users, []}]
                 ++
                 case Users of
                     [] -> [];
                     [_|_] ->
                         [{call, e_auth_dets, delete_user,
                           ?LET(#user{username = Username},
                                elements(Users),
                                [fault(string(), Username)])},
                          {call, e_auth_dets, rename_user,
                           ?LET(#user{username = Username},
                                elements(Users),
                                [fault(string(), Username),
                                 string()])},
                          {call, e_auth_dets, authenticate,
                           ?LET(#user{username = Username, password = Password},
                                elements(Users),
                                [fault(string(), Username),
                                 fault(string(), Password)])},
                          {call, e_auth_dets, change_password,
                           ?LET(#user{username = Username},
                                elements(Users),
                                [fault(string(), Username),
                                 string()])},
                          {call, e_auth_dets, get_groups,
                          ?LET(#user{username = Username},
                                elements(Users),
                                [fault(string(), Username)])}]
                             ++
                             case Groups of
                                 [] -> [];
                                 [_|_] ->
                                     [{call, e_auth_dets, add_to_group,
                                       ?LET({#user{username = Username},
                                             Group},
                                            {elements(Users), elements(Groups)},
                                            [fault(string(), Username),
                                             fault(string(), Group)])},
                                      {call, e_auth_dets, delete_group,
                                       [fault(string(), elements(Groups))]},
                                      {call, e_auth_dets, remove_from_group,
                                       ?LET(#user{username = Username,
                                                  groups = UserGroups},
                                            elements(Users),
                                            [fault(string(), Username),
                                             case UserGroups of
                                                 [] -> elements(Groups);
                                                 [_|_] ->
                                                     fault(elements(Groups),
                                                           elements(UserGroups))
                                             end])},
                                      {call, e_auth_dets, rename_group,
                                       ?LET(Group, elements(Groups),
                                            [fault(string(), Group),
                                             string()])}]
                             end
                 end
                )).

precondition(_State, _Call) ->
    true.

postcondition(State, {call, _, add_user, [Username, _Password]}, Result) ->
    Expected =
        case lists:keymember(Username, #user.username, State#state.users) of
            false -> ok;
            true -> {error, user_exists}
        end,
    Expected == Result;
postcondition(State, {call, _, add_to_group, [Username, Group]}, Result) ->
    case {lists:keymember(Username, #user.username, State#state.users),
          lists:member(Group, State#state.groups),
          Result} of
        {true, true, ok} ->
            true;
        {_, _, {error, _}} ->
            true;
        _ ->
            false
    end;
postcondition(State, {call, _, authenticate, [Username, Password]}, Result) ->
    Expected =
        case lists:keysearch(Username, #user.username, State#state.users) of
            {value, #user{password = Password}} -> ok;
            {value, #user{}} -> {error, authentication};
            false -> {error, no_such_user}
        end,
    Expected == Result;
postcondition(State, {call, _, change_password, [Username, _Password]}, Result) ->
    Expected =
        case lists:keymember(Username, #user.username, State#state.users) of
            true -> ok;
            false -> {error, no_such_user}
        end,
    Expected == Result;
postcondition(State, {call, _, get_users, []}, Result) ->
    OurResult = [User#user.username || User <- State#state.users],
    lists:sort(OurResult) == lists:sort(Result);
postcondition(State, {call, _, get_groups, []}, Result) ->
    lists:sort(State#state.groups) == lists:sort(Result);
postcondition(State, {call, _, get_groups, [Username]}, Result) ->
    case lists:keysearch(Username, #user.username, State#state.users) of
        false ->
            %% hm...
            true;
        {value, #user{groups = Groups}} ->
            lists:sort(Result) == lists:sort(Groups)
    end;
postcondition(State, {call, _, rename_user, [OldUserName, NewUserName]}, Result) ->
    case {lists:keymember(OldUserName, #user.username, State#state.users),
          lists:keymember(NewUserName, #user.username, State#state.users),
          Result} of
        {true, false, ok} ->
            true;
        {_, _, {error, _}} ->
            true;
        _ ->
            false
    end;
postcondition(State, {call, _, rename_group, [Old, New]}, Result) ->
    case {lists:member(Old, State#state.groups),
          lists:member(New, State#state.groups),
          Result} of
        {true, false, ok} ->
            true;
        {_, _, {error, _}} ->
            true;
        _ ->
            false
    end;
postcondition(_State, _Call, {error, _}) ->
    false;
postcondition(_State, _Call, ok) ->
    true.

next_state(State, _Result, {call, _, add_user, [Username, Password]}) ->
    case lists:keymember(Username, #user.username, State#state.users) of
        false ->
            State#state{users = [#user{username = Username, password = Password}|State#state.users]};
        true ->
            State
    end;
next_state(State, _Result, {call, _, delete_user, [Username]}) ->
    State#state{users =
                lists:keydelete(Username, #user.username,
                                State#state.users)};
next_state(State, _Result, {call, _, add_group, [Group]}) ->
    State#state{groups = lists:usort([Group|State#state.groups])};
next_state(State, _Result, {call, _, change_password, [Username, Password]}) ->
    case lists:keysearch(Username, #user.username, State#state.users) of
        {value, User} ->
            State#state{users = [User#user{password = Password}|
                                 lists:delete(User, State#state.users)]};
        false ->
            State
    end;
next_state(State, _Result, {call, _, add_to_group, [Username, Group]}) ->
    case lists:member(Group, State#state.groups) of
        true ->
            update_user(State, Username,
                        fun(#user{groups = Groups} = User) ->
                                User#user{groups = lists:usort([Group|Groups])}
                        end);
        false ->
            State
    end;
next_state(State, _Result, {call, _, remove_from_group, [Username, Group]}) ->
    update_user(State, Username,
                fun(#user{groups = Groups} = User) ->
                        User#user{groups = Groups -- [Group]}
                end);
next_state(State, _Result, {call, _, rename_group, [Old, New]}) ->
    case {lists:member(Old, State#state.groups),
          lists:member(New, State#state.groups)} of
        {true, false} ->
            NewState =
                State#state{groups =
                            [New|State#state.groups -- [Old]]},
            update_all_users(
              NewState,
              fun(#user{groups = Groups} = User) ->
                      case lists:member(Old, Groups) of
                          false -> User;
                          true ->
                              User#user{groups =
                                        lists:usort([New|Groups--[Old]])}
                      end
              end);
        _ ->
            State
    end;
next_state(State, _Result, {call, _, delete_group, [Group]}) ->
    NewState = State#state{groups = State#state.groups -- [Group]},
    update_all_users(
      NewState,
      fun(#user{groups = Groups} = User) ->
              User#user{groups = Groups -- [Group]}
      end);
next_state(State, _Result, {call, _, rename_user, [OldUserName, NewUserName]}) ->
    case {lists:keymember(OldUserName, #user.username, State#state.users),
          lists:keymember(NewUserName, #user.username, State#state.users)} of
        {true, false} ->
            update_user(State, OldUserName,
                        fun(User) ->
                                User#user{username = NewUserName}
                        end);
        _ ->
            State
    end;
next_state(State, _Result, _Call) ->
    State.

update_user(State, Username, Fun) ->
    State#state{users =
                lists:map(
                  fun(#user{username = U} = User)
                     when U == Username ->
                          Fun(User);
                     (User) -> User
                  end, State#state.users)}.

update_all_users(State, Fun) ->
    State#state{users = lists:map(Fun, State#state.users)}.
