%% The contents of this file are subject to the Erlang Web Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Web Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Erlang Training & Consulting
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2008,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%% File    : e_auth.erl
%%% @author Roberto Aloi <roberto.aloi@erlang.consulting.com>
%%% @doc A common interface to the all supported authentication engines.
%%% Its behaviour depends on the authentication engine framework uses.
%%% The engine could be set inside the <i>project.conf</i> file. FIXME!!!
%%% @end
%%% @see e_conf:get_conf/1
%%% @see e_auth_sample
-module(e_auth).
-behaviour(e_component).

-export([install/1, uninstall/1, dependencies/0]).

-export([add_user/2, delete_user/1, change_password/2, rename_user/2]).
-export([get_users/0]).
-export([add_group/1, rename_group/2, delete_group/1,
         get_groups/0, get_groups/1, add_to_group/2, remove_from_group/2]).
-export([login/2, logout/0, status/0, username/0, authorize/1]).

-spec install(_) -> 'ok'.
install(_Conf) ->
    ok.

-spec uninstall(_) -> 'ok'.
uninstall(_Conf) ->
    ok.

-spec dependencies() -> [].
dependencies() ->
    [].

%% Do a function call to the user backend module.
%% Use it like this: ?USERMOD(foo("bar", "baz"))
-define(USERMOD(X),
        begin
            UserBackendModule = e_conf:get_conf(?MODULE),
            UserBackendModule:X
        end).
%% Likewise for the group backend module.  For now they are the
%% same...
-define(GROUPMOD(X),
        begin
            GroupBackendModule = e_conf:get_conf(?MODULE),
            GroupBackendModule:X
        end).

%%
%% @spec add_user(UserName :: string(), Password :: string()) ->
%%   ok | {error, Reason :: atom()}
%% @doc Add a User
%% @end
%%
-spec add_user(UserName::string(), Password::string()) ->
    'ok' | {'error', Reason::atom()}.
add_user(UserName, Password) ->
    ?USERMOD(add_user(UserName, Password)).

%%
%% @spec delete_user(UserName :: string()) -> ok | {error, Reason::atom()}
%% @doc Delete a User
%% @end
%%
-spec delete_user(UserName::string()) ->
    'ok' | {'error', Reason::atom()}.
delete_user(UserName) ->
    ?USERMOD(delete_user(UserName)).

%% @spec rename_user(OldUserName::string(), NewUserName::string()) ->
%%   ok | {error, Reason :: atom()}
%% @doc Rename a user.
%% @end
-spec rename_user(OldUserName::string(), NewUserName::string()) ->
    'ok' | {'error', Reason::atom()}.
rename_user(OldUserName, NewUserName) ->
    ?USERMOD(rename_user(OldUserName, NewUserName)).

%%
%% @spec change_password(UserName::string(), NewPassword::string()) ->
%%   ok | {error, Reason::atom()}
%% @doc Change password for a user.
-spec change_password(UserName::string(), NewPassword::string()) ->
    'ok' | {'error', Reason::atom()}.
change_password(UserName, NewPassword) ->
    ?USERMOD(change_password(UserName, NewPassword)).

%%
%% @spec get_users() -> [string()]
%% @doc Retrieve the list of the users
%% @end
%%
-spec get_users() -> [string()].
get_users() ->
    ?USERMOD(get_users()).

%%
%% @spec add_group(GroupName::string()) -> ok | {error, Reason::atom()}
%% @doc Create a new group.
-spec add_group(GroupName::string()) ->
    'ok' | {'error', Reason::atom()}.
add_group(GroupName) ->
    ?GROUPMOD(add_group(GroupName)).

%%
%% @spec rename_group(OldName::string(), NewName::string()) -> ok | {error, Reason::atom()}
%% @doc Rename a group.
-spec rename_group(OldName::string(), NewName::string()) ->
    'ok' | {'error', Reason::atom()}.
rename_group(OldName, NewName) ->
    ?GROUPMOD(rename_group(OldName, NewName)).

%%
%% @spec delete_group(GroupName::string()) -> ok | {error, Reason::atom()}
%% @doc Delete a group.
-spec delete_group(GroupName::string()) ->
    'ok' | {'error', Reason::atom()}.
delete_group(GroupName) ->
    ?GROUPMOD(delete_group(GroupName)).

%% @spec get_groups() -> [Groups::string()]
%% @doc Get a list of all existing groups.
-spec get_groups() -> [Group::string()].
get_groups() ->
    ?GROUPMOD(get_groups()).

%%
%% @spec get_groups(UserName::string()) -> [string()]
%% @doc Retrieve the list of existing groups for a specific user
%% @end
%%
-spec get_groups(UserName::string()) ->
    [Group::string()] | {'error', Reason::atom()}.
get_groups(UserName) ->
    ?GROUPMOD(get_groups(UserName)).

%%
%% @spec add_to_group(UserName::string(), GroupName::string()) ->
%%   ok | {error, Reason::atom()}
%% @doc Add a user to a group.
-spec add_to_group(UserName::string(), GroupName::string()) ->
    'ok' | {'error', Reason::atom()}.
add_to_group(UserName, GroupName) ->
    ?GROUPMOD(add_to_group(UserName, GroupName)).

%%
%% @spec remove_from_group(UserName::string(), GroupName::string()) ->
%%   ok | {error, Reason::atom()}
%% @doc Remove a user from a group.
-spec remove_from_group(UserName::string(), GroupName::string()) ->
    'ok' | {'error', Reason::atom()}.
remove_from_group(UserName, GroupName) ->
    ?GROUPMOD(remove_from_group(UserName, GroupName)).


%%
%% @spec login(UserName :: string(), Password :: string()) -> ok | {error, Reason::atom()}
%% @doc Login the user, given his <i>UserName</i> and <i>Password</i>.
%% Store the user name as "session:user_id", and the list of groups
%% that the user belongs to as "session:groups".
-spec login(UserName::string(), Password::string()) ->
    'ok' | {'error', Reason::atom()}.
login(UserName, Password) ->
    case ?USERMOD(authenticate(UserName, Password)) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            Groups = get_groups(UserName),
            wpart:finsert("session:user_id", UserName),
            wpart:finsert("session:groups", Groups),
            ok
    end.

%%
%% @spec logout() -> ok
%% @doc Logout the current user.
-spec logout() -> 'ok'.
logout() ->
    wpart:finsert("session:groups", undefined),
    wpart:finsert("session:user_id", undefined),
    ok.

%%
%% @spec status() -> bool()
%% @doc Returns <i>true</i> if the current user is logged, <i>false</i> otherwise.
%% @end
%%
-spec status() -> bool().
status() ->
    case username() of
        undefined ->
            false;
        _Else ->
            true
    end.

%%
%% @spec username() -> string() | undefined
%% @doc Returns the Username for the current user. The atom <i>undefined</i> otherwise.
%% @end
%%
-spec username() -> string() | 'undefined'.
username() ->
    wpart:fget("session:user_id").

%%
%% @spec authorize(AllowedGroups :: [string()]) -> ok | {error, Reason}
%% @doc Authorize the user, depending on group membership.
%% Returns <i>ok</i> if the user is a member of one of the
%% <i>AllowedGroups</i>, <i>{error, Reason}</i> otherwise.
%% @end
%%
-spec authorize(AllowedGroups::[string()]) ->
    'ok' | {'error', Reason::atom()}.
authorize(AllowedGroups) ->
    UserGroups = case status() of
                     true ->
                         wpart:fget("session:groups");
                     false ->
                         ["guest"]
                 end,
    case UserGroups of
        undefined ->
            {error, authorization};
        _ ->
            case lists:any(fun(UserGroup) ->
                                   lists:member(UserGroup, AllowedGroups)
                           end, UserGroups) of
                true ->
                    ok;
                false ->
                    {error, authorization}
            end
    end.
