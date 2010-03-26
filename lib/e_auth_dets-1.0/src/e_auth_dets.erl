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

%%%-------------------------------------------------------------------
%%% File    : e_auth_dets.erl
%%% @author Roberto Aloi <roberto.aloi@erlang-consulting.com>
%%% @doc An interface to the Dets authentication/authorization engine.
%%% This module should not be used explicitly, it should be only called from
%%% the <i>e_auth</i>.<br/>
%%% @see e_auth
%%% @end
%%%-------------------------------------------------------------------
-module(e_auth_dets).
-behaviour(e_component).
-behaviour(e_auth_user).
-behaviour(e_auth_group).

-export([install/1, uninstall/1, dependencies/0]).

-export([add_user/2, delete_user/1, get_users/0, change_password/2]).
-export([rename_user/2]).
-export([add_group/1, delete_group/1]).
-export([add_to_group/2, remove_from_group/2, get_groups/1, rename_group/2]).
-export([authenticate/2]).
-export([get_groups/0]).

-spec install(_) -> 'ok'.
install(_Conf) ->
    e_conf:set_conf(e_auth, ?MODULE),
    {ok, _} = dets:open_file(users, []),
    {ok, _} = dets:open_file(groups, [{type, bag}]),
    ok.

-spec uninstall(_) -> 'ok'.
uninstall(_Conf) ->
    dets:close(users),
    dets:close(groups),
    e_conf:set_conf(e_auth, undefined),
    ok.

-spec dependencies() -> [{'e_auth', {float(), 'undefined'}},...].
dependencies() ->
    [{e_auth, {1.0, undefined}}].

%%
%% @spec add_user(UserName::string(), Password::string()) -> ok
%% @equiv e_auth:add_user(UserName, Password)
%%	    
-spec add_user(UserName::string(), Password::string()) -> 'ok'.
add_user(Username, Password) ->
    Salt = crypto:rand_bytes(16),
    Encrypted = crypto:sha([Salt, Password]),
    case dets:insert_new(users, {Username, Salt, Encrypted}) of
        true ->
            ok;
        false ->
            {error, user_exists}
    end.

%%
%% @spec delete_user(UserName::string()) ->
%%   ok | {error, Reason::atom()}
%% @equiv e_auth:delete_user(UserName)
%%	    
-spec delete_user(UserName::string()) ->
    'ok' | {'error', Reason::atom()}.
delete_user(UserName) ->
    ok = dets:delete(users, UserName),
    ok = dets:match_delete(groups, {'_', UserName}).

%% @spec rename_user(OldUserName::string(), NewUserName::string()) ->
%%   ok | {error, Reason::atom()}
%% @equiv e_auth:rename_user(OldUserName, NewUserName)
-spec rename_user(OldUserName::string(), NewUserName::string()) ->
    'ok' | {'error', Reason::atom()}.
rename_user(OldUserName, NewUserName) ->
    case dets:lookup(users, OldUserName) of
        [] ->
            {error, not_found};
        [{OldUserName, Salt, Encrypted}] ->
            case dets:insert_new(users, {NewUserName, Salt, Encrypted}) of
                true ->
                    Groups = dets:match(groups, {'$1', OldUserName}),
                    dets:match_delete(groups, {'_', OldUserName}),
                    lists:foreach(fun([Group]) ->
                                          dets:insert(groups, {Group, NewUserName})
                                  end, Groups),

                    dets:delete(users, OldUserName);
                false ->
                    {error, user_exists}
            end
    end.

%%
%% @spec get_users() -> [string()]
%% @equiv e_auth:get_users()
%%	    
-spec get_users() -> [UserName::string()].
get_users() ->
    lists:append(dets:match(users,{'$1','_','_'})).

%%
%% @spec change_password(UserName::string(), NewPassword::string()) ->
%%   ok | {error, Reason::atom()}
%% @equiv e_auth:change_password(UserName, NewPassword)
-spec change_password(UserName::string(), NewPassword::string()) ->
    'ok' | {'error', Reason::atom()}.
change_password(UserName, NewPassword) ->
    case dets:lookup(users, UserName) of
        [{UserName, _OldSalt, _OldPassword}] ->
            Salt = crypto:rand_bytes(16),
            Encrypted = crypto:sha([Salt, NewPassword]),
            dets:insert(users, {UserName, Salt, Encrypted});
        _ ->
            {error, no_such_user}
    end.

%%
%% @spec authenticate(UserName::string(), Password::string()) ->
%%   ok | {error, Reason::atom()}
%% @equiv e_auth:authenticate(UserName, Password)
-spec authenticate(UserName::string(), Password::string()) ->
    'ok' | {'error', Reason::atom()}.
authenticate(Username, Password) ->
    case dets:lookup(users, Username) of
        [{Username, Salt, EncryptedPassword}] ->
            case crypto:sha([Salt, Password]) of
                EncryptedPassword ->
                    ok;
                _ ->
                    {error, authentication}
            end;
        _ ->
            {error, no_such_user}
    end.

%%
%% @spec add_group(GroupName::string()) -> ok
%% @equiv e_auth:add_group(GroupName)
-spec add_group(GroupName::string()) ->
    'ok'. %% | {'error', Reason::atom()}.
add_group(GroupName) ->
    dets:insert(groups, {GroupName, exists}),
    ok.

%%
%% @spec delete_group(GroupName::string()) ->
%%   ok | {error, Reason::atom()}
%% @equiv e_auth:delete_group(GroupName)
-spec delete_group(GroupName::string()) ->
    'ok' | {'error', Reason::atom()}.
delete_group(GroupName) ->
    dets:match_delete(groups, {GroupName, '_'}).

%%
%% @spec get_groups(UserName::string()) -> {ok, Groups :: list(list())} | {error, Reason}
%% @equiv e_auth:get_groups(UserName)
%%	    
-spec get_groups(UserName::string()) -> [Group::string()].
get_groups(Username) ->
    lists:append(dets:match(groups, {'$1', Username})).

%% @spec get_groups() -> [Groups::string()]
-spec get_groups() -> [Group::string()].
get_groups() ->
    Entries = dets:match(groups, {'$1', '_'}),
    Appended = lists:append(Entries),
    Sorted = lists:usort(Appended),
    Sorted.

%%
%% @spec add_to_group(UserName::string(), GroupName::string()) ->
%%   ok | {error, Reason::atom()}
%% @equiv e_auth:add_to_group(UserName, GroupName)
-spec add_to_group(UserName::string(), GroupName::string()) ->
    'ok' | {'error', Reason::atom()}.
add_to_group(UserName, GroupName) ->
    case {dets:member(users, UserName),
          dets:member(groups, GroupName)} of
        {true, true} ->
            dets:insert(groups, {GroupName, UserName});
        {false, _} ->
            {error, no_such_user};
        {_, false} ->
            {error, no_such_group}
    end.

%%
%% @spec remove_from_group(UserName::string(), GroupName::string()) ->
%%   ok | {error, Reason::atom()}
%% @equiv e_auth:remove_from_group(UserName, GroupName)
-spec remove_from_group(UserName::string(), GroupName::string()) ->
    'ok' | {'error', Reason::atom()}.
remove_from_group(UserName, GroupName) ->
    dets:match_delete(groups, {GroupName, UserName}).

%%
%% @spec rename_group(OldName::string(), NewName::string()) -> ok | {error, Reason::atom()}
%% @equiv e_auth:rename_group(OldName, NewName)
-spec rename_group(OldName::string(), NewName::string()) ->
    'ok' | {'error', Reason::atom()}.
rename_group(OldName, NewName) ->
    case {dets:member(groups, OldName),
          dets:member(groups, NewName)} of
        {true, false} ->
            Entries = dets:match_object(groups, {OldName, '_'}),
            lists:foreach(fun(Entry) ->
                                  dets:delete_object(groups, Entry)
                          end, Entries),
            dets:insert(groups, [{NewName, User} || {_, User} <- Entries]);
        {false, _} ->
            {error, no_such_group};
        {_, true} ->
            {error, group_exists}
    end.

