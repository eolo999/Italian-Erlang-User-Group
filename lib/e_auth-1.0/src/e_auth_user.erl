%% @doc Behaviour specification for user authentication backends.
-module(e_auth_user).

-export([behaviour_info/1]).

-spec behaviour_info('callbacks') ->
    [{atom(), non_neg_integer()}].
behaviour_info(callbacks) ->
    [{add_user, 2},
     {delete_user, 1},
     {rename_user, 2},
     {change_password, 2},
     {get_users, 0},
     {authenticate, 2}].
