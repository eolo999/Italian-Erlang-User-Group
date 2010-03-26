%% @doc Behaviour specification for group authentication backends.
-module(e_auth_group).

-export([behaviour_info/1]).

-spec behaviour_info('callbacks') ->
    [{atom(), non_neg_integer()}].
behaviour_info(callbacks) ->
    [{add_group, 1},
     {rename_group, 2},
     {delete_group, 1},
     {get_groups, 0},
     {get_groups, 1},
     {add_to_group, 2},
     {remove_from_group, 2}].
