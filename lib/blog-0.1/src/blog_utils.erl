-module(blog_utils).

-export([validate/4]).
-export([check_existence/4]).
-export([authorize/4]).

-include("../../eptic-1.4/include/e_annotation.hrl").

?BEFORE.
validate({Model, Type}, Mod, Fun, _Args) ->
  case validate_tool:validate_cu(Model, Type) of
    {ok, Post} ->
      {proceed, [Post]};
    {error, _Reason} ->
      {error, {Mod, validate_error, [Fun]}}
  end.

?BEFORE.
check_existence({id, Model}, Mod, _Fun, [Args]) ->
  Id = list_to_integer(proplists:get_value(id, Args)),
  case (list_to_atom("wtype_" ++ atom_to_list(Model))):read(Id) of
    not_found ->
      {error, {Mod, element_not_found, [Id]}};
    Element ->
      {proceed, [Element]}
  end;
check_existence({element, Model}, Mod, _Fun, [Element]) ->
  Id = element(2, Element),
  case (list_to_atom("wtype_" ++ atom_to_list(Model))):read(Id) of
    not_found ->
      {error, {Mod, element_not_found, [Id]}};
    _ ->
      {proceed, [Element]}
  end.

?BEFORE.
authorize(_AnnArg, _Mod, _Fun, Args) ->
  case e_auth:status() of
    true ->
      {proceed, Args};
    false ->
      {skip, {redirect, "/login"}}
  end.

