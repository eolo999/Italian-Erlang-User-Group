-module(blog_admin).
-export([do_add/1, delete/1]).

do_add(_Args) ->
  case validate_tool:validate_cu(post, create) of
    {ok, Post} ->
      wtype_post:create(Post),
      {redirect, "/section/blog/"};
    {error, _Reason} ->
      wpart:fset("__edit", wtype_post:prepare_validated()),
      {template, "post/add.html"}
  end.

delete(Args) ->
  Id = list_to_integer(proplists:get_value(id, Args)),
  wtype_post:delete(Id),
  {redirect, "/section/blog/"}.

