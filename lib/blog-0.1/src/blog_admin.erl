-module(blog_admin).
-export([do_add/1]).

do_add(_Args) ->
  case validate_tool:validate_cu(post, create) of
    {ok, Post} ->
      wtype_post:create(Post),
      {redirect, "/section/blog/"};
    {error, _Reason} ->
      wpart:fset("__edit", wtype_post:prepare_validated()),
      {template, "post/add.html"}
  end.

