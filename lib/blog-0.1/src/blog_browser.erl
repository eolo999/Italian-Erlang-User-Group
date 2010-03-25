-module(blog_browser).
-export([show_post/1, list_all/1]).

show_post(Args) ->
  N = list_to_integer(proplists:get_value(id, Args)),
  Post = wtype_post:read(N),
  wpart:fset("post", wtype_post:format(Post)),

  {template, "post/show.html"}.

list_all(_Args) ->
  Posts = wtype_post:read(all),
  wpart:fset("posts", lists:map(fun wtype_post:format/1, Posts)),
  {template, "post/show_all.html"}.
