-module(blog_admin).
-export([add/1, do_add/1, delete/1]).
-export([validate_error/1, element_not_found/1]).
-include("blog_utils_annotations.hrl").

?AUTHORIZE(not_used).
add(_Post) ->
  {template, "post/add.html"}.

?AUTHORIZE(not_used).
?VALIDATE({post, create}).
do_add(Post) ->
  wtype_post:create(Post),
  {redirect, "/blog/"}.

?AUTHORIZE(not_used).
?CHECK_EXISTENCE({id, post}).
delete(Post) ->
  wtype_post:delete(element(2, Post)),
  {redirect, "/blog/"}.

validate_error(do_add) ->
  wpart:fset("__edit", wtype_post:prepare_validated()),
  {template, "post/add.html"}.

element_not_found(Id) ->
  wpart:fset("id", Id),
  {template, "post/not_found.html"}.
