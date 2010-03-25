-module(wtype_post).
-export([get_record_info/1]).
-export([create/1, read/1, update/1, delete/1]).
-export([prepare_initial/0, prepare_validated/0, prepare_edit/1]).
-export([format/1]).
-include("lib/blog-0.1/include/post.hrl").

get_record_info(post) -> record_info(fields, post);
get_record_info(post_types) -> #post_types{}.

create(Post) ->
    e_db:write(post, Post).

read(all) ->
    e_db:read(post);
read(Id) ->
    e_db:read(post, Id).

update(Post) ->
    e_db:update(post, Post).

delete(Id) ->
    e_db:delete(post, Id).

prepare_initial() ->
    wpart_db:build_record_structure(post, #post{}).

prepare_validated() ->
    Post = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(post, Post).

prepare_edit(Item) ->
    wpart_db:build_record_structure(post, Item).

format(Post) ->
  [
    {"id", Post#post.id},
    {"title", Post#post.title},
    {"author", Post#post.author},
    {"body", Post#post.body}
  ].

