-module(db_init).
-export([init/0]).
-define(TABS, [post]).
-include("lib/blog-0.1/include/post.hrl").

init() ->
  mnesia:stop(),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok ->
      application:start(mnesia),
      e_db:install(),
      [install(Table) || Table <- ?TABS];

    {error, {Node, {already_exists, Node}}} ->
      application:start(mnesia),
      error_logger:warning_msg("~p module, mnesia's scheme already exists.", [?MODULE]),
      {error, schema_already_exists}
  end.

install(Name) ->
  mnesia:create_table(Name, [
      {attributes, 
        (list_to_atom("wtype_" ++ atom_to_list(Name))):get_record_info(Name)},
      {disc_copies, 
        [node()]}
    ]).
