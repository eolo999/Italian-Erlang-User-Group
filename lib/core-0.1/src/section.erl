-module(section).

-export([display/1]).

-define(SECTIONS, ["what", "learn", "news", "jobs", "team", "blog", "links"]).

display(Args) ->
    SectionId = proplists:get_value(id, Args),
    case section_is_enabled(SectionId) of
        false ->
            {template, "404.html"};
        true ->
	    set_active(SectionId),
            {template, SectionId ++ ".html"}
    end.

section_is_enabled(SectionId) ->
    lists:member(SectionId, ?SECTIONS).

set_active(SectionId) ->
    wpart:fset("active_" ++ SectionId, "active").
