-module(session).
-export([language/1]).

-define(LANGUAGES, ["it", "en"]).

language(Args) ->
    LanguageId = proplists:get_value(id, Args),
    case language_is_supported(LanguageId) of
        false ->
            ok;
        true ->
            wpart:fset("session:lang", LanguageId)
    end,
    % FIXME: This should redirect to the page previously visited by the user. How?
    {redirect, "/"}.

language_is_supported(LanguageId) ->
    lists:member(LanguageId, ?LANGUAGES).
