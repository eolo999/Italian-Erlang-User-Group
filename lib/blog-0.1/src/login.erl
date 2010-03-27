-module(login).
-export([login/1, logout/1]).

login(_Args) ->
  Username = wpart:fget("post:username"),
  Password = wpart:fget("post:password"),

  case e_auth:login(Username, Password) of
    ok ->
      {redirect, "/"};
    {error, Reason} ->
      wpart:fset("error", Reason),
      {template, "login/unsuccessful.html"}
  end.

logout(_Args) ->
  e_auth:logout(),
  {redirect, "/blog/"}.

