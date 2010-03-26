%%% ===================================================================
%%% @author  Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @copyright (C) 2009 Erlang Training & Consulting Ltd.
%%% ===================================================================
{application, e_auth,
 [{description, "Authorization and authentication interface"},
  {vsn, "1.0"},
  {modules, [e_auth, e_auth_user, e_auth_group]},
  {registered,[]},
  {env,[]},
  {applications, [kernel, stdlib, sasl, eptic]}]}.
