%%% ===================================================================
%%% @author  Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @copyright (C) 2009 Erlang Training & Consulting Ltd.
%%% ===================================================================
{application, e_auth_dets,
 [{description, "Authorization and authentication DETS e_auth implementation"},
  {vsn, "1.0"},
  {modules, [e_auth_dets]},
  {registered,[]},
  {env,[]},
  {applications, [kernel, stdlib, sasl, crypto, eptic, e_auth]}]}.
