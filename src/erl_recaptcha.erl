% Copyright (c) 2009 Buddy Moore
% 
% Permission is hereby granted, free of charge, to any person
% obtaining a copy of this software and associated documentation
% files (the "Software"), to deal in the Software without
% restriction, including without limitation the rights to use,
% copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following
% conditions:
% 
% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
% OTHER DEALINGS IN THE SOFTWARE.

-module(erl_recaptcha).

-export([
  display_html/3,
  submit/4
]).

-define(API_SSL_SERVER, "https://api-secure.recaptcha.net").
-define(API_SERVER, "http://api.recaptcha.net").
-define(VERIFY_SERVER, "http://api-verify.recaptcha.net/verify").

display_html(PublicKey, UseSSL, Error) ->
  ErrorParam = case Error of
    undefined ->
      ""
    ;
    ErrorCode ->
      "&error="++ErrorCode
  end,
  ApiServer = case UseSSL of
    true ->
      ?API_SSL_SERVER
    ;
    false ->
      ?API_SERVER
  end,
  "<script type=\"text/javascript\" src=\""++ApiServer++"/challenge?k="++PublicKey++ErrorParam++"\"></script>"
.

submit(PrivateKey, RemoteIp, Challenge, Response) ->
  inets:start(),
	Data = 
		"privatekey="++ PrivateKey ++ "&"
		"remoteip=" ++ RemoteIp ++ "&"
		"challenge=" ++ Challenge ++ "&"
		"response=" ++ Response,
  HttpResponse = 
    http:request(
      post,
      {
        ?VERIFY_SERVER,
        [
          {"User-Agent", "reCAPTCHA/erlang"},
          {"Content-Length", integer_to_list(string:len(Data))}
        ],
        "application/x-www-form-urlencoded",
        Data
      },
      [], []
    ),
    error_logger:info_msg("ReCAPTCHA Response~n~p", [HttpResponse]),
  BodyStr = 
    case HttpResponse of
      {ok, saved_to_file} -> "saved";
      {ok, Result} -> 
        case Result of
          {_Status, _Headers, Body} -> Body;
          {_Status, Body} -> Body
        end;
      {error, _Reason} -> "error"
    end,
  Lines = string:tokens(BodyStr, "\r\n"),
  [Line1 | Rest] = Lines,
  [Line2 | _Rest2] = Rest,
  {Line1, Line2}
.
