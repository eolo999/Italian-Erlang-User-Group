-record(post, {
    id,
    title = "",
    author = "",
    body = ""
  }).

-record(post_types, {
    id = {integer, [primary_key, {private, true}]},
    title = {string, [{max_length, 256}]},
    author = {string, [{max_length, 256}]},
    body = {text, [{max_length, 50000}]}
  }).
