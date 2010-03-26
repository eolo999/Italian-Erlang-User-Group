-record(post, {
    id,
    title = "",
    author = "",
    body = ""
  }).

-record(post_types, {
    id = {integer, [primary_key, {private, true}]},
    title = {string, [
        {description, "Title"},
        {max_length, 256}
      ]},
    author = {string, [
        {description, "Author"},
        {max_length, 256}
      ]},
    body = {text, [
        {description, "Body"},
        {max_length, 50000}
      ]}
  }).
