-record(post, {
    id,
    title = "",
    author = "",
    date,
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
    date = {date, [
        {description, "Date"},
        {format, "YYYY:MM:DD"},
        {max, {2020,1,1}},
        {min, {1999,1,1}}
      ]},
    body = {text, [
        {description, "Body"},
        {max_length, 50000},
        {html_attrs,[{rows,10},
                     {cols,70}
                    ]
        },
        {html_whitelist, ["u", "b", "i", "a", "h1", "h2", "h3",
       "ul", "ol", "li", "br", "hr", "img",
       "center"]}
  ]}}).
