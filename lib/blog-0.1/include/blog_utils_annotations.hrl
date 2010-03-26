-compile({parse_transform, e_user_annotation}).
-compile(nowarn_shadow_vars).

-define(VALIDATE(Args), -ew_user_annotation({Args, before, blog_utils, validate})).

-define(CHECK_EXISTENCE(Args), -ew_user_annotation({Args, before, blog_utils, check_existence})).

-define(AUTHORIZE(Args), -ew_user_annotation({Args, before, blog_utils, authorize})).

