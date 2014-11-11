
-record(oauth, {
    token = "",
    token_secret = "",
    consumer_key = "",
    consumer_secret = "",
    app_token = ""
}).


-record(twitter, {
    auth = nil,
    format = json,
    domain = "api.twitter.com",
    secure = true,
    api_version = "1.1"
}).


-record(twitter_pointer, {
    api = nil,
    path = "",
    args = [],
    first_id = 0,
    last_id = 0,
    count = 0
}).