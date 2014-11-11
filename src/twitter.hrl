
-record(twitter, {
    auth = nil,
    format = json,
    domain = "api.twitter.com",
    secure = true,
    api_version = "1.1"
}).


-record(twitter_chunk, {
    api = nil,
    first_id = 0,
    last_id = 0,
    count = 0,
    tweets = []
}).