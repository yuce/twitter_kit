# Twitter Kit Tests

You need the following files in `fixtures` directory to be able to run tests and examples. There are already template files in that directory. You can just fill them in, and remove the `.txt` extension.

- `app_pre.fixture`: This fixture is needed for tests and examples which produces application (_bearer_) token from *consumer key* and *consumer secret*,
- `app_post.fixture`: Needed for tests/examples which uses an application token,
- `oauth_post.fixture`: Needed for tests/examples which uses OAuth authentication. You can use the token on your Twitter app console.

The tests are run using in the project root directory:

    rebar eunit skip_deps=true
