# Install

Twitter Kit uses [Rebar](https://github.com/rebar/rebar) as its build tool. You just need to add it to your `rebar.config` as a dependency:

    {deps, [
            {twitter_kit, "1.*", {git, "ssh://tiamat.mobilarti.com/git/twitter_kit.git", "master"}}]}.

Twitter Kit uses [jsx](https://github.com/talentdeficit/jsx) to decode JSON responses. You don't need to add it to your config.
