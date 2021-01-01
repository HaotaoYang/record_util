record_util
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {record_util, {git, "https://host/user/record_util.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 record_util
    ===> Fetching record_util
    ===> Compiling record_util
    <Plugin Output>
