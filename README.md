record_util
=====

record_util plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {record_util, {git, "https://github.com/HaotaoYang/record_util.git", {branch, "master"}}}
    ]}.

Add the config to your rebar config:
    %% record_util_opts
    {record_util_opts, [
        {hrl_dirs, ["include"]},
        {dest_dir, "src"},
        {module_name, "record_helper"}
    ]}.

    %% provider_hooks
    {provider_hooks, [
        {pre, [
            {compile, {record_util, pre_compile}},
            {clean, {record_util, pre_clean}}
        ]}
    ]}.
