%% you code here
{application, mylib, [
    {description, "Mylib"},
    {vsn, "0.1"},
    {modules, [mylib_app,mylib_sup,mylib_worker]},
    {env, [
        {min_val, 2},
        {max_val, 10},
        {connection_timeout, 10000},
        {query_timeout, 10000}
    ]},
    {mod, {mylib_app, []}}
]}.