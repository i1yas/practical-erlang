%% you code here
{application, my_crypt, [
    {description, "My crypt app"},
    {vsn, "0.1"},
    {modules, [my_crypt_app, my_crypt_sup, my_crypt_worker]},
    {env, []},
    {mod, {my_crypt_app, []}}
]}.
