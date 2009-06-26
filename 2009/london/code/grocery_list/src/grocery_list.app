{application, grocery_list,
 [{description, "grocery_list"},
  {vsn, "0.1"},
  {modules, [
    grocery_list,
    grocery_list_app,
    grocery_list_sup,
    grocery_list_deps,
    grocery_list_resource
  ]},
  {registered, []},
  {mod, {grocery_list_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
