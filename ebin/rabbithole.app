{application, rabbithole,
 [
  {description, ""},
  {vsn, "0.1"},
  {modules, [
             rabbithole_app,
             rabbithole_sup,
             rabbithole, rabbithole_srv,
             % Interfaces
             rabbithole_interface_sup,
             gproc_interface, rabbitmq_interface
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { rabbithole_app, []}},
  {env, []}
 ]}.
