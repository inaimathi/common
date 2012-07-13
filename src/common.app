{application, common,
 [{description, "Common components that are included friggin everywhere"},
  {vsn, "1.0"},
  {modules, [common, db, gpg]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {common_app, []}},
  {start_phases, []}]}.