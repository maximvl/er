##About

Er is ETS register. Main goal is to have safe ets owner processes, never lose tables when process dies and table registration under any erlang term.

Er has two separate name spaces (processes) for handling public and non-public etses. Public tables accessed as usual, while private and protected should be accepted by a process in a ```ets:give_avay(Tab, _, {er_priv, Name})``` way.
For non-public tables er automatically sets ```heir``` option, so these tables will be returned back to er if current owner dies and no data will be lost.

##API

* ```new(Name, Opts)``` - creates table and registers under ```Name```, ```Opts``` is usual ets options
* ```replace(Name, Opts)``` - replaces existing or non-existing table under name ```Name```
* ```get_or_new(Name, Opts)``` - find or create new table with given name
* ```get_public(Name)``` - get public table with given name
* ```get_private(Name)``` - get non-public (protected or private) table with given name
* ```del_public(Name)``` - delete public table
* ```del_private(Name)``` - delete private table

## TODO

* get_protected when only reading is required?
