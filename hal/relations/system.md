## System

### Method: GET

Returns information about the erlang emulator.

The link is a [URI template](http://tools.ietf.org/html/rfc6570) containing the variable 'items'. This variable can be a list of infoitems that will be requested about the emulator. The list of items that can be requested can be found in the function [erlang:system_info/1](http://www.erlang.org/doc/man/erlang.html#system_info-1) excluding the infoitems for allocators of cpu_topology. If the variable 'items' is not present, all the infoitems will be requested for the emulator.

NOTE: the infoitems 'info' and 'procs' are excluded from response given to a request without query. If you want to view them, include them in the infoitems list.

