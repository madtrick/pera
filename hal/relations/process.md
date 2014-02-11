## Process

### Method: GET

Returns information about a particular process.

The link is a [URI template](http://tools.ietf.org/html/rfc6570) containing the variable 'items'. This variable can be a list of infoitems that will be requested for the process. The list of items that can be requested can be found in the function [erlang:process_info/2](http://www.erlang.org/doc/man/erlang.html#process_info-2). If the variable 'items' is not present, all the infoitems will be requested for the process.
