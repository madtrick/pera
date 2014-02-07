-module(pera_hal).
-include("pera.hrl").

%% @hidden
%% @doc Module with utility methods to generate representations
%% of resources according to the <a href="http://stateless.co/hal_specification.html">HAL+JSON</a>
%% specification

-export([link/3]).
-export([resource/3]).


%%========================================
%% API
%%========================================

%% @doc Creates a link with the expected fields
%% for a HAL link
-spec link(
  Rel     :: atom() | binary(),
  HREF    :: binary(),
  Options :: list(pera_hal_link_option())
  ) -> pera_hal_link().
link(Rel, HREF, Options) ->
  pera_hal_link_data:new(Rel, HREF, Options).

%% @doc Creates a HAL resource
-spec resource(
  Links      :: list(pera_hal_link()),
  Embedded   :: pera_hal_embedded() | list(pera_hal_embedded()),
  Properties :: list(pera_hal_resource_property())
  ) -> pera_hal_resource().
resource(Links, Embedded, Properties) ->
  pera_hal_resource_data:new(Links, Embedded, Properties).
