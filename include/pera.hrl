-type pera_hal_link_option() ::
  { templated   , boolean()} |
  { type        , binary() } |
  { deprecation , binary() } |
  { name        , binary() } |
  { profile     , binary() } |
  { title       , binary() } |
  { hreflang    , binary() }.

-record(pera_hal_link_data, {
    rel     :: binary(),
    href    :: binary(),
    options :: list(pera_hal_link_option())
  }).

-record(pera_hal_resource_data, {
    links      :: list(pera_hal_link()),
    embedded   :: pera_hal_embedded() | list(pera_hal_embedded()),
    properties :: list(pera_hal_resource_property())
  }).

-type pera_hal_link()              :: #pera_hal_link_data{}.
-type pera_hal_resource_property() :: {binary(), atom() | binary() | boolean() | integer() | list()}.
-type pera_hal_resource()          :: #pera_hal_resource_data{}.
-type pera_hal_embedded()          :: {Rel :: binary(), Resource :: pera_hal_resource() | list(pera_hal_resource())}.
