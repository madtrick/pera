-type pera_hal_link_option() ::
  { templated   , boolean()} |
  { type        , binary() } |
  { deprecation , binary() } |
  { name        , binary() } |
  { profile     , binary() } |
  { title       , binary() } |
  { hreflang    , binary() }.

-record(pera_hal_rel_data, {
    name  :: atom(),
    links :: pera_hal_link() | list(pera_hal_link())
  }).

-record(pera_hal_link_data, {
    href    :: binary(),
    options :: list(pera_hal_link_option())
  }).

-record(pera_hal_resource_data, {
    rels       :: list(pera_hal_rel()),
    embedded   :: pera_hal_embedded() | list(pera_hal_embedded()),
    properties :: list(pera_hal_resource_property_object())
  }).

-type pera_hal_rel()                      :: #pera_hal_rel_data{}.
-type pera_hal_link()                     :: #pera_hal_link_data{}.
-type pera_hal_resource_property_object() :: {object, list(pera_hal_resource_property())}.
-type pera_hal_resource_property_value()  :: atom() | binary() | boolean() | integer() | list() | list(pera_hal_resource_property()) | pera_hal_resource_property_object().
-type pera_hal_resource_property()        :: {binary() | atom(), pera_hal_resource_property_value()}.
-type pera_hal_resource()                 :: #pera_hal_resource_data{}.
-type pera_hal_embedded()                 :: {Rel :: binary() | atom(), Resource :: pera_hal_resource() | list(pera_hal_resource())}.
