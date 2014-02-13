-define(HAL_RESOURCE(Rels, Embeddeds, Properties), pera_hal:resource(Rels, Embeddeds, Properties)).
-define(HAL_REL(Name, Links), pera_hal:rel(Name, Links)).
-define(HAL_LINK(Href, Properties), pera_hal:link(Href, Properties)).
-define(HAL_EMBEDDED(Name, Resources), {Name, Resources}).
-define(HAL_PROPERTY(Name, Value), {Name, Value}).
-define(HAL_PROPERTY_OBJECT(Properties), {object, Properties}).