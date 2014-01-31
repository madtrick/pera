-module(pera_response).
-include("pera.hrl").

-export([new/2]).

-spec new(
  Data       :: any(),
  Serializer :: module()
  ) -> #pera_response{}.
new(Data, Serializer) ->
  #pera_response{ raw = Data, serializer = Serializer}.
