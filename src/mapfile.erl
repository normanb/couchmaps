% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

% simple non-validating pattern matching MapServer mapfile parser / writer

-module(mapfile).

-export([to_json/1, from_json/1, test/0]).

-include("couch_db.hrl").

-define(QUERYMAP, "QUERYMAP"). 
-define(WEB, "WEB"). 
-define(METADATA, "METADATA"). 
-define(OUTPUTFORMAT, "OUTPUTFORMAT"). 
-define(LEGEND, "LEGEND"). 
-define(SCALEBAR, "SCALEBAR"). 
-define(LABEL, "LABEL"). 
-define(STYLE, "STYLE"). 
-define(CLASS, "CLASS"). 
-define(POINTS, "POINTS"). 
-define(SYMBOL, "SYMBOL"). 
-define(FEATURE, "FEATURE"). 
-define(GRID, "GRID"). 
-define(JOIN, "JOIN"). 
-define(REFERENCE, "REFERENCE"). 
-define(CLUSTER, "CLUSTER"). 
-define(LAYER, "LAYER").
-define(LAYERSET, "LAYERSET"). 
-define(SYMBOLSET, "SYMBOLSET"). 
-define(PROJECTION, "PROJECTION").
-define(MAP, "MAP"). 
-define(CONFIG, "CONFIG").
-define(END, "END").

-define(NEWOBJ, ":{").
-define(COORDINATES, "coordinates").
-define(LAYERS, "layers").

from_json({MapJson}) ->
   % parse a JSON map file
   Result = parse(MapJson, <<"MAP">>), 
   {ok, <<Result/binary, "\nEND">>}.
   
to_json(MapFile) ->
  % parse the tokenised string in reverse, creating objects 
  MapParts = binary:split(MapFile, [<<"\r\n">>, <<"\n">>], [trim, global]),
  {KeyList} = ?JSON_DECODE(map_match([remove_leading_spaces(B) || B <- MapParts], <<>>)),
  % post process group layer properties together in arrays
  {ok, group_layers(KeyList, [])}.   


% internal API from_json functions
parse([], Acc) ->
  Acc;

% simple key value
parse([{K, V} | Rem], Acc) when is_binary(V) ->
  MapLine = write_line(K, V),
  parse(Rem, <<Acc/binary, "\n",  MapLine/binary>>);


parse([{<<?LAYERS>>, LayerList} | Rem], Acc) ->
  % reconstruct individual layer objects
  NewAcc = parse([{<<?LAYER>>, Tuple} || Tuple <- LayerList], <<>>),
  parse(Rem, <<Acc/binary, "\n", NewAcc/binary>>);

% value is a JSON list
parse([{K, V} | Rem], Acc) when is_list(V) ->
   MapLine = write_line(K, V),
   parse(Rem, <<Acc/binary, "\n", MapLine/binary>>);

% CONFIG definition, special case
parse([{<<"config">>, {[{K, V}]}} | Rem], Acc) ->
  NewK = ?l2b(string:to_upper(?b2l(K))),
  parse(Rem, <<Acc/binary, "\n", "CONFIG ", NewK/binary, " \"", V/binary, "\"">>);

parse([{K, {ObjectList}}| Rem], Acc) ->
  NewK = ?l2b(string:to_upper(?b2l(K))),
  NewAcc = parse(ObjectList, <<>>),
  parse(Rem, <<Acc/binary, "\n", NewK/binary, NewAcc/binary, "\nEND">>).

write_line(<<"init">>, V) ->
    <<"\"init=", V/binary, "\"">>;
    
write_line(<<"coordinates">>, V) ->
   write_line(<<"">>, V);

write_line(K, V) when is_list(V) ->
   % when a list then K is a mapfile element
   Values = lists:foldl(fun(X, NewAcc) ->
            N = ?l2b(lists:flatten(io_lib:format("~p", [X]))),
	   <<NewAcc/binary, " ", N/binary>> 
	end, <<>>, V),
    NewK = ?l2b(string:to_upper(?b2l(K))),
    <<NewK/binary, Values/binary>>;
	
write_line(K, V) ->
   % check that K is not a mapfile element
   case process_element(string:to_upper(?b2l(K))) of 
       {map_element, quote} ->
            % map elements are in upper case without double quotes, in this case quote the value
           NewK = ?l2b(string:to_upper(?b2l(K))),
	   <<NewK/binary, " \"", V/binary, "\"">>;
       map_element ->
            % map elements are in upper case without double quotes
           NewK = ?l2b(string:to_upper(?b2l(K))),
	   <<NewK/binary, " ", V/binary>>;
	_ ->
	   <<"\"", K/binary, "\"", " \"", V/binary, "\"">>
    end.


process_element(?QUERYMAP) -> map_element;
process_element(?WEB) -> map_element;
process_element(?METADATA) -> map_element;
process_element(?OUTPUTFORMAT) -> map_element;
process_element(?LEGEND) -> map_element;
process_element(?SCALEBAR) -> map_element;
process_element(?LABEL) -> map_element;
process_element(?STYLE) -> map_element;
process_element(?CLASS) -> map_element;
process_element(?SYMBOL) -> map_element;
process_element(?FEATURE) -> map_element;
process_element(?POINTS) -> map_element;
process_element(?GRID) -> map_element;
process_element(?JOIN) -> map_element;
process_element(?REFERENCE) -> map_element;
process_element(?CLUSTER) -> map_element;
process_element(?LAYER) -> map_element;
process_element(?LAYERSET) -> map_element;
process_element(?SYMBOLSET) -> map_element;
process_element(?PROJECTION) -> map_element;
process_element("PROCESSING") -> {map_element, quote};
process_element("IMAGETYPE") -> map_element;
process_element("EXTENT") -> map_element;
process_element("SIZE") -> map_element;
process_element("IMAGEPATH") -> {map_element, quote};
process_element("IMAGEURL") -> {map_element, quote};
process_element("SHAPEPATH") -> map_element;
process_element("NAME") -> {map_element, quote};
process_element("FONTSET") -> map_element;
process_element("UNITS") -> map_element;
process_element("STATUS") -> map_element;
process_element("RESOLUTION") -> map_element;
process_element("CONFIG") -> map_element;
process_element("DATAPATTERN") -> map_element;
process_element("DEBUG") -> map_element;
process_element("IMAGECOLOR") -> map_element;
process_element("COLOR") -> map_element;
process_element("SCALE") -> map_element;
process_element("TEMPLATEPATTERN") -> map_element;
process_element("CONNECTIONTYPE") -> map_element;
process_element("CONNECTION") -> {map_element, quote};
process_element("TYPE") -> map_element;
process_element("FILLED") -> map_element;
process_element("TRANSFORM") -> map_element;
process_element("DUMP") -> map_element;
process_element("DATA") -> {map_element, quote};
process_element("TEXT") -> {map_element, quote};
process_element("DRIVER") -> {map_element, quote};
process_element("MIMETYPE") -> {map_element, quote};
process_element("IMAGEMODE") -> {map_element, quote};
process_element("EXTENSION") -> {map_element, quote};
process_element("FORMATOPTION") -> {map_element, quote};
process_element(_Element) -> false.

% internal API to_json functions

% match on mapfile elements

% entry point for a map file
map_match([<<?MAP, _/binary>> | Rem], Acc)  ->
   map_match(Rem, <<Acc/binary, "{">>, [{1, init}]).

map_match([], Acc, _Levels) ->
   Acc;

map_match([<<"#", _/binary>> | Rem], Acc, Levels) ->
  map_match(Rem, Acc, Levels);
  
map_match([<<?QUERYMAP, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?QUERYMAP, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?WEB, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?WEB, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>,  [new_level(Level) | Levels]); 

map_match([<<?METADATA, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?METADATA, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>,  [new_level(Level) | Levels]); 

map_match([<<?OUTPUTFORMAT, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?OUTPUTFORMAT, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?LEGEND, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?LEGEND, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?SCALEBAR, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?SCALEBAR, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?LABEL, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?LABEL, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?STYLE, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?STYLE, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?CLASS, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?CLASS, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?SYMBOL, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?SYMBOL, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?FEATURE, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?FEATURE, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 
  
map_match([<<?POINTS, _/binary>>, Coords, <<?END, _/binary>> |  Rem], Acc, [{Level, Init}| RemLevels]) ->
  Object = new_obj(?POINTS, Init),
  Points = make_property(remove_leading_spaces(<<?COORDINATES, " ", Coords/binary>>)),
  map_match(Rem, <<Acc/binary, Object/binary, Points/binary, "}">>, [{Level, undefined} | RemLevels]);   

map_match([<<?GRID, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?GRID, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?JOIN, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?JOIN, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?REFERENCE, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?REFERENCE, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?CLUSTER, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?CLUSTER, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?LAYER, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?LAYER, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?LAYERSET, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?LAYERSET, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?SYMBOLSET, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?SYMBOLSET, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?PROJECTION, _/binary>> | Rem], Acc, [{Level, Init}| _] = Levels) ->
  Object = new_obj(?PROJECTION, Init),
  map_match(Rem, <<Acc/binary, Object/binary>>, [new_level(Level) | Levels]); 

map_match([<<?CONFIG, Prop/binary>> | Rem], Acc, [{_Level, Init} | _] = Levels) ->
   Object = new_obj(?CONFIG, Init),
   ConfigProperty = make_property(remove_leading_spaces(Prop)),
   map_match(Rem, <<Acc/binary, Object/binary, ConfigProperty/binary, "}">>, Levels);

map_match([<<?END, _/binary>> | Rem], Acc, [_ | RemLevels]) ->
   map_match(Rem, <<Acc/binary, "}">>, RemLevels);

% if not a map file element then map to JSON object property

% ignore comments
map_match([<<"#", _>> | Rem], Acc, Levels) ->
   map_match(Rem, Acc, Levels);

% ignore empty space
map_match([<<>> | Rem], Acc, Levels) ->
   map_match(Rem, Acc, Levels);

% first property for this object, no comma
map_match([Prop| Rem], Acc, [{LevelNo, init} | RemLevels]) ->
  % split property on white space	
  PropValue = make_property(Prop),
  map_match(Rem, <<Acc/binary, PropValue/binary>>, [{LevelNo, undefined} | RemLevels]);

map_match([Prop| Rem], Acc, Levels) ->
  PropValue = make_property(Prop),
  map_match(Rem, <<Acc/binary, ",",PropValue/binary>>, Levels).

make_property(Prop) ->
  [K | RemSplit] = binary:split(Prop, [<<" ">>, <<"=">>], [trim, global]),
  % trim RemSplit of all whitespace, \t or # comment sections
  V = trim_value(RemSplit, <<>>),
  NewK = escape_string(?l2b(string:to_lower(?b2l(K)))),
  NewV = escape_string(V),  
  <<NewK/binary, ":", NewV/binary>>.

new_level(CurrentLevel) ->
  {CurrentLevel + 1, init}.
  
new_obj(Name, Init) when is_list(Name) ->
  new_obj(list_to_binary(string:to_lower(Name)), Init);
  
new_obj(Name, init) ->
   <<"\"", Name/binary, "\"", ?NEWOBJ>>;

new_obj(Name, _) ->
   <<",", "\"", Name/binary, "\"",?NEWOBJ>>.

trim_value([], Acc) ->
   Acc;

trim_value([<<"\t">> | Rem], Acc) ->
   trim_value(Rem, Acc);

trim_value([<<" ">> | Rem], Acc) ->
   trim_value(Rem, Acc);
   
trim_value([<<>> | Rem], Acc) ->
   trim_value(Rem, Acc);
   
trim_value(V, Acc) ->
   % lose trailing comments, extra spaces are also taken out
   {_, Values} = lists:foldl(fun(X, {Do, NewAcc}) ->
        case Do of
	   true ->
	      case X of 
	          <<>> -> 
		     {Do, NewAcc};
		  <<$#, _/binary>> ->
		      {false, NewAcc};
		  _ ->
		      {Do, [X|NewAcc]}
	       end;
	   _ ->
	      {Do, NewAcc}
	end
   end, {true, []}, V),

   PropValue = case length(Values) > 1 of
      true ->
         % create a json array of numbers or pad strings
	 [H | Rem] = lists:reverse(Values),
	 case H of
	    <<"\"", _/binary>> -> 
	        lists:foldl(fun(X, NewAcc) ->
		   <<NewAcc/binary, " ", X/binary>> 
		end, <<H/binary>>, Rem);
	    _ ->
		Result = lists:foldl(fun(X, NewAcc) ->
		     <<NewAcc/binary, ",", X/binary>>
		   end, <<"[", H/binary>>, Rem),
		<<Result/binary, "]">>
	end;
      _ ->
          [Result] = Values,
	  Result
   end,
   <<Acc/binary, PropValue/binary>>.

escape_string(V) ->
   <<Start, _/binary>> = V,
   Size = byte_size(V) - 1,
   <<_:Size/binary, Last>> = V,

   Tmp = case Start of 
      $" ->
          V;
      $[ ->
          V;
       _ ->
          <<"\"", V/binary>> 
   end,

   case Last of
        $" ->
	     Tmp;
	$] ->
	     Tmp;
	_ ->
	  <<Tmp/binary, "\"">>
   end.
   

remove_leading_spaces(<<"\t",Rest/binary>>) ->
    remove_leading_spaces(Rest);

remove_leading_spaces(<<" ",Rest/binary>>) ->
    remove_leading_spaces(Rest);
    
remove_leading_spaces(WithoutSpaces) ->
    WithoutSpaces.

group_layers(TupleList, Layers) ->
  case lists:keytake(<<"layer">>, 1, TupleList) of
    {value, LayerTuple, RemTuples} ->
       group_layers(RemTuples, [LayerTuple | Layers]);
    _ ->
      case length(Layers) of
          0 ->
            {TupleList};
          1 ->
            {TupleList ++ Layers};
          _ ->
 	    {TupleList  ++ [{<<?LAYERS>>, [LayerContents || {_, LayerContents} <- lists:reverse(Layers)] }]}
      end
  end.

test() ->
  {ok, MapFile} = file:read_file("sample.map"),
  {ok, JsonMap} = to_json(MapFile),  

  {ok, JsonFd} = file:open("result.json", [write]),
  file:write(JsonFd, ?JSON_ENCODE(JsonMap)),
  file:close(JsonFd),

  {ok, NewMapFile} = from_json(JsonMap),
  {ok, MapFd} = file:open("result.map", [write]),
  file:write(MapFd, NewMapFile),
  file:close(MapFd).
