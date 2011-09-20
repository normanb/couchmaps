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
-module(mapserver_httpd).

-export([handle_request/2, ows_callback/2]).

-import(couch_httpd, [send_method_not_allowed/2, send_error/4, send_json/3]).

-include("couch_db.hrl").

%% Database request handlers

% list method, list returns a mapfile format in mapserver text format
handle_request(#httpd{
    method='GET',
    path_parts=[DbName, _MapServ,_Design, DDocName, _List, ListFunc, ViewFunc]
}=Req, _Db) ->
   % make http request to couchdb for map file, assume localhost 
   % ibrowse has already been started by CouchDB
   % assume localhost and vanilla http is enabled for localhost
   Port = couch_config:get("httpd", "port", "5984"),
   Link = lists:flatten(io_lib:format("http://localhost:~s/~s/_design/~s/_list/~s/~s",
	 [Port, DbName, DDocName, ListFunc, ViewFunc])),

   case ibrowse:send_req(Link, [], get) of
     {ok, _, _, ResponseBody} ->
	process(Req, ResponseBody);
     {error, Reason} ->
	couch_httpd:send_response(Req, 500, [], Reason)
   end;
   
   

handle_request(#httpd{
    method='GET',
    db_frontend=DbFrontend, % couchbase
    path_parts=[_DbName, _MapServ, MapFile]
}=Req, Db) ->
  % format the request
  % get the MapServer mapfile in JSON format from couchdb and call mapserver
  case DbFrontend:couch_doc_open(Db, MapFile, nil, []) of % couchbase
    % case couch_httpd_db:couch_doc_open(Db, MapFile, nil, []) of
    {not_found, _} ->
	% nothing to do send message back
	couch_httpd:send_response(Req, 204, [], <<>>);
    Doc ->
        {[_id, _rev | Rem]} = couch_doc:to_json_obj(Doc, []),
        % transform the JSON doc to MapServer mapfile format
        {ok, NewMapFile} = mapfile:from_json({Rem}),  
	process(Req, NewMapFile)
   end;

%% Database request handlers
handle_request(#httpd{
    method='PUT',
    path_parts=[_DbName, _MapServ, MapFile]
}=Req, Db) ->
  % have to be authenticated to do a put
  ok = couch_db:check_is_admin(Db),

  case is_number(couch_httpd:body_length(Req)) of
    false ->
      couch_httpd:send_error(Req, couch_httpd:error_info(bad_request));
    _ ->
      % get the post body
      {ok, JsonMap} = mapfile:to_json(couch_httpd:body(Req)), 
      % write this doc to couch,
      {DocId, NewRev} = write_doc_to_couch(Db, #doc{id=MapFile, revs={0, []}, body=JsonMap}, []),
      Loc = couch_httpd:absolute_uri(Req, "/" ++ ?b2l(Db#db.name) ++ "/" ++ ?b2l(DocId)),
      RespHeaders = [{"Location", Loc}],
      couch_httpd:send_json(Req, 201, RespHeaders, {[{id, DocId}, {rev, couch_doc:rev_to_str(NewRev)}]})
  end;

handle_request(Req, _Db) ->
    send_method_not_allowed(Req, "GET/PUT").

ows_callback(Data, {Req, 0}) ->
  Hdrs = binary_to_list(Data),
  [MimeType | MimeValue] = string:tokens(string:strip(Hdrs, right, $\n), ":"),
  {ok, Resp} = couch_httpd:start_chunked_response(Req, 200, [{MimeType, lists:flatten(MimeValue)}]),   
  {Resp, 1};

ows_callback(done, {Resp, _}) ->
  couch_httpd:last_chunk(Resp);

ows_callback(Data, {Resp, Counter}) ->
  couch_httpd:send_chunk(Resp, Data),
  {Resp, Counter + 1}.


% internal
process(Req, MapFile) ->        
  % create the temp file in TmpDir
  TmpDir = couch_config:get("mapserver", "tmp", "/tmp"),
        
  MapName = filename:join([TmpDir,
	      lists:flatten(io_lib:format("~p.map", [erlang:phash2(make_ref())]))]),
  {ok, Fd} = file:open(MapName, [write]),
  file:write(Fd, MapFile),
  file:close(Fd),
       
  QueryString = lists:foldl(fun({K, V}, Acc) ->
      Acc ++ "&" ++ K ++ "=" ++ V
    end, "map=" ++ MapName, couch_httpd:qs(Req)),

  Result = mapserver:request(QueryString, fun ?MODULE:ows_callback/2, {Req, 0}),
  file:delete(MapName),
  Result.

write_doc_to_couch(Db, Doc, Options) ->
   case couch_db:update_doc(Db,  Doc, Options) of
      {ok, NewRev} ->
         {Doc#doc.id, NewRev};
      Error ->
         throw({error, Error})
   end.
