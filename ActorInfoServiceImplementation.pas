unit ActorInfoServiceImplementation;

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.StrUtils,
  System.IOUtils,
  System.JSON,
  System.Math,
  System.NetEncoding,
  System.Generics.Collections,

  REST.JSON,

  XData.Server.Module,
  XData.Service.Common,
  XData.Sys.Exceptions,

  IdHTTP, IdSSLOpenSSL, idURI,

  System.Net.URLClient,
  System.Net.HttpClientComponent,
  System.Net.HttpClient,

  brotli,
  IdGlobalProtocols,

  ActorInfoService;

type
  [ServiceImplementation]
  TActorInfoService = class(TInterfacedObject, IActorInfoService)

      // Used to help ensure client is using latest version, hopefully avoiding any server caching issues
      function GetClientVersion(Day: String): TStream;

      // Lookup data directly rather than through a query of some kind
      function Lookup(Secret: String; Lookup: String; Progress: String):TStream;

      // These get data from Wikipedia which is then used as the source for locating TMDb data
      function BirthDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;
      function DeathDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;
      function ReleaseDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;
      function Relatives(Secret: String; RelatedTo:Integer; Progress: String):TStream;

      // Get Actor information based on dates
      function ActorBirthDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;
      function ActorDeathDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;
      function ActorBirthDay50(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;
      function ActorDeathDay50(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;

      // Get Movie information based on dates
      function MovieReleaseDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;

      // Get Actor information based on TMDb top 10,000 list
      function TopOneThousand(Secret: String; Progress: String):TStream;
      function TopFiveThousand(Secret: String; Progress: String):TStream;

      // Get the top actors for today (or another day) - used by ActoriousToday, for example
      function TopToday(Secret: String; aMonth: Integer; aDay: Integer):TStream;

      // Get information from a TMDb search
      function SearchPeople(Secret: String; SearchTerm: String; Progress: String):TStream;
      function SearchPeopleExtended(Secret: String; SearchTerm: String; Progress: String):TStream;

      // Return current progress of a request
      function Progress(Secret: String; Progress: String):String;

    end;

implementation

uses Unit2;

{ TActorInfoService }


///////////////////////////////////////////////////////////////////////////////////////////////////
// FilterResponse                                                                                //
//                                                                                               //
// Sometimes we get back JSON that has illegal characters in it. Much of the data from TMDb, for //
// example, is user-supplied and it seemingly doesn't filter these out.  As these trip up the    //
// various JSON processing functions downstream (in the client even) we need to get rid of them  //
// as soon as possible.                                                                          //
///////////////////////////////////////////////////////////////////////////////////////////////////

function FilterResponse(Response: String):String;
begin
  Result := Response;
  Result := StringReplace(Result, chr( 9),  '',    [rfReplaceAll]);   // Tab
  Result := StringReplace(Result, chr(10),  '',    [rfReplaceAll]);   // NL
  Result := StringReplace(Result, chr(13),  '',    [rfReplaceAll]);   // CR
  Result := StringReplace(Result, '\u0013', '',    [rfReplaceAll]);
  Result := StringReplace(Result, '\S',     '/S',  [rfReplaceAll]);
  Result := StringReplace(Result, ' \ ',    ' / ', [rfReplaceAll]);
end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// SetBrotliHeaders                                                                              //
//                                                                                               //
// When returning data, we're going to be sending back Brotli-compressed files, so we need the   //
// headers to reflect this, and also that the data is JSON.                                      //
///////////////////////////////////////////////////////////////////////////////////////////////////

procedure SetBrotliHeaders;
begin
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// GetImageURI                                                                                   //
//                                                                                               //
// Given an image URL (expecting a TMDb image reference here) the actual image is retrieved and  //
// then converted to a Data URI.  This is all to pass to the first launch of the Actorious app,  //
// so we can display as much of the first page as possible (mostly the initial top section)      //
// without having to do another fetch.                                                           //
//                                                                                               //
// This is complicated slightly by wanting to encode Base64 without using any line breaks, which //
// are generally not allowed in JSON.                                                            //
///////////////////////////////////////////////////////////////////////////////////////////////////

function GetImageURI(URL: String): String;
var
  Query: String;           // The full image URL we want to retrieve
  Client: TNetHTTPClient;  // The client connection
  Photo: TMemoryStream;    // The image coming back from TMDb
  Encoding: TBase64Encoding;
begin

  Query := 'https://image.tmdb.org/t/p/w185'+URL;

  Client := TNetHTTPClient.Create(nil);
  Client.ConnectionTimeout  := 60000;
  Client.ResponseTimeout    := 60000;
  Client.UserAgent          := 'Actorious';
  Client.SecureProtocols    := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

  Photo := TMemoryStream.Create;
  try
    Client.Get(Query, Photo);
    Photo.Seek(0, soFromBeginning);
    Encoding := TBase64Encoding.Create(0); // CharsPerLine -> 0 -> No line breaks
    if Pos('.jpg', LowerCase(Query)) > 0
    then Result := 'data:image/jpg;base64,'+Encoding.EncodeBytesToString(Photo.Memory, Photo.Size)
    else Result := 'data:image/png;base64,'+Encoding.EncodeBytesToString(Photo.Memory, Photo.Size);
    Encoding.Free;
  except on E: Exception do
    begin
      MainForm.LogException('GetImageURI', E.ClassName, E.Message, Query);
      Result := '';
    end;
  end;
  Photo.Free;
  Client.Free;

end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// GetDataFromWikiData                                                                           //
//                                                                                               //
// The process is largely the same for retrieving data from WikiData.  So lets use a function to //
// make this a little simpler in the other functions
///////////////////////////////////////////////////////////////////////////////////////////////////

function GetDataFromWikiData(Query:String; CacheFile: String):String;
var
  Client: TNetHTTPClient;  // The client connection
  Response: TStringList;   // The response from TMDB
  CacheAge: TDateTime;     // The age of an existing cache file
  Update: Boolean;         // To Update or Not

begin
  Response := TStringList.Create;
  Response.Text := '';

  // Determine whether we're updating;
  Update := False;
  if FileExists(CacheFile) then
  begin
    FileAge(CacheFile, CacheAge);
    if HoursBetween(Now, CacheAge) > 24 then
    begin
      Update := True;
    end
  end
  else
  begin
    Update := True;
  end;

  // Update only if necessary
  if Update then
  begin

    // Standard commection, more or less.
    Client := TNetHTTPClient.Create(nil);
    Client.ConnectionTimeout  := 60000;
    Client.ResponseTimeout    := 60000;
    Client.UserAgent          := 'Actorious';
    Client.Accept             := 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8';
    Client.SecureProtocols    := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    // Try and get the data
    try
      Response.Text := Client.Get(Query).ContentAsString(TEncoding.UTF8);
      Response.Text := FilterResponse(Response.Text);
      if Pos('SPARQL-QUERY', Response.Text) = 0
      then Response.SaveToFile(CacheFile, TEncoding.UTF8)
      else Response.Text := '';
      Client.Free;
    except on E: Exception do
      begin
        MainForm.LogException('GetDataFromWikiData', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;

  // If We didn't get a response (or didn't ask for one) try and load the data
  // from the cache instead.
  if (Response.Text = '') then
  begin
    try
      if FileExists(CacheFile)
      then Response.LoadFromFile(CacheFile, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.LogException('GetDataFromWikiData/Load', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;

  // If we still don't have data, well, I guess we don't have data
  if (Response.Text = '') then Response.Text := '{}';

  // Return the Response, Fresh or Cached or Otherwise
  Result := FilterResponse(Response.Text);
  Response.Free;

end;

///////////////////////////////////////////////////////////////////////////////////////////////////
// GetPersonFromTMDb                                                                             //
//                                                                                               //
// This simply contacts TMDb and gets as much information about a person as we can get in one    //
// query (their API counts requests in this way so we try to make as few as we possibly can).    //
// This contains a great deal of information about the person, but not as much detail about the  //
// roles so we'll have to augment that data later on.                                            //
///////////////////////////////////////////////////////////////////////////////////////////////////

function GetPersonfromTMDb(TMDb_ID: Integer; ForceUpdate: Boolean):String;
var
  Client: TNetHTTPClient;  // The client connection
  Query: String;           // The query we're building
  Response: TStringList;   // The response from TMDB
  CacheFile: String;       // The location where we're going to put it
  Update: Boolean;         // To Update or Not
  Reason: String;          // Why are we doing what we're doing

begin
  // Figure out where to put this
  TDirectory.CreateDirectory('cache/people/tmdb/'+RightStr('00000000'+IntToStr(TMDB_ID),3));
  CacheFile := 'cache/people/tmdb/'+RightStr('00000000'+IntToStr(TMDB_Id),3)+'/person-'+RightStr('00000000'+IntToStr(TMDb_ID),8)+'.json';
  Response := TStringList.Create;
  Response.Text := '';

  // Determine whether we're updating;
  Update := ForceUpdate;
  if not(Update) then
  begin
    if FileExists(CacheFile) then
    begin
      if TFile.GetLastWriteTime(CacheFile) < (Now - 5) then
      begin
        Update := True;
        Reason := 'Age';
      end
    end
    else
    begin
      Update := True;
      Reason := 'Miss';
      MainForm.LogEvent('- GetPerson Cache Miss: '+IntToStr(TMDb_ID));
    end;
  end
  else
  begin
    Reason := 'Force';
  end;


  // Update only if necessary
  if Update then
  begin

    // Standard commection, more or less.
    Client := TNetHTTPClient.Create(nil);
    Client.ConnectionTimeout  := 60000;
    Client.ResponseTimeout    := 60000;
    Client.UserAgent          := 'Actorious';
    Client.Accept             := 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8';
    Client.SecureProtocols    := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    // Get basically everything that we possibly can.  Note that the order is kinda important
    Query := 'https://api.themoviedb.org/3/person/'+IntToSTr(TMDb_ID);
    Query := Query+'?api_key='+MainForm.edTMDbAPI.Text;
    Query := Query+'&language=en-US';
    Query := Query+'&include_image_language=en,null';
    Query := Query+'&append_to_response=images,videos,external_ids,tagged_images,combined_credits';

    // Try and get the data
    try
      Response.Text := Client.Get(Query).ContentAsString(TEncoding.UTF8);
      Client.Free;
      Response.Text := FilterResponse(Response.Text);
      Response.SaveToFile(CacheFile, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.LogException('GetPersonFromTMDb', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end
  else
  begin
    Reason := 'Cache';
  end;

  // If We didn't get a response (or didn't ask for one) try and load the data
  // from the cache instead.
  if (Response.Text = '') then
  begin
    try
      if FileExists(CacheFile)
      then Response.LoadFromFile(CacheFile, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.LogException('GetPersonFromTMDb/Load', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;

  // If we still don't have data, well, I guess we don't have data
  if (Response.Text = '') then Response.Text := '{}';

  // Return the Response, Fresh or Cached or Otherwise
  Response.Text := FilterResponse(Response.Text);
  Result := Response.Text;
  Response.Free;

  // Update Cache Information
  inc(MainForm.PersonCacheRequests);
  if      Reason = 'Cache' then inc(MainForm.PersonCacheHit)
  else if Reason = 'Force' then inc(MainForm.PersonCacheForce)
  else if Reason = 'Age'   then inc(MainForm.PersonCacheAge)
  else if Reason = 'Miss'  then inc(MainForm.PersonCacheMiss);

end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// GetMovieFromTMDb                                                                              //
//                                                                                               //
// This contacts TMDb and gets as much information about a movie as we can get in one single     //
// query (their API counts requests in this way so we try to make as few as we possibly can).    //
// This contains a great deal of information about the movie, enough to fill in the rest of the  //
// pieces for the RoleTabulator as well as the list of actors when the row is selected.          //
///////////////////////////////////////////////////////////////////////////////////////////////////

function GetMoviefromTMDb(TMDb_ID: Integer; ForceUpdate: Boolean):String;
var
  Client: TNetHTTPClient;  // The client connection
  Query: String;           // The query we're building
  Response: TStringList;   // The response from TMDB
  CacheFile: String;       // The location where we're going to put it
  CacheAge: TDateTime;     // The age of an existing cache file
  Update: Boolean;         // To Update or Not
  Reason: String;          // Why are we doing what we're doing
  Success: Boolean;
  Attempt: Integer;

begin
  // Figure out where to put this
  TDirectory.CreateDirectory('cache/movies/tmdb/'+RightStr('00000000'+IntToStr(TMDB_ID),3));
  CacheFile := 'cache/movies/tmdb/'+RightStr('00000000'+IntToStr(TMDB_Id),3)+'/movie-'+RightStr('00000000'+IntToStr(TMDb_ID),8)+'.json';
  Response := TStringList.Create;
  Response.Text := '';

  // Determine whether we're updating;
  Update := ForceUpdate;
  if not(Update) then
  begin
    if FileExists(CacheFile) then
    begin
      FileAge(CacheFile, CacheAge);
      if HoursBetween(Now, CacheAge) > 168 then
      begin
        Update := True;
        Reason := 'Age';
      end
    end
    else
    begin
      Update := True;
      Reason := 'Miss';
    end;
  end
  else
  begin
    Reason := 'Force';
  end;

  // Update only if necessary
  if Update then
  begin
    // Standard commection, more or less.
    Client := TNetHTTPClient.Create(nil);
    Client.ConnectionTimeout := 90000; // 90 seconds
    Client.ResponseTimeout := 90000; // 90 seconds
    Client.UserAgent := 'Actorious';
    Client.Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8';
    Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    // Get basically everything that we possibly can
    Query := 'https://api.themoviedb.org/3/movie/'+IntToSTr(TMDb_ID);
    Query := Query+'?api_key='+MainForm.edTMDbAPI.Text;
    Query := Query+'&language=en-US';
    Query := Query+'&include_image_language=en,null';
    Query := Query+'&append_to_response=images,videos,external_ids,tagged_images,credits';

    // Try and get the data
    try
      Response.Text := Client.Get(Query).ContentAsString(TEncoding.UTF8);
      Client.Free;
    except on E: Exception do
      begin
        MainForm.LogException('GetMovieFromTMDb', E.ClassName, E.Message, CacheFile);
      end;
    end;

    Success := False;
    Attempt := 1;
    while ((Success = False) and (Attempt <= 3)) do
    begin
      try
        Response.Text := FilterResponse(Response.Text);
        Response.SaveToFile(CacheFile, TEncoding.UTF8);
        Success := True;
      except on E: Exception do
        begin
          MainForm.LogException('GetMovieFromTMDb', E.ClassName, E.Message, CacheFile);
          Attempt := Attempt + 1;
        end;
      end;
    end;
  end
  else
  begin
    Reason := 'Cache';
  end;

  // If We didn't get a response (or didn't ask for one) try and load the data
  // from the cache instead.
  if (Response.Text = '') then
  begin
    try
      if FileExists(CacheFile)
      then Response.LoadFromFile(CacheFile, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.LogException('GetMovieFromTMDb/Load', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;

  // If we still don't have data, well, I guess we don't have data
  if (Response.Text = '') then Response.Text := '{}';

  // Return the Response, Fresh or Cached or Otherwise
  Result := FilterResponse(Response.Text);
  Response.Free;

  // Update Cache Information
  inc(MainForm.MovieCacheRequests);
  if      Reason = 'Cache' then inc(MainForm.MovieCacheHit)
  else if Reason = 'Force' then inc(MainForm.MovieCacheForce)
  else if Reason = 'Age'   then inc(MainForm.MovieCacheAge)
  else if Reason = 'Miss'  then inc(MainForm.MovieCacheMiss);

end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// GetTVShowFromTMDb                                                                             //
//                                                                                               //
// This contacts TMDb and gets as much information about a TVShow as we can get in one single    //
// query (their API counts requests in this way so we try to make as few as we possibly can).    //
// This contains a great deal of information about the TVShow, enough to fill in the rest of the //
// pieces for the RoleTabulator as well as the list of actors when the row is selected.          //
///////////////////////////////////////////////////////////////////////////////////////////////////

function GetTVShowfromTMDb(TMDb_ID: Integer; ForceUpdate: Boolean):String;
var
  Client: TNetHTTPClient;  // The client connection
  Query: String;           // The query we're building
  Response: TStringList;   // The response from TMDB
  CacheFile: String;       // The location where we're going to put it         \
  CacheAge: TDateTime;     // The age of an existing cache file
  Update: Boolean;         // To Update or Not
  Reason: String;          // Why are we doing what we're doing

begin
  // Figure out where to put this
  TDirectory.CreateDirectory('cache/TVShows/tmdb/'+RightStr('00000000'+IntToStr(TMDB_ID),3));
  CacheFile := 'cache/tvshows/tmdb/'+RightStr('00000000'+IntToStr(TMDB_Id),3)+'/tvshow-'+RightStr('00000000'+IntToStr(TMDb_ID),8)+'.json';
  Response := TStringList.Create;

  // Determine whether we're updating;
  Update := ForceUpdate;
  if not(Update) then
  begin
    if FileExists(CacheFile) then
    begin
      FileAge(CacheFile, CacheAge);
      if HoursBetween(Now, CacheAge) > 168 then
      begin
        Update := True;
        Reason := 'Age';
      end
    end
    else
    begin
      Update := True;
      Reason := 'Miss';
    end;
  end
  else
  begin
    Reason := 'Force';
  end;


  // Update only if necessary
  if Update then
  begin

    // Standard commection, more or less.
    Client := TNetHTTPClient.Create(nil);
    Client.ConnectionTimeout := 90000; // 90 seconds
    Client.ResponseTimeout := 90000; // 90 seconds
    Client.UserAgent := 'Actorious';
    Client.Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8';
    Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    // Get basically everything that we possibly can
    Query := 'https://api.themoviedb.org/3/tv/'+IntToSTr(TMDb_ID);
    Query := Query+'?api_key='+MainForm.edTMDbAPI.Text;
    Query := Query+'&language=en-US';
    Query := Query+'&include_image_language=en,null';
    Query := Query+'&append_to_response=images,videos,external_ids,tagged_images,aggregate_credits';

    // Try and get the data
    try
      Response.Text := Client.Get(Query).ContentAsString(TEncoding.UTF8);
      Response.Text := FilterResponse(Response.Text);
      Response.SaveToFile(CacheFile, TEncoding.UTF8);
      Client.Free;
    except on E: Exception do
      begin
        MainForm.LogException('GetTVShowFromTMDb', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end
  else
  begin
    Reason := 'Cache';
  end;

  // If We didn't get a response (or didn't ask for one) try and load the data
  // from the cache instead.
  if (Response.Text = '') then
  begin
    try
      if FIleExists(CacheFile)
      then Response.LoadFromFile(CacheFile, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.LogException('GetTVShowFromTMDb/Load', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;

  // If we still don't have data, well, I guess we don't have data
  if (Response.Text = '') then Response.Text := '{}';

  // Return the Response, Fresh or Cached or Otherwise
  Response.Text := FilterResponse(Response.Text);
  Result := Response.Text;
  Response.Free;

  // Update Cache Information
  inc(MainForm.TVShowCacheRequests);
  if      Reason = 'Cache' then inc(MainForm.TVShowCacheHit)
  else if Reason = 'Force' then inc(MainForm.TVShowCacheForce)
  else if Reason = 'Age'   then inc(MainForm.TVShowCacheAge)
  else if Reason = 'Miss'  then inc(MainForm.TVShowCacheMiss);

end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// SaveActoriousPersonData                                                                       //
//                                                                                               //
// Saves the Actorious version of the data to disk along with the Brotli-compressed version.     //
// These are then used subsequently when building responses to search queries or other requests. //
///////////////////////////////////////////////////////////////////////////////////////////////////

procedure SaveActoriousPersonData(Person: String; PersonID: Integer; AdultActor:Boolean);
var
  CacheFile: String;
  PersonData: TStringList;
  ResponseFile: TMemoryStream;
  Brotli: TMemoryStream;
  AdultList: TJSONObject;
  Success: Boolean;
  Attempt: Integer;

begin

  // Figure out where to put this
  TDirectory.CreateDirectory('cache/people/actorious/'+RightStr('00000000'+IntToStr(PersonID),3));
  CacheFile := 'cache/people/actorious/'+RightStr('00000000'+IntToStr(PersonID),3)+'/person-'+RightStr('00000000'+IntToStr(PersonID),8);

  Success := False;
  Attempt := 1;
  while ((Success = False) and (Attempt <= 3)) do
  begin
    // Try and Save the data
    try
      // Save the response to disk as-is
      PersonData := TStringList.Create;
      PersonData.Text := Person;
      PersonData.SaveToFile(CacheFile+'.json', TEncoding.UTF8);

      // Load binary file from disk into stream
      ResponseFile := TMemoryStream.Create;
      ResponseFile.LoadFromFile(CacheFile+'.json');
      ResponseFile.Seek(0, soFromBeginning);

      // Compress the stream with Brotli
      Brotli := TMemoryStream.Create;
      BrotliCompressStream(ResponseFile, Brotli, bcMax);
      Brotli.Seek(0, soFromBeginning);

      // Save the Brotli-compressed response to disk
      Brotli.SaveToFile(CacheFile+'.json.br');

      // We were never here
      Brotli.Free;
      ResponseFile.Free;
      PersonData.Free;

      Success := True;

    except on E: Exception do
      begin
        MainForm.LogException('SaveActoriousPersonData', E.ClassName, E.Message, 'Attempt #'+IntToStr(Attempt)+'/3: '+CacheFile);
        Attempt := Attempt + 1;
        sleep(5000);
      end;
    end;
  end;

  // Here we're just creating a list of Adult Actors as we have no other way
  // of generating such a list for use with things like the Top1000 queries.
  // This list should be regenerated from time to time.
  if AdultActor then
  begin
    try
      // Get the existing list
      PersonData := TStringList.Create;
      AdultList := TJSONObject.Create;
      try
        if FileExists('cache/people/top1000/top1000-0.json')
        then PersonData.LoadFromFile('cache/people/top1000/top1000-0.json', TEncoding.UTF8);

        // Create an empty list if one doesn't exist already
        if (PersonData.Text = '')
        then PersonData.Text := '{"page":0,"results":[]}';

        AdultList := TJSONObject.ParseJSONValue(PersonData.Text) as TJSONObject;
      except on E: Exception do
        begin
          MainForm.LogException('SaveActoriousPersonData/LoadAdult', E.ClassName, E.Message, CacheFile);
        end;
      end;

      // Add the current person to the JSON Array
      ((AdultList as TJSONObject).getValue('results') as TJSONArray).AddElement(TJSONObject.Create(TJSONPair.Create('id',TJSONNumber.create(PersonID))));

      // Save the updated list
      PersonData.Text := AdultList.ToString;
      PersonData.SaveToFile('cache/people/top1000/top1000-0.json', TEncoding.UTF8);
      PersonData.SaveToFile('cache/people/top5000/top5000-0.json', TEncoding.UTF8);

      PersonData.Free;
      AdultList.Free;
    except on E: Exception do
      begin
        MainForm.LogException('SaveActoriousPersonData/SaveAdult', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;
end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// SaveActoriousMovieData                                                                        //
//                                                                                               //
// Saves the Actorious version of the data to disk along with the Brotli-compressed version.     //
// These are then used subsequently when building responses to search queries or other requests. //
///////////////////////////////////////////////////////////////////////////////////////////////////

procedure SaveActoriousMovieData(Movie: String; MovieID: Integer; AdultMovie: Boolean);
var
  CacheFile: String;
  MovieData: TStringList;
  ResponseFile: TMemoryStream;
  Brotli: TMemoryStream;
  AdultList: TJSONObject;
  Success: Boolean;
  Attempt: Integer;

begin

  // Figure out where to put this
  TDirectory.CreateDirectory('cache/movies/actorious/'+RightStr('00000000'+IntToStr(MovieID),3));
  CacheFile := 'cache/movies/actorious/'+RightStr('00000000'+IntToStr(MovieID),3)+'/movie-'+RightStr('00000000'+IntToStr(MovieID),8);

  Success := False;
  Attempt := 1;
  while ((Success = False) and (Attempt <= 3)) do
  begin
    // Try and Save the data
    try
      MovieData := TStringList.Create;
      MovieData.Text := Movie;
      MovieData.SaveToFile(CacheFile+'.json', TEncoding.UTF8);

      // Save the response to disk as-is
      ResponseFile := TMemoryStream.Create;
      ResponseFile.LoadFromFile(CacheFile+'.json');
      ResponseFile.Seek(0, soFromBeginning);

      // Compress the stream with Brotli
      Brotli := TMemoryStream.Create;
      BrotliCompressStream(ResponseFile, Brotli, bcMax);
      Brotli.Seek(0, soFromBeginning);

      // Save the Brotli-compressed response to disk
      Brotli.SaveToFile(CacheFile+'.json.br');

      // We were never here
      Brotli.Free;
      ResponseFile.Free;
      MovieData.Free;

      Success := True;

    except on E: Exception do
      begin
        MainForm.LogException('SaveActoriousPersonData', E.ClassName, E.Message, 'Attempt #'+IntToStr(Attempt)+'/3: '+CacheFile);
        Attempt := Attempt + 1;
        sleep(5000);
      end;
    end;
  end;

  // Here we're just creating a list of Adult Movies as we have no other way
  // of generating such a list for use with things like the Top1000 queries.
  // This list should be regenerated from time to time.
  if AdultMovie then
  begin
    try
      // Get the existing list
      MovieData := TStringList.Create;
      AdultList := TJSONObject.Create;
      try
        MovieData := TStringList.Create;
        MovieData.Text := '';

        if FileExists('cache/movies/top1000/top1000-0.json')
        then MovieData.LoadFromFile('cache/movies/top1000/top1000-0.json', TEncoding.UTF8);

        // Create an empty list if one doesn't exist already
        if (MovieData.Text = '')
        then MovieData.Text := '{"page":0,"results":[]}';

        AdultList := TJSONObject.ParseJSONValue(MovieData.Text) as TJSONObject;

      except on E: Exception do
        begin
          MainForm.LogException('SaveActoriousMovieData/LoadAdult', E.ClassName, E.Message, CacheFile);
        end;
      end;

      // Add the current Movie to the JSON Array
      ((AdultList as TJSONObject).getValue('results') as TJSONArray).AddElement(TJSONObject.Create(TJSONPair.Create('id',TJSONNumber.create(MovieID))));

      // Save the updated list
      MovieData.Text := AdultList.ToString;
      MovieData.SaveToFile('cache/movies/top1000/top1000-0.json', TEncoding.UTF8);
      MovieData.SaveToFile('cache/movies/top5000/top5000-0.json', TEncoding.UTF8);

      MovieData.Free;
      AdultList.Free;
    except on E: Exception do
      begin
        MainForm.LogException('SaveActoriousMovieData/SaveAdult', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;
end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// SaveActoriousTVShowData                                                                       //
//                                                                                               //
// Saves the Actorious version of the data to disk along with the Brotli-compressed version.     //
// These are then used subsequently when building responses to search queries or other requests. //
///////////////////////////////////////////////////////////////////////////////////////////////////

procedure SaveActoriousTVShowData(TVShow: String; TVShowID: Integer; AdultTVShow: Boolean);
var
  CacheFile: String;
  TVShowData: TStringList;
  ResponseFile: TMemoryStream;
  Brotli: TMemoryStream;
  AdultList: TJSONObject;
  Success: Boolean;
  Attempt: Integer;

begin

  // Figure out where to put this
  TDirectory.CreateDirectory('cache/tvshows/actorious/'+RightStr('00000000'+IntToStr(TVShowID),3));
  CacheFile := 'cache/tvshows/actorious/'+RightStr('00000000'+IntToStr(TVShowID),3)+'/tvshows-'+RightStr('00000000'+IntToStr(TVShowID),8);

  Success := False;
  Attempt := 1;
  while ((Success = False) and (Attempt <= 3)) do
  begin
    // Try and Save the data
    try
      TVShowData := TStringList.Create;
      TVShowData.Text := TVShow;
      TVShowData.SaveToFile(CacheFile+'.json', TEncoding.UTF8);

      // Save the response to disk as-is
      ResponseFile := TMemoryStream.Create;
      ResponseFile.LoadFromFile(CacheFile+'.json');
      ResponseFile.Seek(0, soFromBeginning);

      // Compress the stream with Brotli
      Brotli := TMemoryStream.Create;
      BrotliCompressStream(ResponseFile, Brotli, bcMax);
      Brotli.Seek(0, soFromBeginning);

      // Save the Brotli-compressed response to disk
      Brotli.SaveToFile(CacheFile+'.json.br');

      // We were never here
      Brotli.Free;
      ResponseFile.Free;
      TVShowData.Free;

    except on E: Exception do
      begin
        MainForm.LogException('SaveActoriousPersonData', E.ClassName, E.Message, 'Attempt #'+IntToStr(Attempt)+'/3: '+CacheFile);
        Attempt := Attempt + 1;
        sleep(5000);
      end;
    end;
  end;

  // Here we're just creating a list of Adult TVShows as we have no other way
  // of generating such a list for use with things like the Top1000 queries.
  // This list should be regenerated from time to time.
  if AdultTVShow then
  begin
    try
      // Get the existing list
      TVShowData := TStringList.Create;
      AdultList := TJSONObject.Create;
      try
        TVShowData := TStringList.Create;
        TVShowData.Text := '';

        if FileExists('cache/tvshows/top1000/top1000-0.json')
        then TVShowData.LoadFromFile('cache/tvshows/top1000/top1000-0.json', TEncoding.UTF8);

        // Create an empty list if one doesn't exist already
        if (TVShowData.Text = '')
        then TVShowData.Text := '{"page":0,"results":[]}';

        AdultList := TJSONObject.ParseJSONValue(TVShowData.Text) as TJSONObject;

      except on E: Exception do
        begin
          MainForm.LogException('SaveActoriousTVShowData/LoadAdult', E.ClassName, E.Message, CacheFile);
        end;
      end;

      // Add the current TVShow to the JSON Array
      ((AdultList as TJSONObject).getValue('results') as TJSONArray).AddElement(TJSONObject.Create(TJSONPair.Create('id',TJSONNumber.create(TVShowID))));

      // Save the updated list
      TVShowData.Text := AdultList.ToString;
      TVShowData.SaveToFile('cache/tvshows/top1000/top1000-0.json', TEncoding.UTF8);
      TVShowData.SaveToFile('cache/tvshows/top5000/top5000-0.json', TEncoding.UTF8);

      TVShowData.Free;
      AdultList.Free;
    except on E: Exception do
      begin
        MainForm.LogException('SaveActoriousTVShowData/SaveAdult', E.ClassName, E.Message, CacheFile);
      end;
    end;
  end;
end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// ProcessActor                                                                                  //
//                                                                                               //
// This handles the messy business of building the Actorious JSON version of an Actor profile    //
// out of the combination of the Wikidata results and the TMDb results.  The idea is to have one //
// complete JSON object that contains the actor information appearing in a single row of the     //
// left table of the Actorious UI, combined with the entire contents of the right table. The     //
// idea is that the user can browse the table largely without needing to do another query based  //
// on actor selections.  We'll need to go back for data if a different role is selected though.  //
///////////////////////////////////////////////////////////////////////////////////////////////////

function ProcessActor(ActorID: Integer; ActorRef: String; TMDB_Data_String: String; WikiIndex: Integer; WikiData_String: String; ProgressPrefix: String; ProgressKey: Integer; ForceUpdate: Boolean):String;
var
  Actor: String;  // This is the result set we're building up

  ActorScore: Double;    // This is the popularity value we calculate for ourselves
  PopScore: Double;      // Popularity from TMDb
  BasicScore: Double;    // Tracks whether basic values are available
  SocialMedia: Integer;  // Keep track of how many social media accounts are available
  MovieBonus: Double;    // Extra points based on role prominence and earnings
  MovieRecent: Double;   // Extra points if work is recent
  TVBonus: Double;       // Extra points based on role prominence and appearances
  TVRecent: Double;      // Extra points if work is recent
  AwardsScore: Double;   // Extra points for winning awards

  CTZ: String;   // Citezenship value used to determine what flag to show
  BPL: String;   // Birthplace

  ActorHeight: Double;  // Calculating ActorHeight is a pain which is why we're
  Height1: String;      // doing it here in the server so we don't have to
  Height2: String;      // spend so much effort doing it in the client
  RealFeet: Double;     // Problem is that the units are not necessarily known
  Feet: Double;         // and data quality is sketchy at best.  But it works.
  Inches: Double;       // Just takes a bit of fiddling

  AdultActor: Boolean;  // Whether this is an Adult Actor

  ActorRoles: Integer;  // Count of how many qualified Roles were found
  MovieRoles: Integer;  // Count of how many qualified Movie roles were found
  TVRoles:    Integer;  // Count of how many qualified TV roles were found

  RoleIndex: Integer;         // Used to iterate through roles
  Role: TJSONObject;          // One of the roles as JSON
  Images: TJSONArray;         // Images available for the person
  ImageIndex: Integer;        // Used to index images
  TopMovieRoles: TStringList; // Used to sort the TV roles
  TopTVRoles: TStringList;    // Used to sort the Movie roles

  TopLimit: Integer;  // How many "top" items to include
  TopCount: Integer;  // Keeping count of how many we have
  DeDupe: Integer;    // Used to ensure we're not adding duplicate entries

  ShowData: TJSONObject;  // The (hopefully cached) Movie or TVShow Data

  TestResult: String;

  i: integer;
  RoleName: String;
  ThisRoleName: String;
  TMDB_Data: TJSONObject;
  WikiData: TJSONArray;
begin
  // Here we are building up a new JSON object to store everything to do with one actor.

  PopScore := 0;
  BasicScore := 0;
  SocialMedia := 0;
  MovieBonus := 0;
  MovieRecent := 0;
  TVBonus := 0;
  TVRecent := 0;
  AwardsScore := 0;

  BPL := '';
  CTZ := '';

  Height1 := '';
  Height2 := '';

  AdultActor := False;

  ActorRoles := 0;
  MovieRoles := 0;
  TVRoles := 0;

  TMDB_Data := TJSONObject.Create;
  try
    TMDB_Data := TJSONObject.ParseJSONValue(TMDB_Data_String) as TJSONObject;
  except on E: Exception do
    begin
      MainForm.LogEvent('TMDB JSON Error: ');
      MainForm.LogEvent(Copy(TMDB_Data_String,1,150));
    end;
  end;

  WikiData := TJSONArray.Create;
  try
    WikiData := TJSONObject.ParseJSONValue(WikiData_String) as TJSONArray;
  except on E: Exception do
    begin
      MainForm.LogEvent('WIKI JSON Error: ');
      MainForm.LogEvent(Copy(WikiData_String,1,150));
    end;
  end;

  // This is sort of the 'header' infromation
  try


    // Entries from Calling Function ///////////////////////////////////////////


    // ID is used by Tabulator as an index, so handy to just add it right here.
    Actor :='{"ID":'+IntToStr(ActorID)+',';

    // Add a timestamp so that we know when this was generated
    Actor := Actor+'"DAT":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'",';

    // Name - Assume it is always present?
    Actor := Actor+'"NAM":'+REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('name') as TJSONString)+',';

    // TMDb ID
    Actor := Actor+'"TID":'+IntToStr(StrToInt(ActorRef))+',';
    SocialMedia := SocialMedia + 1;


    // Entries from WikiData Query /////////////////////////////////////////////


    // WikiData ID
    if (WikiIndex <> -1) then
    begin
      Actor := Actor+'"WID":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('person') as TJSONObject).GetValue('value') as TJSONString).Value+'",';
      SocialMedia := SocialMedia + 1;
    end;

    // Models ID
    if ( WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('ModelID') <> nil) then
    begin
     if Trim((((WikiData.Items[WikiIndex] as TJSONObject).getValue('ModelID') as TJSONObject).GetValue('value') as TJSONString).Value) <> '' then
     begin
        Actor := Actor +'"MID":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('ModelID') as TJSONObject).GetValue('value') as TJSONString).Value+'",';
        SocialMedia := SocialMedia + 1;
     end;
    end;

    // Rotten Tomatoes ID
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('RTID') <> nil) then
    begin
      if Trim((((WikiData.Items[WikiIndex] as TJSONObject).getValue('RTID') as TJSONObject).GetValue('value') as TJSONString).Value) <> '' then
     begin
        Actor := Actor+'"RID":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('RTID') as TJSONObject).GetValue('value') as TJSONString).Value+'",';
        SocialMedia := SocialMedia + 1;
     end;
    end;

    // MetaCritic ID
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('MetaCriticID') <> nil) then
    begin
      if Trim((((WikiData.Items[WikiIndex] as TJSONObject).getValue('MetaCriticID') as TJSONObject).GetValue('value') as TJSONString).Value) <> '' then
      begin
        Actor := Actor+'"MET":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('MetaCriticID') as TJSONObject).GetValue('value') as TJSONString).Value+'",';
        SocialMedia := SocialMedia + 1;
      end;
    end;

    // Wikipedia Link
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('Wikipedia') <> nil) then
    begin
      if Trim((((WikiData.Items[WikiIndex] as TJSONObject).getValue('Wikipedia') as TJSONObject).GetValue('value') as TJSONString).Value) <> '' then
      begin
        Actor := Actor+'"WIK":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('Wikipedia') as TJSONObject).GetValue('value') as TJSONString).Value+'",';
        SocialMedia := SocialMedia + 1;
      end;
    end;

    // Height
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('Height') <> nil) then
    begin
      ActorHeight := StrToFloatDef(Trim((((WikiData.Items[WikiIndex] as TJSONObject).getValue('Height') as TJSONObject).GetValue('value') as TJSONString).Value),0.0);
      if (ActorHeight > 0.0) then
      begin

        BasicScore := BasicScore + 10;

        if (ActorHeight < 2.2) then  // meters
        begin
          Height1 := FloatToStrF(ActorHeight,ffNumber,4,2)+'m';
          RealFeet := ((ActorHeight*39.3700) / 12);
          Feet := Floor(RealFeet);
          Inches := Round((RealFeet - Feet) * 12);
          if (Inches = 12) then
          begin
            Inches := 0;
            Feet := Feet + 1;
          end;
          Height2 := IntToStr(Trunc(Feet))+'&apos;'+INtToStr(Trunc(Inches))+'&quot;';
        end

        else if (ActorHeight > 100) then // centimeters
        begin
          ActorHeight := ActorHeight / 100;
          Height1 := FloatToStrF(ActorHeight,ffNumber,4,2)+'m';
          RealFeet := ((ActorHeight*39.3700) / 12);
          Feet := Floor(RealFeet);
          Inches := Round((RealFeet - Feet) * 12);
          if (Inches = 12) then
          begin
            Inches := 0;
            Feet := Feet + 1;
          end;
          Height2 := IntToStr(Trunc(Feet))+'&apos;'+INtToStr(Trunc(Inches))+'&quot;';
        end

        else if (ActorHeight > 10) then // inches
        begin
          Feet := Floor(ActorHeight / 12);
          Inches := ActorHeight - (Feet * 12);
          if (Inches = 12) then
          begin
            Inches := 0;
            Feet := Feet + 1;
          end;
          Height1 := FloatToStrF(((Feet*2.54*12)+(Inches*2.54))/100,ffNumber,4,2)+'m';
          Height2 := IntToStr(Trunc(Feet))+'&apos;'+INtToStr(Trunc(Inches))+'&quot;';
        end

        else // feet.inches
        begin
          Feet := Floor(ActorHeight);
          Inches := (Actorheight - Feet)*100;
          if (Inches = 12) then
          begin
            Inches := 0;
            Feet := Feet + 1;
          end
          else if (inches > 12) then
          begin
            Inches := inches / 10;
          end;
          Height1 := FloatToStrF(((Feet*2.54*12)+(Inches*2.54))/100,ffNumber,4,2)+'m';
          Height2 := IntToStr(Trunc(Feet))+'&apos;'+INtToStr(Trunc(Inches))+'&quot;';
        end;

        Actor := Actor+'"HT1":"'+Height1+'","HT2":"'+Height2+'",';
      end;
    end;

    // Birthday - We got the birthday from Wikipedia, but not always set (or even the same?!) in TMDb
    if (WikiIndex <> -1) then
    begin
      Actor := Actor+'"DOB":"'+Copy((((WikiData.Items[WikiIndex] as TJSONObject).getValue('DOB') as TJSONObject).GetValue('value') as TJSONString).Value,1,10)+'",';
      BasicScore := BasicScore + 10;
    end;

    // Deathday - We got the deathday from Wikipedia, but not always set (or even the same?!) in TMDb
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('DOD') <> nil) then
    begin
      Actor := Actor+'"DOD":"'+Copy((((WikiData.Items[WikiIndex] as TJSONObject).getValue('DOD') as TJSONObject).GetValue('value') as TJSONString).Value,1,10)+'",';
      BasicScore := BasicScore + 10;
    end;

    // Birthplace
    if not(TMDB_Data.getValue('place_of_birth') = nil) and not((TMDB_Data.getValue('place_of_birth') is TJSONNULL))
    then BPL := REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('place_of_birth') as TJSONString);
    if (BPL <> '') and (BPL <> '""') then
    begin
      Actor := Actor+'"BPL":'+BPL+',';
      BasicScore := BasicScore + 10;
    end;

    // Citizenship
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('CountryCode') <> nil) then
    begin
      CTZ := Trim((((WikiData.Items[WikiIndex] as TJSONObject).getValue('CountryCode') as TJSONObject).GetValue('value') as TJSONString).Value);
      BasicScore := BasicScore + 10;
    end;
    // Don't have a citizenship specified from WikiData so let's try and use the BPL to set one
    if (CTZ = 'YU')  then CTZ := 'MK';
    if (CTZ = '') then
    begin
      if Pos('USA'          ,BPL) > 0 then CTZ := 'US';
      if Pos('Canada'       ,BPL) > 0 then CTZ := 'CA';
      if Pos('Australia'    ,BPL) > 0 then CTZ := 'AU';
      if Pos('France'       ,BPL) > 0 then CTZ := 'FR';
      if Pos('Germany'      ,BPL) > 0 then CTZ := 'DE';
      if Pos('Spain'        ,BPL) > 0 then CTZ := 'ES';
      if Pos('Italy'        ,BPL) > 0 then CTZ := 'IT';
      if Pos('China'        ,BPL) > 0 then CTZ := 'CN';
      if Pos('Hong Kong'    ,BPL) > 0 then CTZ := 'CN';
      if Pos('UK'           ,BPL) > 0 then CTZ := 'GB';
      if Pos('Denmark'      ,BPL) > 0 then CTZ := 'DK';
      if Pos('Israel'       ,BPL) > 0 then CTZ := 'IL';
      if Pos('Norway'       ,BPL) > 0 then CTZ := 'NO';
      if Pos('Taiwan'       ,BPL) > 0 then CTZ := 'TW';
      if Pos('Cuba'         ,BPL) > 0 then CTZ := 'CU';
      if Pos('Ireland'      ,BPL) > 0 then CTZ := 'IE';
      if Pos('South Korea'  ,BPL) > 0 then CTZ := 'KR';
      if Pos('Thailand'     ,BPL) > 0 then CTZ := 'TH';
      if Pos('Japan'        ,BPL) > 0 then CTZ := 'JP';
      if Pos('Ukraine'      ,BPL) > 0 then CTZ := 'UA';
      if Pos('Turkey'       ,BPL) > 0 then CTZ := 'TR';
      if Pos('rkiye'        ,BPL) > 0 then CTZ := 'TR';
      if Pos('Sweden'       ,BPL) > 0 then CTZ := 'SE';
      if Pos('Argentina'    ,BPL) > 0 then CTZ := 'AR';
      if Pos('New Zealand'  ,BPL) > 0 then CTZ := 'NZ';
      if Pos('Guatemala'    ,BPL) > 0 then CTZ := 'GT';
      if Pos('Malaysia'     ,BPL) > 0 then CTZ := 'MY';
      if Pos('Belgium'      ,BPL) > 0 then CTZ := 'BE';
      if Pos('Chile'        ,BPL) > 0 then CTZ := 'CL';
      if Pos('Philippines'  ,BPL) > 0 then CTZ := 'PH';
      if Pos('South Africa' ,BPL) > 0 then CTZ := 'ZA';
      if Pos('Pakistan'     ,BPL) > 0 then CTZ := 'PK';
      if Pos('Brazil'       ,BPL) > 0 then CTZ := 'BR';
      if Pos('Bulgaria'     ,BPL) > 0 then CTZ := 'BG';
      if Pos('Romania'      ,BPL) > 0 then CTZ := 'RO';
      if Pos('Mexico'       ,BPL) > 0 then CTZ := 'MX';
      if Pos('Bermuda'      ,BPL) > 0 then CTZ := 'BM';
      if Pos('Costa Rica'   ,BPL) > 0 then CTZ := 'CR';
      if Pos('Puerto Rico'  ,BPL) > 0 then CTZ := 'PR';
      if Pos('Lebanon'      ,BPL) > 0 then CTZ := 'LB';
      if Pos('Netherlands'  ,BPL) > 0 then CTZ := 'NL';
      if Pos('India'        ,BPL) > 0 then CTZ := 'IN';
      if Pos('Jamaica'      ,BPL) > 0 then CTZ := 'JM';
      if Pos('Honduras'     ,BPL) > 0 then CTZ := 'HN';
      if Pos('Finland'      ,BPL) > 0 then CTZ := 'FI';
      if Pos('Morocco'      ,BPL) > 0 then CTZ := 'MA';
      if Pos('Iran'         ,BPL) > 0 then CTZ := 'IR';
      if Pos('Colombia'     ,BPL) > 0 then CTZ := 'CO';
      if Pos('Poland'       ,BPL) > 0 then CTZ := 'FI';
      if Pos('Nepal'        ,BPL) > 0 then CTZ := 'NP';
      if Pos('Russia'       ,BPL) > 0 then CTZ := 'RU';
      if Pos('USSR'         ,BPL) > 0 then CTZ := 'RU';
      if Pos('U.S.S.R.'     ,BPL) > 0 then CTZ := 'RU';
      if Pos('Czech'        ,BPL) > 0 then CTZ := 'CZ';
      if Pos('Ghana'        ,BPL) > 0 then CTZ := 'GH';
      if Pos('Austria'      ,BPL) > 0 then CTZ := 'AT';
      if Pos('Ethiopia'     ,BPL) > 0 then CTZ := 'ET';
      if Pos('Slovak'       ,BPL) > 0 then CTZ := 'SK';
      if Pos('Yugoslavia'   ,BPL) > 0 then CTZ := 'MK';
      if Pos('Syria'        ,BPL) > 0 then CTZ := 'SY';
      if Pos('Singapore'    ,BPL) > 0 then CTZ := 'SG';
      if Pos('Moldova'      ,BPL) > 0 then CTZ := 'MD';
      if Pos('Portugal'     ,BPL) > 0 then CTZ := 'PT';
    end;
    // Take whatever we've got and add it to the record
    if CTZ <> ''
    then Actor := Actor+'"CTZ":"'+CTZ+'",';


    // Entries from TMDb ///////////////////////////////////////////////////////


    // Family Friendly... Or not?
    if not(TMDB_Data.getValue('adult') = nil) and not((TMDB_Data.getValue('adult') is TJSONNULL)) then
    begin
      if (TMDB_Data.getValue('adult') as TJSONBool).AsBoolean = True then
      begin
        AdultActor := True;
      end;
    end;
    if AdultActor
    then Actor := Actor+'"XXX":true,'
    else Actor := Actor+'"XXX":false,';


    if not(TMDB_Data.getValue('external_ids') = nil) then
    begin

      // IMDb ID
      if not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('imdb_id') = nil) and not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('imdb_id') is TJSONNULL) then
      begin
        if (REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('imdb_id') as TJSONString)) <> '""' then
        begin
          Actor := Actor+'"IID":'+REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('imdb_id') as TJSONString)+',';
          SocialMedia := SocialMedia + 1;
        end;
      end;

      // Facebook ID
      if not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('facebook_id') = nil) and not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('facebook_id') is TJSONNULL) then
      begin
        if (REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('facebook_id') as TJSONString)) <> '""' then
        begin
          Actor := Actor+'"FID":'+REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('facebook_id') as TJSONString)+',';
          SocialMedia := SocialMedia + 1;
        end;
      end;

      // Twitter ID
      if not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('twitter_id') = nil) and not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('twitter_id') is TJSONNULL) then
      begin
        if (REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('twitter_id') as TJSONString)) <> '""' then
        begin
          Actor := Actor+'"TWT":'+REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('twitter_id') as TJSONString)+',';
          SocialMedia := SocialMedia + 1;
        end;
      end;

      // Instagram ID
      if not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('instagram_id') = nil) and not((TMDB_Data.getValue('external_ids') as TJSONObject).getValue('instagram_id') is TJSONNULL) then
      begin
        if (REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('instagram_id') as TJSONString)) <> '""' then
        begin
          Actor := Actor+'"INS":'+REST.JSON.TJSON.JSONEncode((TMDB_Data.getValue('external_ids') as TJSONOBJECT).getValue('instagram_id') as TJSONString)+',';
          SocialMedia := SocialMedia + 1;
        end;
      end;

    end; // external_ids

    // HomePage
    if not(TMDB_Data.getValue('homepage') = nil) and not((TMDB_Data.getValue('homepage') is TJSONNULL)) then
    begin
      if REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('homepage') as TJSONString) <> '""' then
      begin
        Actor := Actor+'"WWW":'+REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('homepage') as TJSONString)+',';
        SocialMedia := SocialMedia + 1;
      end;
    end;

    // Popularity - a TMDb scoring metric
    if not(TMDB_Data.getValue('popularity') = nil) and not((TMDB_Data.getValue('popularity') is TJSONNULL)) then
    begin
      PopScore := (TMDB_Data.getValue('popularity') as TJSONNumber).AsDouble;
      Actor := Actor+'"POP":'+FloatToStr(PopScore)+','
    end
    else
    begin
      PopScore := 0.0;
      Actor := Actor+'"POP":0.0,';
    end;

    // Biography
    if not(TMDB_Data.getValue('biography') = nil) and not((TMDB_Data.getValue('biography') is TJSONNULL)) then
    begin
      if REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('biography') as TJSONString) <> '""' then
      begin
        Actor := Actor+'"BIO":'+REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('biography') as TJSONString)+',';
        BasicScore := BasicScore + 10 + min(15,15.0*(Length(REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('biography') as TJSONString))/1000.0));
      end;
    end;

    // Path to get photo - We want null values in this case, so we can display missing image placeholders
    if not(TMDB_Data.getValue('profile_path') = nil) then
    begin
      if ((TMDB_Data.getValue('profile_path') is TJSONNULL))
      then Actor := Actor+'"PIC":null,'
      else
      begin
        Actor := Actor+'"PIC":'+REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('profile_path') as TJSONString)+',';
        BasicScore := BasicScore + 10;
      end;
    end
    else Actor := Actor+'"PIC":null,';

    // Social Media Count
    Actor := Actor+'"SOC":'+IntToStr(SocialMedia)+',';

  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/Header', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
    end;
  end;

  // Here we're ading in information about the profile images avaiable
  try
    if not(TMDB_Data.getValue('images') = nil) then
    begin
      if not((TMDB_Data.getValue('images') as TJSONObject).getValue('profiles') = nil)  then
      begin
        // we've got images?
        Images := (TMDB_Data.getValue('images') as TJSONObject).getValue('profiles') as TJSONArray;
        // Add Image count
        Actor := Actor+'"IMC":'+IntToStr(Images.Count)+',"IMG":[';
        BasicScore := BasicScore + min(Images.Count,15);
        for ImageIndex := 0 to Images.Count-1 do
        begin
          if ImageIndex = 0
          then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"'
          else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"';
        end;
        Actor := Actor+'],';
      end;
    end;
  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/Photos', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
    end;
  end;


  // Top Roles /////////////////////////////////////////////////////////////////


  TopMovieRoles := TStringList.Create;
  TopTVRoles := TStringList.Create;

  try
    // This is a bit of a mess as the popularity figure is completely different for TV vs. Movies.
    // So we separate them out and sort them to get the top five of each.
    if (TMDB_Data.getValue('combined_credits') <> nil) and ((TMDB_Data.getValue('combined_credits') as TJSONObject).getValue('cast') <> nil) then
    begin

      // Lets iterate through every Role for this Actor where they are a member of the cast, if any
      for RoleIndex := 0 to ((TMDB_Data.getValue('combined_credits') as TJSONObject).getValue('cast') as TJSONArray).Count - 1 do
      begin

        Role := (((TMDB_Data.getValue('combined_credits') as TJSONObject).getValue('cast') as TJSONArray).Items[RoleIndex] as TJSONObject);

        // Needs to have a popularity value.  Lots of stuff there that is not really Movie or TV roles that need to be skipped
        if (Role.getValue('popularity') <> nil) then
        begin

          // Needs to stipulate whether it is a Movie or TV role, so if no media type, we're not interested in it.
          if (Role.getValue('media_type') <> nil) then
          begin

              if ((Role.getValue('media_type') as TJSONString).Value = 'movie') then
              begin
                TopMovieRoles.Add(RightStr('00000000'+IntToStr(Trunc(100000000-(role.getValue('popularity') as TJSONNumber).AsDouble*1000.0)),8)+'/'+IntToStr(RoleIndex));
              end
              else if ((role.getValue('media_type') as TJSONString).Value = 'tv') then
              begin
                TopTVRoles.Add(RightStr('00000000'+IntToStr(Trunc(100000000-(role.getValue('popularity') as TJSONNumber).AsDouble*1000.0)),8)+'/'+IntToStr(RoleIndex));
              end;

          end;
        end;
      end;
    end;

    // Sort the lists
    TopMovieRoles.Sort;
    TopTVRoles.Sort;

  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/TopRoles', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
    end;
  end;

  // This adds the most popular Movie roles to the 'header' part of the JSON.  All we're really after here is the poster and the title
  try
    if (TopMovieRoles.Count > 0) then
    begin

      // Currently, we have 5 top Movies and TV Shows
      TopLimit := 5;
      TopCount := 0;
      DeDupe   := 0;

      for RoleIndex := 0 to TopMovieRoles.Count - 1 do
      begin

        Role := (((TMDB_Data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[StrToInt(Copy(TopMovieRoles[RoleIndex],Pos('/',TopMovieRoles[RoleIndex])+1,8))] as TJSONObject);

        if ((Role.getValue('id') as TJSONNumber).AsInt <> DeDupe) then
        begin

          TopCount := TopCount + 1;
          ActorRoles := ActorRoles + 1;
          MovieRoles := MovieRoles + 1;
          DeDupe := (Role.getValue('id') as TJSONNumber).AsInt;

          if Topcount <= TopLimit then
          begin
            if (Role.getValue('title') = nil)
            then Actor := Actor+'"MT'+IntToStr(MovieRoles)+'":"Untitled",'
            else Actor := Actor+'"MT'+IntToStr(MovieRoles)+'":'+REST.JSON.TJSON.JSONEncode(Role.getValue('title') as TJSONString)+',';

            if  ((Role.getValue('adult') = nil) or (Role.getValue('adult') is TJSONNull) or ((role.getValue('adult') as TJSONBool).asBoolean = False)) // Family Friendly
            then Actor := actor+'"MX'+IntToStr(MovieRoles)+'":false,'
            else Actor := actor+'"MX'+IntToStr(MovieRoles)+'":true,';

            if not(Role.getValue('poster_path') = nil)
            then if ((Role.getValue('poster_path') is TJSONNULL))
                 then Actor := Actor+'"MP'+IntToStr(MovieRoles)+'":null,'
                 else Actor := actor+'"MP'+IntToStr(MovieRoles)+'":'+REST.JSON.TJSON.JSONEncode(Role.getValue('poster_path') as TJSONString)+',';

          end;
        end;
      end;
    end;

  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/TopMovieRoles', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
    end;
  end;


  // This adds the most popular TV roles to the 'header' part of the JSON.  All we're really after here is the poster and the title
  try
    if (TopTVRoles.Count > 0) then
    begin

      // Currently, we have 5 top Movies and TV Shows
      TopLimit := 5;
      TopCount := 0;
      DeDupe   := 0;

      for RoleIndex := 0 to TopTVRoles.Count-1 do
      begin

        Role := (((TMDB_Data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[StrToInt(Copy(TopTVRoles[RoleIndex],Pos('/',TopTVRoles[RoleIndex])+1,8))] as TJSONObject);

        if ((role.getValue('id') as TJSONNumber).AsInt <> DeDupe) then
        begin

          TopCount := TopCount + 1;
          ActorRoles := ActorRoles + 1;
          TVRoles := TVRoles + 1;
          DeDupe := (Role.getValue('id') as TJSONNumber).AsInt;

          if Topcount <= TopLimit then
          begin
            if (Role.getValue('name') = nil)
            then Actor := Actor+'"TT'+IntToStr(TVRoles)+'":"Untitled",'
            else Actor := Actor+'"TT'+IntToStr(TVRoles)+'":'+REST.JSON.TJSON.JSONEncode(Role.getValue('name') as TJSONString)+',';

            if  ((Role.getValue('adult') = nil) or (Role.getValue('adult') is TJSONNull) or ((role.getValue('adult') as TJSONBool).asBoolean = False)) // Family Friendly
            then Actor := actor+'"TX'+IntToStr(TVRoles)+'":false,'
            else Actor := actor+'"TX'+IntToStr(TVRoles)+'":true,';

            if not(Role.getValue('poster_path') = nil)
            then if ((Role.getValue('poster_path') is TJSONNULL))
                 then Actor := Actor+'"TP'+IntToStr(TVRoles)+'":null,'
                 else Actor := actor+'"TP'+IntToStr(TVRoles)+'":'+REST.JSON.TJSON.JSONEncode(Role.getValue('poster_path') as TJSONString)+',';

          end;
        end;
      end;
    end;

  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/TopTVRoles', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
    end;
  end;

  // Number of roles
  Actor := Actor+'"NUM":'+IntToStr(ActorRoles)+',';
  Actor := Actor+'"NOM":'+IntToStr(MovieRoles)+',';
  Actor := Actor+'"NOT":'+IntToStr(TVRoles)+',';



  // Now let's do the same thing again, but with more data for each role.  This is essentially building out the
  // contents of the second Tabulator table in its entirely - this will be passed directly as its own JSON
  // So we've got to get it right here as we're not planning on changing it later.
  Actor := Actor + '"WRK":[';
  TopCount := 0;

 // This adds all the Movie roles to the 'work' part of the JSON.
  try
    if (TopMovieRoles.Count > 0) then
    begin

      DeDupe := 0;

      for RoleIndex := 0 to TopMovieRoles.Count - 1 do
      begin

        Role := (((TMDB_Data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[StrToInt(Copy(TopMovieRoles[RoleIndex],Pos('/',TopMovieRoles[RoleIndex])+1,8))] as TJSONObject);

        if ((role.getValue('id') as TJSONNumber).AsInt <> DeDupe) then
        begin

          TopCount := TopCount + 1;
          DeDupe := (Role.getValue('id') as TJSONNumber).AsInt;

          if (TopCount = 1)
          then Actor := Actor+'{"ID":'+IntToStr(TopCount)
          else Actor := Actor+',{"ID":'+IntToStr(TopCount);

          // TMSDb ID
          Actor := Actor+',"TID":'+IntToStr((Role.getValue('id') as TJSONNumber).AsInt);

          // Moview or TV Show
          Actor := Actor+',"TYP":"movie"';

          // Family Friendly
          if  ((Role.getValue('adult') = nil) or (Role.getValue('adult') is TJSONNull) or ((role.getValue('adult') as TJSONBool).asBoolean = False))
          then Actor := Actor+',"XXX":false'
          else Actor := Actor+',"XXX":true';

          // Title
          if not(Role.getValue('title') = nil) and not(Role.getValue('title') is TJSONNULL)
          then Actor := Actor+',"NAM":'+REST.JSON.TJSON.JSONEncode(Role.getValue('title') as TJSONString);

          // Popularity
          if not(role.getValue('popularity') = nil) and not (Role.getValue('popularity') is TJSONNULL)
          then Actor := Actor+',"POP":'+FloatToStr((Role.getValue('popularity') as TJSONNumber).AsDouble);

          // Character
          if not(Role.getValue('character') = nil) and not(Role.getValue('character') is TJSONNULL)
          then Actor := Actor+',"CHR":'+REST.JSON.TJSON.JSONEncode(Role.getValue('character') as TJSONString);

          // Overview
          if not(Role.getValue('overview') = nil) and not(Role.getValue('overivew') is TJSONNULL)
          then Actor := Actor+',"OVR":'+REST.JSON.TJSON.JSONEncode(Role.getValue('overview') as TJSONString);

          // Release Date
          if not(Role.getValue('release_date') = nil) and not(Role.getValue('release_date') is TJSONNULL)
          then Actor := Actor+',"REL":"'+(Role.getValue('release_date') as TJSONString).Value+'"';

          // Poster
          if not(Role.getValue('poster_path') = nil)
          then if ((Role.getValue('poster_path') is TJSONNULL))
               then Actor := Actor+',"PIC":null'
               else Actor := Actor+',"PIC":'+REST.JSON.TJSON.JSONEncode(Role.getValue('poster_path') as TJSONString);



          // Get the extended version of the data for this title
          ShowData := TJSONObject.ParseJSONValue(GetMovieFromTMDb((Role.getValue('id') as TJSONNumber).AsInt, False)) as TJSONObject;

          // Budget
          if not(ShowData.getValue('budget') = nil)
          then Actor := Actor+',"BUD":'+FloatToStr((ShowData.getValue('budget') as TJSONNumber).AsDouble);

          // Revenue
          if not(ShowData.getValue('revenue') = nil)
          then Actor := Actor+',"REV":'+FloatToStr((ShowData.getValue('revenue') as TJSONNumber).AsDouble);

          // Runtime
          if not(ShowData.getValue('runtime') = nil)
          then Actor := Actor+',"RTM":'+FloatToStr((ShowData.getValue('runtime') as TJSONNumber).AsDouble);

          // Status
          if not(ShowData.getValue('status') = nil) and not((ShowData.getValue('status') is TJSONNULL)) then
          begin
            if REST.JSON.TJSON.JSONEncode(ShowData.getValue('status') as TJSONString) <> '""' then
            begin
              Actor := Actor+',"STS":'+REST.JSON.TJSON.JSONEncode(ShowData.getValue('status') as TJSONString);
            end;
          end;

          // Tagline
          if not(ShowData.getValue('tagline') = nil) and not((ShowData.getValue('tagline') is TJSONNULL)) then
          begin
            if REST.JSON.TJSON.JSONEncode(ShowData.getValue('tagline') as TJSONString) <> '""' then
            begin
              Actor := Actor+',"TGL":'+REST.JSON.TJSON.JSONEncode(ShowData.getValue('tagline') as TJSONString);
            end;
          end;

          // HomePage
          if not(ShowData.getValue('homepage') = nil) and not((ShowData.getValue('homepage') is TJSONNULL)) then
          begin
            if REST.JSON.TJSON.JSONEncode(ShowData.getValue('homepage') as TJSONString) <> '""' then
            begin
              Actor := Actor+',"WWW":'+REST.JSON.TJSON.JSONEncode(ShowData.getValue('homepage') as TJSONString);
            end;
          end;

          // External IDs
          if not(ShowData.getValue('external_ids') = nil) then
          begin

            // IMDb ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('imdb_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('imdb_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('imdb_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"IID":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('imdb_id') as TJSONString);
              end;
            end;

            // Facebook ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('facebook_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('facebook_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('facebook_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"FID":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('facebook_id') as TJSONString);
              end;
            end;

            // Twitter ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('twitter_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('twitter_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('twitter_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"TWT":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('twitter_id') as TJSONString);
              end;
            end;

            // Instagram ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('instagram_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('instagram_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('instagram_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"INS":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('instagram_id') as TJSONString);
              end;
            end;

          end; // external_ids


          // Production Countries
          if not(ShowData.getValue('production_countries') = nil) then
          begin
            // we've got countries?
            Images := ShowData.getValue('production_countries') as TJSONArray;
            // Add country count
            Actor := Actor+',"CTY":'+IntToStr(Images.Count)+',"CTG":[';
            for ImageIndex := 0 to Images.Count-1 do
            begin
              if ImageIndex = 0
              then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('iso_3166_1') as TJSONString).Value+'"'
              else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('iso_3166_1') as TJSONString).Value+'"';
            end;
            Actor := Actor+']';
          end; // Production Countries


          // Posters
          if not(ShowData.getValue('images') = nil) then
          begin
            if not((ShowData.getValue('images') as TJSONObject).getValue('posters') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('images') as TJSONObject).getValue('posters') as TJSONArray;
              // Add Image count
              Actor := Actor+',"PSC":'+IntToStr(Images.Count)+',"PSG":[';
              for ImageIndex := 0 to Images.Count-1 do
              begin
                if ImageIndex = 0
                then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"'
                else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"';
              end;
              Actor := Actor+']';
            end;
          end; // Posters


          // Backgrounds
          if not(ShowData.getValue('images') = nil) then
          begin
            if not((ShowData.getValue('images') as TJSONObject).getValue('backdrops') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('images') as TJSONObject).getValue('backdrops') as TJSONArray;
              // Add Image count
              Actor := Actor+',"BDC":'+IntToStr(Images.Count)+',"BDS":[';
              for ImageIndex := 0 to Images.Count-1 do
              begin
                if ImageIndex = 0
                then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"'
                else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"';
              end;
              Actor := Actor+']';
            end;
          end; // Backgrounds


          // Videos
          if not(ShowData.getValue('videos') = nil) then
          begin
            if not((ShowData.getValue('videos') as TJSONObject).getValue('results') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('videos') as TJSONObject).getValue('results') as TJSONArray;
              // Add Image count
              Actor := Actor+',"VDC":'+IntToStr(Images.Count)+',"VDS":[';
              for ImageIndex := 0 to Images.Count-1 do
              begin
                if ImageIndex = 0
                then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('site') as TJSONString).Value+':'+((Images.Items[ImageIndex] as TJSONObject).getValue('key') as TJSONString).Value+'"'
                else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('site') as TJSONString).Value+':'+((Images.Items[ImageIndex] as TJSONObject).getValue('key') as TJSONString).Value+'"';
              end;
              Actor := Actor+']';
            end;
          end; // Videos


          // Actors
          if not(ShowData.getValue('credits') = nil) then
          begin
            if not((ShowData.getValue('credits') as TJSONObject).getValue('cast') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('credits') as TJSONObject).getValue('cast') as TJSONArray;
              // Add Image count
              Actor := Actor+',"ACC":'+IntToStr(Images.Count)+',"ACA":[';
              for ImageIndex := 0 to Min(Images.Count-1,50) do
              begin
                if ImageIndex = 0
                then Actor := Actor+'{'
                else Actor := Actor+',{';

                if not((images.Items[ImageIndex] as TJSONObject).getValue('id') = nil)
                then Actor := Actor+'"ID":'+IntToStr(((images.Items[ImageIndex] as TJSONObject).getValue('id') as TJSONNumber).asInt);

                if not((images.Items[ImageIndex] as TJSONObject).getValue('character') = nil)
                then if not((images.Items[ImageIndex] as TJSONObject).getValue('character') is TJSONNULL)
                     then if Trim(((images.Items[ImageIndex] as TJSONObject).getValue('character') as TJSONSTring).Value) <> ''
                          then Actor := Actor+',"CHR":'+REST.JSON.TJSON.JSONEncode(((images.Items[ImageIndex] as TJSONObject).getValue('character') as TJSONSTring));

                if (ImageIndex < 10) then
                begin
                  // Family Friendly
                  if  (((images.Items[ImageIndex] as TJSONObject).getValue('adult') = nil) or ((images.Items[ImageIndex] as TJSONObject).getValue('adult') is TJSONNull) or (((images.Items[ImageIndex] as TJSONObject).getValue('adult') as TJSONBool).asBoolean = False))
                  then Actor := Actor+',"XXX":false'
                  else Actor := Actor+',"XXX":true';

                  if not((images.Items[ImageIndex] as TJSONObject).getValue('name') = nil)
                  then Actor := Actor+',"NAM":'+REST.JSON.TJSON.JSONEncode(((images.Items[ImageIndex] as TJSONObject).getValue('name') as TJSONSTring));

                  if not((images.Items[ImageIndex] as TJSONObject).getValue('profile_path') = nil)
                  then if ((images.Items[ImageIndex] as TJSONObject).getValue('profile_path') is TJSONNULL)
                       then Actor := Actor+',"PIC":null'
                       else Actor := Actor+',"PIC":"'+((images.Items[ImageIndex] as TJSONObject).getValue('profile_path') as TJSONSTring).Value+'"';

                  if not((images.Items[ImageIndex] as TJSONObject).getValue('order') = nil)
                  then Actor := Actor+',"ORD":'+IntToStr(((images.Items[ImageIndex] as TJSONObject).getValue('order') as TJSONNumber).asInt);
                end;
                Actor := Actor+'}';
              end;
              Actor := Actor+']';
            end;
          end; // Actors

          ShowData.Free;

          Actor := Actor + '}';
        end;
      end;
    end;

  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/AllMovieRoles', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
    end;
  end;



 // This adds all the TV roles to the 'work' part of the JSON.
  try
//    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data: All TV Roles","TP":'+FloatToStr(Now)+'}';

    if (TopTVRoles.Count > 0) then
    begin

      DeDupe := 0;

      for RoleIndex := 0 to TopTVRoles.Count-1 do
      begin

        Role := (((TMDB_Data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[StrToInt(Copy(TopTVRoles[RoleIndex],Pos('/',TopTVRoles[RoleIndex])+1,8))] as TJSONObject);

//        if  ((Role.getValue('adult') is TJSONNull) or ((role.getValue('adult') as TJSONBool).asBoolean = False)) and     // Family Friendly
        if    ((role.getValue('id') as TJSONNumber).AsInt <> DeDupe)    then    // Don't already have it?
        begin

          TopCount := TopCount + 1;
          DeDupe := (Role.getValue('id') as TJSONNumber).AsInt;

          if (TopCount = 1)
          then Actor := Actor+'{"ID":'+IntToStr(TopCount)
          else Actor := Actor+',{"ID":'+IntToStr(TopCount);

          if  ((Role.getValue('adult') = nil) or (Role.getValue('adult') is TJSONNull) or ((role.getValue('adult') as TJSONBool).asBoolean = False)) // Family Friendly
          then Actor := Actor+',"XXX":false'
          else Actor := Actor+',"XXX":true';

          // TMSDb ID
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV ID","TP":'+FloatToStr(Now)+'}';
          Actor := Actor+',"TID":'+IntToStr((Role.getValue('id') as TJSONNumber).AsInt);

          // Moview or TV Show
 //         MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV ID","TP":'+FloatToStr(Now)+'}';
          Actor := Actor+',"TYP":"'+(Role.getValue('media_type') as TJSONString).Value+'"';

          // Title
  //        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV NAM","TP":'+FloatToStr(Now)+'}';
          if not(Role.getValue('name') = nil) and not(Role.getValue('name') is TJSONNULL)
          then Actor := Actor+',"NAM":'+REST.JSON.TJSON.JSONEncode(Role.getValue('name') as TJSONString);

          // Popularity
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV POP","TP":'+FloatToStr(Now)+'}';
          if not(role.getValue('popularity') = nil) and not (Role.getValue('popularity') is TJSONNULL)
          then Actor := Actor+',"POP":'+FloatToStr((Role.getValue('popularity') as TJSONNumber).AsDouble);

          // Character
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV CHR","TP":'+FloatToStr(Now)+'}';
          if not(Role.getValue('character') = nil) and not(Role.getValue('character') is TJSONNULL)
          then Actor := Actor+',"CHR":'+REST.JSON.TJSON.JSONEncode(Role.getValue('character') as TJSONString);

          // Overview
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV OVR","TP":'+FloatToStr(Now)+'}';
          if not(Role.getValue('overview') = nil) and not(Role.getValue('overivew') is TJSONNULL)
          then Actor := Actor+',"OVR":'+REST.JSON.TJSON.JSONEncode(Role.getValue('overview') as TJSONString);

          // First Air Date
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV REL","TP":'+FloatToStr(Now)+'}';
          if not(Role.getValue('first_air_date') = nil) and not(Role.getValue('first_air_date') is TJSONNULL)
          then Actor := Actor+',"REL":"'+(Role.getValue('first_air_date') as TJSONString).Value+'"';

          // Poster
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"TV PIC","TP":'+FloatToStr(Now)+'}';
          if not(Role.getValue('poster_path') = nil)
          then if ((Role.getValue('poster_path') is TJSONNULL))
               then Actor := Actor+',"PIC":null'
               else Actor := Actor+',"PIC":'+REST.JSON.TJSON.JSONEncode(Role.getValue('poster_path') as TJSONString);


          // Get the extended version of the data for this title
          ShowData := TJSONObject.ParseJSONValue(GetTVShowFromTMDb((Role.getValue('id') as TJSONNumber).AsInt, False)) as TJSONObject;

          // Episode Runtime - let's just take the first number we find
          if (ShowData.getValue('episode_run_time') <> nil)
          then if (ShowData.getValue('episode_run_time') is TJSONArray)
               then if ((ShowData.getValue('episode_run_time') as TJSONArray).Count > 0)
                    then if ((ShowData.getValue('episode_run_time') as TJSONArray).Items[0] is TJSONNumber)
                         then Actor := Actor+',"RTM":'+FloatToStr(((ShowData.getValue('episode_run_time') as TJSONArray).Items[0] as TJSONNumber).AsDouble);

          // Episodes
          if not(ShowData.getValue('number_of_episodes') = nil)
          then if (ShowData.getValue('number_of_episodes') is TJSONNumber)
               then Actor := Actor+',"EPS":'+FloatToStr((ShowData.getValue('number_of_episodes') as TJSONNumber).AsDouble);

          // Seasons
          if not(ShowData.getValue('number_of_seasons') = nil)
          then if (ShowData.getValue('number_of_seasons') is TJSONNumber)
               then Actor := Actor+',"SNS":'+FloatToStr((ShowData.getValue('number_of_seasons') as TJSONNumber).AsDouble);

          // Status
          if not(ShowData.getValue('status') = nil) and not((ShowData.getValue('status') is TJSONNULL)) then
          begin
            if REST.JSON.TJSON.JSONEncode(ShowData.getValue('status') as TJSONString) <> '""' then
            begin
              Actor := Actor+',"STS":'+REST.JSON.TJSON.JSONEncode(ShowData.getValue('status') as TJSONString);
            end;
          end;

          // Tagline
          if not(ShowData.getValue('tagline') = nil) and not((ShowData.getValue('tagline') is TJSONNULL)) then
          begin
            if REST.JSON.TJSON.JSONEncode(ShowData.getValue('tagline') as TJSONString) <> '""' then
            begin
              Actor := Actor+',"TGL":'+REST.JSON.TJSON.JSONEncode(ShowData.getValue('tagline') as TJSONString);
            end;
          end;

          // HomePage
          if not(ShowData.getValue('homepage') = nil) and not((ShowData.getValue('homepage') is TJSONNULL)) then
          begin
            if REST.JSON.TJSON.JSONEncode(ShowData.getValue('homepage') as TJSONString) <> '""' then
            begin
              Actor := Actor+',"WWW":'+REST.JSON.TJSON.JSONEncode(ShowData.getValue('homepage') as TJSONString);
            end;
          end;

          // External IDs
          if not(ShowData.getValue('external_ids') = nil) then
          begin

            // IMDb ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('imdb_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('imdb_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('imdb_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"IID":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('imdb_id') as TJSONString);
              end;
            end;

            // Facebook ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('facebook_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('facebook_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('facebook_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"FID":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('facebook_id') as TJSONString);
              end;
            end;

            // Twitter ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('twitter_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('twitter_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('twitter_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"TWT":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('twitter_id') as TJSONString);
              end;
            end;

            // Instagram ID
            if not((ShowData.getValue('external_ids') as TJSONObject).getValue('instagram_id') = nil) and not((ShowData.getValue('external_ids') as TJSONObject).getValue('instagram_id') is TJSONNULL) then
            begin
              if (REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('instagram_id') as TJSONString)) <> '""' then
              begin
                Actor := Actor+',"INS":'+REST.JSON.TJSON.JSONEncode((ShowData.getValue('external_ids') as TJSONOBJECT).getValue('instagram_id') as TJSONString);
              end;
            end;

          end; // external_ids


          // Production Countries
          if not(ShowData.getValue('production_countries') = nil) then
          begin
            // we've got countries?
            Images := ShowData.getValue('production_countries') as TJSONArray;
            // Add country count
            Actor := Actor+',"CTY":'+IntToStr(Images.Count)+',"CTG":[';
            for ImageIndex := 0 to Images.Count-1 do
            begin
              if ImageIndex = 0
              then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('iso_3166_1') as TJSONString).Value+'"'
              else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('iso_3166_1') as TJSONString).Value+'"';
            end;
            Actor := Actor+']';
          end; // Production Countries


          // Posters
          if not(ShowData.getValue('images') = nil) then
          begin
            if not((ShowData.getValue('images') as TJSONObject).getValue('posters') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('images') as TJSONObject).getValue('posters') as TJSONArray;
              // Add Image count
              Actor := Actor+',"PSC":'+IntToStr(Images.Count)+',"PSG":[';
              for ImageIndex := 0 to Images.Count-1 do
              begin
                if ImageIndex = 0
                then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"'
                else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"';
              end;
              Actor := Actor+']';
            end;
          end; // Posters


          // Backgrounds
          if not(ShowData.getValue('images') = nil) then
          begin
            if not((ShowData.getValue('images') as TJSONObject).getValue('backdrops') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('images') as TJSONObject).getValue('backdrops') as TJSONArray;
              // Add Image count
              Actor := Actor+',"BDC":'+IntToStr(Images.Count)+',"BDS":[';
              for ImageIndex := 0 to Images.Count-1 do
              begin
                if ImageIndex = 0
                then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"'
                else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('file_path') as TJSONString).Value+'"';
              end;
              Actor := Actor+']';
            end;
          end; // Backgrounds


          // Videos
          if not(ShowData.getValue('videos') = nil) then
          begin
            if not((ShowData.getValue('videos') as TJSONObject).getValue('results') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('videos') as TJSONObject).getValue('results') as TJSONArray;
              // Add Image count
              Actor := Actor+',"VDC":'+IntToStr(Images.Count)+',"VDS":[';
              for ImageIndex := 0 to Images.Count-1 do
              begin
                if ImageIndex = 0
                then Actor := Actor+'"'+((Images.Items[ImageIndex] as TJSONObject).getValue('site') as TJSONString).Value+':'+((Images.Items[ImageIndex] as TJSONObject).getValue('key') as TJSONString).Value+'"'
                else Actor := Actor+',"'+((Images.Items[ImageIndex] as TJSONObject).getValue('site') as TJSONString).Value+':'+((Images.Items[ImageIndex] as TJSONObject).getValue('key') as TJSONString).Value+'"';
              end;
              Actor := Actor+']';
            end;
          end; // Videos


          // Actors
          ThisRoleName := '';
          if not(ShowData.getValue('aggregate_credits') = nil) then
          begin
            if not((ShowData.getValue('aggregate_credits') as TJSONObject).getValue('cast') = nil)  then
            begin
              // we've got images?
              Images := (ShowData.getValue('aggregate_credits') as TJSONObject).getValue('cast') as TJSONArray;
              // Add Image count
              Actor := Actor+',"ACC":'+IntToStr(Images.Count)+',"ACA":[';
              for ImageIndex := 0 to min(Images.Count-1,50) do
              begin
                if ImageIndex = 0
                then Actor := Actor+'{'
                else Actor := Actor+',{';

                if not((images.Items[ImageIndex] as TJSONObject).getValue('id') = nil)
                then Actor := Actor+'"ID":'+IntToStr(((images.Items[ImageIndex] as TJSONObject).getValue('id') as TJSONNumber).asInt);

                if not((images.Items[ImageIndex] as TJSONObject).getValue('order') = nil)
                then Actor := Actor+',"ORD":'+IntToStr(((images.Items[ImageIndex] as TJSONObject).getValue('order') as TJSONNumber).asInt);

                RoleName := '';
                if ((images.Items[ImageIndex] as TJSONObject).getValue('roles') <> nil) then
                begin
                  if ((images.Items[ImageIndex] as TJSONObject).getValue('roles') is TJSONArray) then
                  begin
                    for i := 0 to ((images.Items[ImageIndex] as TJSONObject).getValue('roles') as TJSONArray).Count -1 do
                    begin
                        if Trim(((((images.Items[ImageIndex] as TJSONObject).getValue('roles') as TJSONArray).Items[i] as TJSONObject).getValue('character') as TJSONString).Value) <> ''
                        then RoleName := RoleName+((((images.Items[ImageIndex] as TJSONObject).getValue('roles') as TJSONArray).Items[i] as TJSONObject).getValue('character') as TJSONString).Value+'<br />';
                        RoleName := RoleName+'('+FloatToStrF(((((images.Items[ImageIndex] as TJSONObject).getValue('roles') as TJSONArray).Items[i] as TJSONObject).getValue('episode_count') as TJSONNumber).AsInt,ffNumber,5,0)+' Eps)<br />';
                    end;
                  end;
                end;
                RoleName := Trim(StringReplace(RoleName,'"','&quot;',[rfReplaceAll]));
                if (RoleName <> '')
                then Actor := Actor+',"CHR":"'+RoleName+'"';

                if (RoleName <> '') and (((images.Items[ImageIndex] as TJSONObject).getValue('id') as TJSONNumber).asInt = StrToInt(ActorRef))
                then ThisRoleName := RoleName;

                if (ImageIndex < 10) then
                begin
                  if not((images.Items[ImageIndex] as TJSONObject).getValue('name') = nil)
                  then Actor := Actor+',"NAM":'+REST.JSON.TJSON.JSONEncode(((images.Items[ImageIndex] as TJSONObject).getValue('name') as TJSONSTring));

                  if not((images.Items[ImageIndex] as TJSONObject).getValue('profile_path') = nil)
                  then if ((images.Items[ImageIndex] as TJSONObject).getValue('profile_path') is TJSONNULL)
                       then Actor := Actor+',"PIC":null'
                       else Actor := Actor+',"PIC":"'+((images.Items[ImageIndex] as TJSONObject).getValue('profile_path') as TJSONSTring).Value+'"';

                  // Family Friendly
                  if  (((images.Items[ImageIndex] as TJSONObject).getValue('adult') = nil) or ((images.Items[ImageIndex] as TJSONObject).getValue('adult') is TJSONNull) or (((images.Items[ImageIndex] as TJSONObject).getValue('adult') as TJSONBool).asBoolean = False))
                  then Actor := Actor+',"XXX":false'
                  else Actor := Actor+',"XXX":true';
                end;
                Actor := Actor+'}';
              end;
              Actor := Actor+']';
            end;
          end; // Actors

          if ThisRoleName <> ''
          then Actor := Actor+',"CHX":"'+ThisRoleName+'"';

          ShowData.Free;

          Actor := Actor + '}';

        end;
      end;
    end;

  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/AllTVRoles', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
    end;
  end;


  // Next, we want to calculate our very own score using our own metrics to
  // counter the silly scores that TMDb returns.  They're not 100% terrible but
  // terrible enough to warrant making up our own.  So here, we're aiming for a
  // score out of 1,000 available points.  Each section contributes a score of
  // up to 100 points, and we've got ten sections.  The resulting calculation
  // is then stored as a value that we'll subsequently track over time.  A bit
  // of a work in progress, but it has got to be better than what we had!
  ActorScore := 0;

  // Section 1/10 - The TMDb POP value
  // OK, it is not entirely without merit, so we'll include it but at 1/10 power
  ActorScore := ActorScore + Min(PopScore, 100.0);

  // Section 2/10 - Social Media
  // 10pts for every social media account we can find, up to a maximum of 100pts
  // The idea is that if you have no website or social media presence then just
  // perhaps you might not be all that popular
  ActorScore := ActorScore + Min(SocialMedia * 10.0, 100.0);

  // Section 3/10 - Basic Data
  // How much do we know about them?  10pts for everything we've been able to
  // find out.  And yes, they get 10 more points if you're dead vs. alive
  // Some are weighted more to penalize those who don't have it (IE, photo, bio)
  // birthday - 10pts
  // deathday - 10pts
  // citizenship - 10pts
  // birthplace - 10pts
  // height - 10pts
  // biography - 25pts
  // photo - 25pts
  ActorScore := ActorScore + Min(BasicScore, 100.0);

  // Section 4/10 - Body of Work - Movie Roles
  ActorScore := ActorScore + Min(100.0*(TopMovieRoles.Count/250.0), 100.0);

  // Section 5/10 - Body of Work - TV Show Roles
  ActorScore := ActorScore + Min(100.0*(TopTVRoles.Count/250), 100.0);

  // Section 6/10 - Movies Role Prominence - Starring vs. Supporting Roles + Earnings
  // Section 7/10 - Movies Recent Work
  // Section 8/10 - TV Shows Role Prominence - Starring vs. Supporting vs. Guest + Episodes
  // Section 9/10 - TV Shows Recent Work
  // Section 10/10 - Awards Recognition

  Actor := Actor + '],"PTS":'+FloatToStrF(ActorScore,ffNumber,5,3)+',';
  Actor := Actor + '"PTB":{'+
    '"Basic":'+FloatToStrF(Min(BasicScore,100.0),ffNumber,6,3)+','+
    '"TMDb":'+FloatToStrF(Min(PopScore,100.0),ffNumber,6,3)+','+
    '"Social":'+FloatToStrF(Min(SocialMedia*10,100.0),ffNumber,6,3)+','+
    '"Movies":'+FloatToStrF(Min(100.0*(TopMovieRoles.Count/250), 100.0),ffNumber,6,3)+','+
    '"MoviesBonus":'+FloatToStrF(Min(0.0,100.0),ffNumber,6,3)+','+
    '"MoviesRecent":'+FloatToStrF(Min(0.0,100.0),ffNumber,6,3)+','+
    '"TVShows":'+FloatToStrF(Min(100.0*(TopTVRoles.Count/250), 100.0),ffNumber,6,3)+','+
    '"TVBonus":'+FloatToStrF(Min(0.0,100.0),ffNumber,6,3)+','+
    '"TVRecent":'+FloatToStrF(Min(0.0,100.0),ffNumber,6,3)+','+
    '"Awards":'+FloatToStrF(Min(0.0,100.0),ffNumber,6,3)+
  '}}';

  // All Done !!!
  if ActorRoles = 0 then Actor := '';

  Result := FilterResponse(Actor);


  ShowData := TJSONObject.Create;
  try
    if Result <> '' then
    begin
      ShowData := TJSONObject.ParseJSONValue(Result) as TJSONObject;
      TestResult := 'Actor: '+ActorRef+' ID: '+IntToSTr((ShowData.getValue('ID') as TJSONNUmber).AsInt).PadLeft(4)+' Length: '+IntToStr(Length(Actor)).PadLeft(8);
    end;
  except on E: Exception do
    begin
      MainForm.LogException('ProcessPerson/Validation', E.ClassName, E.Message, ActorRef);
      MainForm.LogEvent(Copy(Actor,1,150));
      Result := '';
    end;
  end;

  SaveActoriousPersonData(Result, StrToInt(ActorRef), AdultActor);

  ShowData.Free;
  TopMovieRoles.Free;
  TopTVRoles.Free;
  if (WikiData is TJSONArray) then WikiData.Free;
  if (TMDB_Data is TJSONObject) then TMDB_Data.Free;
end;

function ProcessMovie(MovieID: Integer; MovieRef: String; TMDB_Data: TJSONObject; WikiIndex: Integer; WikiData: TJSONArray; ProgressPrefix: String; ProgressKey: Integer):String;
var
  Movie: String;  // This is the result set we're building up

//  MovieRoles: Integer; // Count of how many qualified Roles were found

//  RoleIndex: Integer; // Used to iterate through roles
//  Images: TJSONArray;  // Images available for the person
//  ImageIndex: Integer; // Used to index images

  AdultMovie: Boolean;

begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');


  AdultMovie := False;

  // Check if we've got valid TMDb data?
  try
    if not(TMDB_Data.getValue('success') = nil) and (TMDB_Data.getValue('success') is TJSONBool) then
    begin
      if (WikiIndex <> -1)
      then MainForm.LogEvent('- ProcessMovie: No data for '+MovieRef+': '+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('movie') as TJSONObject).GetValue('value') as TJSONString).Value)
      else MainForm.LogEvent('- ProcessMovie: No data for '+MovieRef+' (no WikiData reference)');
      Result := '';
      exit;
    end;
  except on E: Exception do
    begin
      MainForm.LogEvent('EXCEPTION Processing Movie Validation Check '+MovieRef);
      MainForm.LogEvent(E.ClassName+': '+E.Message);
      MainForm.LogEvent(Copy(Movie,1,150));
    end;
  end;

  // Here we are building up a new JSON object to store everything to do with one Movie.
  // This is sort of the 'header' infromation
  try
    // ID is used by Tabulator as an index, so handy to just add it right here.
    Movie :='{"ID":'+IntToStr(MovieID)+',';

    // Add a timestamp so that we know when this was generated
    Movie := Movie+'"DAT":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'",';

    // Name - Assume it is always present?
//    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data: Name","TP":'+FloatToStr(Now)+'}';
    Movie := Movie+'"NAM":'+REST.JSON.TJSON.JSONEncode(TMDB_Data.getValue('title') as TJSONString)+',';

    // TMDb ID
//    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data: TMDb","TP":'+FloatToStr(Now)+'}';
    Movie := Movie+'"TID":'+IntToStr(StrToInt(MovieRef))+',';

    // WikiData ID
//    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data: WikiData","TP":'+FloatToStr(Now)+'}';
    if (WikiIndex <> -1)
    then Movie := Movie+'"WID":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('movie') as TJSONObject).GetValue('value') as TJSONString).Value+'",';


    // Entries from WikiData Query

    // Rotten Tomatoes ID
//    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data: RT","TP":'+FloatToStr(Now)+'}';
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('RTID') <> nil)
    then Movie := Movie+'"RID":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('RTID') as TJSONObject).GetValue('value') as TJSONString).Value+'",';

    // MetaCritic ID
//    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data: MetaCritic","TP":'+FloatToStr(Now)+'}';
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('MetaCriticID') <> nil)
    then Movie := Movie+'"MET":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('MetaCriticID') as TJSONObject).GetValue('value') as TJSONString).Value+'",';

    // Wikipedia Link
//    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data: Wikipedia","TP":'+FloatToStr(Now)+'}';
    if (WikiIndex <> -1) and ((WikiData.Items[WikiIndex] as TJSONObject).getValue('Wikipedia') <> nil)
    then Movie := Movie+'"WIK":"'+(((WikiData.Items[WikiIndex] as TJSONObject).getValue('Wikipedia') as TJSONObject).GetValue('value') as TJSONString).Value+'",';


    // Entries from TMDb Query

    // Family Friendly... Or not?
    AdultMovie := False;
    if not(TMDB_Data.getValue('adult') = nil) and not((TMDB_Data.getValue('adult') is TJSONNULL)) then
    begin
      if (TMDB_Data.getValue('adult') as TJSONBool).AsBoolean = false then
      begin
        Movie := Movie+'"XXX":false,';
      end
      else
      begin
        Movie := Movie+'"XXX":true,';
        AdultMovie := True;
      end;
    end
    else Movie := Movie+'"XXX":false,';

    if not(TMDB_Data.getValue('popularity') = nil) and not((TMDB_Data.getValue('popularity') is TJSONNULL))
    then Movie := Movie+'"POP":'+FloatToStr((TMDB_Data.getValue('popularity') as TJSONNumber).AsDouble)
    else Movie := Movie+'"POP":0.0';

  except on E: Exception do
    begin
      MainForm.LogEvent('EXCEPTION Processing Movie Data '+MovieRef);
      MainForm.LogEvent(E.ClassName+': '+E.Message);
      MainForm.LogEvent(Copy(Movie,1,150));
    end;
  end;

  // All Done !!!
  Movie := Movie+'}';
  SaveActoriousMovieData(Movie, StrToInt(MovieRef), AdultMovie);
  Result := Movie;

end;



///////////////////////////////////////////////////////////////////////////////////////////////////
// GetPerson                                                                                     //
//                                                                                               //
// Figure out if we've got a person already or if we have to generate a person's data from       //
// either the pre-existing TMDb data, or whether we have to get that as well.                    //
///////////////////////////////////////////////////////////////////////////////////////////////////
function GetPerson(ActorID: Integer; ActorCount: Integer; ActorRef: String; ActorRefShort: String; Wiki:String; ProgressPrefix: String; ProgressKey: Integer; ForceUpdate: Boolean): String;
var
  CacheFile: String;
  TMDBCacheFile: String;
  PersonData: TStringList;
  Update: Boolean;
  Reason: String;
  TMDB: String;
begin

  Reason        := 'Cache';
  CacheFile     := 'cache/people/actorious/'+ActorRefShort+'/person-'+ActorRef+'.json';
  TMDBCacheFile := 'cache/people/tmdb/'     +ActorRefShort+'/person-'+ActorRef+'.json';

  // Determine whether we're updating;
  Update := ForceUpdate;
  if not(Update) then
  begin
    if FileExists(CacheFile) then
    begin
      if (TFile.GetLastWriteTime(CacheFile) < (Now - 5))  or (FilesizeByName(CacheFile) < 10) then
      begin
        Update := True;
        Reason := 'Age';
      end
    end
    else
    begin
      Update := True;
      Reason := 'Miss';
      MainForm.LogEvent('- GetPerson Cache Miss: '+ActorRef);
    end;
  end
  else
  begin
    Reason := 'Force';
  end;

  if Update then
  begin
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Requesting TMDb# '+ActorRef+' ( '+IntToStr(ActorID)+' of '+IntToStr(ActorCount)+' )","TP":'+FloatToStr(Now)+'}';
    TMDB := GetPersonFromTMDB(StrToInt(ActorRef),ForceUpdate);
    if Pos('"success":false,"status_code"', TMDB) > 0 then
    begin
      Result := '';
    end
    else
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing TMDb# '+ActorRef+' ( '+IntToStr(ActorID)+' of '+IntToStr(ActorCount)+' )","TP":'+FloatToStr(Now)+'}';
      if Wiki = ''
      then Result := ProcessActor(ActorID, ActorRef, TMDB, -1, '[]', ProgressPrefix, ProgressKey, ForceUpdate)
      else Result := ProcessActor(ActorID, ActorRef, TMDB, 0, '['+Wiki+']', ProgressPrefix, ProgressKey, ForceUpdate);
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Returning TMDb# '+ActorRef+' ( '+IntToStr(ActorID)+' of '+IntToStr(ActorCount)+' )","TP":'+FloatToStr(Now)+'}';
    end;
  end
  else
  begin
    inc(MainForm.PersonCacheRequests);
    if Reason = 'Cache' then inc(MainForm.PersonCacheHit);
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Loading TMDb# '+ActorRef+' ( '+IntToStr(ActorID)+' of '+IntToStr(ActorCount)+' )","TP":'+FloatToStr(Now)+'}';
    PersonData := TStringList.Create;
    PersonData.LoadFromFile(CacheFile, TEncoding.UTF8);
    if Pos('"success":false', PersonData.Text) > 0 then
    begin
      Result := '';
    end
    else
    begin
      Result := FilterResponse(PersonData.Text);
    end;
    PersonData.Free;
  end;
end;



function TActorInfoService.Progress(Secret, Progress: String): String;
begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

end;

function TActorInfoService.BirthDay(Secret: String; aMonth, aDay: Integer; Progress: String): TStream;
var
  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  cacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ClientReq: TNetHTTPClient;        // Used to connect to Wikidata
  SPARQL: String;                   // The SPARQL query being passed to Wikidata
  URL: String;                      // Wikidata SPARQL endpoint URL

  Response: TStream;                // Response from Wikidata
  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  CacheFile: String;                // Where the cache for this execution is stored
begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  CacheFile := 'cache/days/wikidata-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"BirthDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+',"TP":'+FloatToStr(Now)+'}');

  // Is there a Cached Response on disk?
  CacheBRResponse := TMemoryStream.Create;
  try
    CacheBRResponse.LoadFromFile(CacheFile+'.br');
  except on E: Exception do
    begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
//      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"WikiData Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if (CacheBRResponse.Size > 0) and (Progress <> MainForm.CurrentProgress.Caption) then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile)));
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
    CacheBRResponse.Free;

  end
  else
  begin
    CacheBRResponse.Free;

    // We don't have data, so lets try and get some
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Contacting Wikidata","TP":'+FloatToStr(Now)+'}';

    // Setup the client connection
    ClientReq := TNetHTTPClient.Create(nil);
    ClientReq.ConnectionTimeout := 900000; // 15 minutes
    ClientReq.ResponseTimeout := 900000; // 15 minutes
    ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    // Get the query from the MainForm, apply parameters
    SPARQL := MainForm.sparqlBirthDays.Lines.Text;
    SPARQL := StringReplace(StringReplace(SPARQL, ':MONTH', IntToStr(aMonth), [rfReplaceAll]), ':DAY', IntToStr(aDay), [rfReplaceAll]);

    // Setup the URL and encode the query in it
    URL := TidURI.URLEncode('https://query.wikidata.org/sparql?query='+SPARQL+'&format=json');
    try
      Response := ClientReq.Get(URL).ContentStream;

      // Save the response to disk as-is
      ResponseFile := TMemoryStream.Create;
      ResponseFile.CopyFrom(Response,Response.size);
      ResponseFile.SaveToFile(CacheFile);
      ResponseFile.Seek(0, soFromBeginning);

      // Compress the stream with Brotli
      Brotli := TMemoryStream.Create;
      BrotliCompressStream(ResponseFile, Brotli, bcMax);
      Brotli.Seek(0, soFromBeginning);

      // Save the Brotli-compressed response to disk
      Brotli.SaveToFile(CacheFile+'.br');
      ResponseFile.Free;
      Brotli.Free;
    except on E: Exception do
      begin
//        MainForm.LogEvent(E.ClassName+': '+E.Message);
      end;
    end;

    // Now check again - Is there a Cached Response on disk?
    CacheBRResponse := TMemoryStream.Create;
    try
      CacheBRResponse.LoadFromFile(CacheFile+'.br');
    except on E: Exception do
      begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
      end;
    end;

    if CacheBRResponse.Size > 0 then
    begin
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile)));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
      CacheBRResponse.Free;
    end
    else
    begin
      // Well, we STILL don't have data, so let's give up for now
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Available","TP":'+FloatToStr(Now)+'}';
      CacheResponse := TStringList.Create;
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
    end;

    ClientReq.Free;
  end;

//  MainForm.LogEvent(MainForm.Progress[ProgressKey]);

end;

function TActorInfoService.DeathDay(Secret: String; aMonth, aDay: Integer; Progress: String): TStream;
var
  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  cacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ClientReq: TNetHTTPClient;        // Used to connect to Wikidata
  SPARQL: String;                   // The SPARQL query being passed to Wikidata
  URL: String;                      // Wikidata SPARQL endpoint URL

  Response: TStream;                // Response from Wikidata
  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  CacheFile: String;                // Where the cache for this execution is stored
begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  CacheFile := 'cache/days/wikidata-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"DeathDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');

  // Is there a Cached Response on disk?
  CacheBRResponse := TMemoryStream.Create;
  try
    CacheBRResponse.LoadFromFile(CacheFile+'.br');
  except on E: Exception do
    begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
//      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"WikiData Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if (CacheBRResponse.Size > 0) and (Progress <> MainForm.CurrentProgress.Caption) then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile)));
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
    CacheBRResponse.Free;

  end
  else
  begin
    CacheBRResponse.Free;

    // We don't have data, so lets try and get some
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Contacting Wikidata","TP":'+FloatToStr(Now)+'}';

    // Setup the client connection
    ClientReq := TNetHTTPClient.Create(nil);
    ClientReq.ConnectionTimeout := 900000; // 15 minutes
    ClientReq.ResponseTimeout := 900000; // 15 minutes
    ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    // Get the query from the MainForm, apply parameters
    SPARQL := MainForm.sparqlDeathDays.Lines.Text;
    SPARQL := StringReplace(StringReplace(SPARQL, ':MONTH', IntToStr(aMonth), [rfReplaceAll]), ':DAY', IntToStr(aDay), [rfReplaceAll]);

    // Setup the URL and encode the query in it
    URL := TidURI.URLEncode('https://query.wikidata.org/sparql?query='+SPARQL+'&format=json');
    try
      Response := ClientReq.Get(URL).ContentStream;

      // Save the response to disk as-is
      ResponseFile := TMemoryStream.Create;
      ResponseFile.CopyFrom(Response,Response.size);
      ResponseFile.SaveToFile(CacheFile);
      ResponseFile.Seek(0, soFromBeginning);

      // Compress the stream with Brotli
      Brotli := TMemoryStream.Create;
      BrotliCompressStream(ResponseFile, Brotli, bcMax);
      Brotli.Seek(0, soFromBeginning);

      // Save the Brotli-compressed response to disk
      Brotli.SaveToFile(CacheFile+'.br');
      ResponseFile.Free;
      Brotli.Free;
    except on E: Exception do
      begin
//        MainForm.LogEvent(E.ClassName+': '+E.Message);
      end;
    end;

    // Now check again - Is there a Cached Response on disk?
    CacheBRResponse := TMemoryStream.Create;
    try
      CacheBRResponse.LoadFromFile(CacheFile+'.br');
    except on E: Exception do
      begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
      end;
    end;

    if CacheBRResponse.Size > 0 then
    begin
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile)));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
      CacheBRResponse.Free;
    end
    else
    begin
      // Well, we STILL don't have data, so let's give up for now
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Available","TP":'+FloatToStr(Now)+'}';
      CacheResponse := TStringList.Create;
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
    end;

    ClientReq.Free;
  end;

//  MainForm.LogEvent(MainForm.Progress[ProgressKey]);

end;

function TActorInfoService.GetClientVersion(Day:string): TStream;
var
  FirstPhoto: TStringList;
  Reply: String;
  ReplyStream: TStringStream;
begin
  // All we're doing here is returning a bit of version information
  // As an added bonus, we're also returning the image to load initially,
  // so it can be displayed AASAFP. Part of improving that Google Lighthouse
  // score is hugely related to just how quickly that image is updated.

  // First, did they send the correct request?
  if (Length(Day) <> 4) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

//  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
  FirstPhoto := TStringList.Create;
  FirstPhoto.Text := '';
  try
    FirstPhoto.LoadFromFile('cache/days/first/'+Day+'.json', TEncoding.UTF8);
  except on E: Exception do
    begin
    end;
  end;

  if FirstPhoto.Text = ''
  then FirstPhoto.Text := '"PIC":"img/person-placeholder.png","NAM":"Unknown","IMG":"img/person-placeholder.png","NUM":0,"BIO":""';

  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');

  Reply := '{"VER":"'+Trim(MainForm.edtClientVersion.Text)+'",'+FirstPhoto.Text+'}';
  ReplyStream := TStringStream.Create(Reply);
  ReplyStream.Seek(0, soFromBeginning);

  Result := TMemoryStream.Create;
  Result.CopyFrom(ReplyStream, ReplyStream.size);

  ReplyStream.Free;
  FirstPhoto.Free;
end;

function TActorInfoService.Lookup(Secret, Lookup, Progress: String): TStream;
var
  Data: TJSONArray;
  ProgressPrefix: String;
  ProgressKey: Integer;
  Actors: String;
  ActorCount: Integer;
  Response: TStringList;
  CacheFile: String;
  i : Integer;
  ActorData: TJSONObject;
  ActorNum: String;
  NotBrotli: TMemoryStream;
  Brotli: TMemoryStream;
  LookupData: TJSONObject;

begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they send a valid search?
  LookupData := TJSONObject.ParseJSONValue(Lookup) as TJSONObject;
  if (LookupData = nil) then raise EXDataHttpUnauthorized.Create('Lookup not Authorized');


  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"Lookup"'+
                    ',"TM":"'+QuotedStr(Lookup)+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');

  Response := TStringList.Create;

  Actors := '{"PPL":[';
  ActorCount := 1;
  Data := (LookupData as TJSONObject).getValue('PPL') as TJSONArray;

  for i := 0 to Data.Count - 1 do
  begin
    ActorNum := RightStr('0000000'+IntToStr(((data.items[i] as TJSONObject).getValue('ID') as TJSONNumber).AsInt),8);
    CacheFile := 'cache/people/actorious/'+
      RightStr(ActorNum,3)+
      '/person-'+
      ActorNum+
      '.json';
    try
      Response.Text := '';
      Response.LoadFromFile(CacheFile, TEncoding.UTF8)
    except on E: Exception do
      begin
        Response.Text := GetPersonfromTMDb(StrToInt(ActorNum), False);
        ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
        ProcessActor(ActorCount, ActorNum, ActorData.ToString, -1, '[]', ProgressPrefix, ProgressKey, False);
        ActorData.Free;
        Response.LoadFromFile('cache/people/actorious/'+
          RightStr(ActorNum,3)+
          '/person-'+
          ActorNum+
          '.json', TEncoding.UTF8);
      end;
    end;

    if (Response.Text <> '')  then
    begin
      ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
      ActorData.RemovePair('ID');
      ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(ActorCount)));

      if ActorCount = 1
      then Actors := Actors+ActorData.ToString
      else Actors := Actors+','+ActorData.ToString;
      ActorCount := ActorCount + 1;

      ActorData.Free;
    end;
  end;
  Actors := Actors +']}';
  ActorCount := ActorCount - 1;

  Response.Text := Actors;
//  MainForm.LogEvent(Response.Text);

  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Lookup for '+QuotedStr(Lookup)+' ( '+IntToStr(ActorCount)+' Match(es) Found )","TP":'+FloatToStr(Now)+'}';

  // This is what we're sending back
  NotBrotli := TMemoryStream.Create;
  Response.SaveToStream(NotBrotli);
  NotBrotli.Seek(0, soFromBeginning);

  // Compress the stream with Brotli
  Brotli := TMemoryStream.Create;
  BrotliCompressStream(NotBrotli, Brotli, bcGood);
  Brotli.Seek(0, soFromBeginning);

  Result.CopyFrom(Brotli,Brotli.Size);
  TXDataOperationContext.Current.Response.Headers.SetValue('content-length',IntToSTr(Length(Response.Text)));
  TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length',IntToStr(Brotli.Size));
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  NotBrotli.Free;
  Brotli.Free;
  Response.Free;
  LookupData.Free;
end;

function TActorInfoService.MovieReleaseDay(Secret: String; aMonth, aDay: Integer; Progress: String): TStream;
var
  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  cacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  data: TJSONObject;                          // Response converted to JSON
  Movies: TJSONArray;
  sortedMovies: TJSONArray;
  sortedMovies50: TJSONArray;
  req: String;

  Reply: String;

  Movieid: integer;  // counter for Movies
  MovieRef: String;   // TMDb for Movie
  MovieRefShort: String; // Last 3 chars used in cache file naming
  Movienew: String;


  i: integer;  // used for iterating list of roles (movies, tv shows)
  j: integer;  // used for iterating list of Releasedays from original Releaseday list

  dedupe: integer;      // Used to de-duplicate the top movies and tv shows.  Duplicates arise when
                        // the same Movie has many different roles on the same show. Which we want to
                        // see, just not in the 'top' section as they tend to crowd out everything else.
  dedupes: string;

  CacheFileDay: String;
  CacheFileDay50: String;
  CacheFilePerson: String;
  CacheFileReleases: String;

  Regenerate: Boolean;

begin
  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;

  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  CacheFileDay      := 'cache/days/actorious-releases/releaseday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFileDay50    := 'cache/days/actorious-releases/releaseday-'+RightStr('000'+IntToStr(CacheIndex),3)+'-50.json';
  CacheFileReleases := 'cache/days/wikidata-releases/releaseday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheResponse     := TStringList.Create;


  // Sometimes we want to force the regeneration of data, particularly after major structural changes
  if ((Pos('::1'           ,TXDataOperationContext.Current.Request.RemoteIP) > 0)  or
      (Pos('23.111.75.19'  ,TXDataOperationContext.Current.Request.RemoteIP) > 0))
     and MainForm.ckRegenerate.Checked
  then Regenerate := True
  else Regenerate := False;


  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"MovieReleaseDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');


  // Is there a Cached Response on disk?
  CacheBRResponse := TMemoryStream.Create;
  try
    CacheBRResponse.LoadFromFile(CacheFileDay+'.br');
  except on E: Exception do
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if ((CacheBRResponse.Size > 0) and not(Regenerate)) then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay+'.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay)));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

    // Cleanup
    CacheBRResponse.Free;
  end
  else
  begin
    // Cleanup for now
    CacheBRResponse.Free;

    // If the Releaseday cache doesn't exist, then get the ReleaseDays.
    // We need a list of TMDb IDs that comes from that last to generate a new extended list.
    try
      CacheResponse.Text := '';
      CacheResponse.LoadFromFile(CacheFileReleases, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"ReleaseDay Data Not Cached","TP":'+FloatToStr(Now)+'}';
      end;
    end;

    Movies := TJSONArray.Create;
    if ((CacheResponse.Text = '') or (Regenerate)) then
    begin
      // Get Updated Releaseday Data
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"WikiData","TP":'+FloatToStr(Now)+'}';
      ReleaseDay(Secret, aMonth, aDay, Progress);
      try
        CacheResponse.Text := '';
        CacheResponse.LoadFromFile(CacheFileReleases);
        Movies := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Current WikiData: '+IntToStr(Movies.count)+'People","TP":'+FloatToStr(Now)+'}';
      except on E: Exception do
        begin
          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"ReleaseDay Data STILL Not Cached","TP":'+FloatToStr(Now)+'}';
        end;
      end;
    end
    else
    begin
      Movies := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Prior WikiData: '+IntToStr(Movies.count)+'People","TP":'+FloatToStr(Now)+'}';
    end;

    if CacheResponse.Text = '' then
    begin
      // Well, we STILL don't have data, so let's give up for now
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"ReleaseDay Data Not Available","TP":'+FloatToStr(Now)+'}';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
      CacheResponse.Free;
      Exit;
    end
    else
    begin
      // Alright, we've somehow got a list of Releasedays but no data cached.
      // Let's go and get that data from TMDb

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing '+IntToStr(Movies.Count)+' Movies","TP":'+FloatToStr(Now)+'}';

      // Gonna build our own JSON string
      Reply := '[';
      MovieID := 1;


      for j := 0 to (Movies.Count - 1) do
      begin

        // This is the Movie we're looking at via their TMDB ID (should be an integer!!)
        MovieRef := Trim((((Movies.Items[j] as TJSONObject).getValue('TMDbID') as TJSONObject).GetValue('value') as TJSONString).Value);
        if (Pos(' ', MovieRef) > 0) then MovieRef := Copy(MovieRef,1,Pos(' ',MovieRef)-1);
        if (Pos('-', MovieRef) > 0) then MovieRef := Copy(MovieRef,1,Pos('-',MovieRef)-1);
        MovieRef := RightStr('00000000'+IntToStr(StrToIntDef(MovieRef,0)),8);
        MovieRefShort := RightStr(MovieRef,3);

        CacheFilePerson := 'cache/movies/actorious/'+MovieRefShort+'/movie-'+MovieRef+'.json';
        MovieNew := '';

        // For each person, we're looking for either a processed person file on disk (Movieious), in which case we're done with this person.
        // If we don't find that, we can look and swee if we have an unprocessed person file on disk (tmdb), in which case we'll process it.
        // Otherwise, we have to go and get the data from TMDB first.

        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Movie # '+MovieRef+' ( '+IntToStr(j+1)+' of '+IntToStr(Movies.Count)+' )","TP":'+FloatToStr(Now)+'}';

        // Do we have the processed data for this person already?
        try
          CacheResponse.Text := '';
          CacheResponse.LoadFromFile(CacheFilePerson, TEncoding.UTF8);
        except on E: Exception do
          begin
            MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Movie # '+MovieRef+' ( '+IntToStr(j+1)+' of '+IntToStr(Movies.Count)+' )","TP":'+FloatToStr(Now)+'}';
          end;
        end;

        // If we can't, or we're regenerating this data, then lets go and get it again
        if ((CacheResponse.Text = '') or (Regenerate)) then
        begin
          CacheFilePerson := 'cache/movies/tmdb/'+RightStr('00000000'+MovieRef,3)+'/movie-'+RightStr('00000000'+MovieRef,8)+'.json';

          // We don't have data
          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Retrieving Movie # '+MovieRef+' ( '+IntToStr(j+1)+' of '+IntToStr(Movies.Count)+' )","TP":'+FloatToStr(Now)+'}';
          CacheResponse.Text := GetMovieFromTMDb(StrToInt(MovieRef), False);

          if CacheResponse.Text = '' then
          begin
            TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
            CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
            CacheResponse.SaveToStream(Result);
            CacheResponse.Free;
            Exit;
          end
          else
          begin
            // Kind of unusual, but with random user-supplied data, perhaps not surprising, here we're
            // passing a quick filter over all of the JSON to get rid of a few random characters that can
            // mess things up - specifically TABS, LINEFEEDS and CARRIAGE RETURNS.  They don't really have
            // a place in JSON, and should be encoded in any place they might be needed.
            req := FilterResponse(CacheResponse.Text);

            try
              Data := TJSONObject.ParseJSONValue(req) as TJSONObject;
//              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Person Data Loaded ('+IntToStr(j+1)+' of '+IntToStr(Movies.Count)+'","TP":'+FloatToStr(Now)+'}';
            except on E: Exception do
              begin
                // Got a response that isn't valid JSON?!
                MainForm.LogEvent('Invalid TMDb Response received');
              end;
            end;

            if (Data <> nil)
              and (req <> '') // Make sure we have some data to work with
//              and ((Data.getValue('adult') = nil)  // no Movies without roles
//                   or ((Data.getValue('adult') as TJSONBool).asBoolean = False)) // family-friendly content only, please
            then
            begin
//              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data","TP":'+FloatToStr(Now)+'}';
              MovieNew := ProcessMovie(MovieID, MovieRef, Data, j, Movies, ProgressPrefix, ProgressKey);
            end;
            Data.Free;
          end;
        end
        else
        begin
          // We Do have data - this is already processed
          MovieNew := CacheResponse.Text
        end;

        // Resetting again
        CacheResponse.Text := '';

        if Movienew <> '' then
        begin
          if Movieid = 1
          then Reply := Reply + Movienew
          else Reply := Reply+','+Movienew;
          Movieid := Movieid + 1;
        end;

      end;

      Reply := Reply+']';
      Movies.Free;

      CacheResponse.Text := Reply;
      CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);

//      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Person Data Processed","TP":'+FloatToStr(Now)+'}';

      // Ok, lets sort the reply and change the ID to be the rank.

      Movies := TJSONObject.ParseJSONValue(Reply) as TJSONArray;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Final Prep Started","TP":'+FloatToStr(Now)+'}';

      CacheResponse.Text := '';
      for i := 0 to Movies.Count-1 do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Final Prep ( '+IntToStr(i)+' of '+IntToStr(Movies.Count-1)+' )","TP":'+FloatToStr(Now)+'}';
        CacheResponse.Add(
          FormatFloat('00000000.0000',1000000.0 - ((Movies.Items[i] as TJSONObject).GetValue('POP') as TJSONNumber).AsDouble)+
          RightStr('00000000'+((Movies.Items[i] as TJSONObject).GetValue('TID') as TJSONString).Value,8)+
          RightStr('00000000'+IntToSTr(i),8)
        );
      end;
      CacheResponse.Sort;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Indexed","TP":'+FloatToStr(Now)+'}';

      SortedMovies := TJSONArray.Create;
      SortedMovies50 := TJSONArray.Create;
      dedupes := '';
      dedupe := 0;
      for i := 0 to CacheResponse.Count - 1 do
      begin
        if Copy(CacheResponse[i],14,8) <> dedupes then
        begin
          dedupes := Copy(CacheResponse[i],14,8);
          dedupe := dedupe + 1;
          (Movies.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).addPair('ID',  TJSONNumber.Create(i+1));
          SortedMovies.AddElement(Movies.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);
        end;
      end;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing Results","TP":'+FloatToStr(Now)+'}';

      try
        CacheResponse.Text := SortedMovies.ToString;
        CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);

        // Save the response to disk as-is
        ResponseFile := TMemoryStream.Create;
        ResponseFile.LoadFromFile(CacheFileDay);
        ResponseFile.Seek(0, soFromBeginning);

        // Compress the stream with Brotli
        Brotli := TMemoryStream.Create;
        BrotliCompressStream(ResponseFile, Brotli, bcMax);
        Brotli.Seek(0, soFromBeginning);

        // Save the Brotli-compressed response to disk
        Brotli.SaveToFile(CacheFileDay+'.br');
        ResponseFile.Free;
        Brotli.Free;
        Movies.Free;
      except on E: Exception do
         begin
//          MainForm.LogEvent(E.ClassName+': '+E.Message);
         end;
      end;
    end;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Sending Results","TP":'+FloatToStr(Now)+'}';

    CacheBRResponse := TMemoryStream.Create;
    try
      CacheBRResponse.LoadFromFile(CacheFileDay+'.br');
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data STILL Not Cached?!","TP":'+FloatToStr(Now)+'}';
        CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
        TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
        CacheResponse.SaveToStream(Result);
      end;
    end;

    if CacheBRResponse.Size > 0 then
    begin
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay+'.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay)));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
      CacheBRResponse.Free;
    end
    else
    begin
      CacheResponse.Text := '[]';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
    end;
  end;

  CacheResponse.Free;

end;

function TActorInfoService.Relatives(Secret: String; RelatedTo: Integer; Progress: String): TStream;
var
  ClientReq: TNetHTTPClient;
  URL: String;
  qry: String;
  Data: TJSONArray;
  ProgressPrefix: String;
  ProgressKey: Integer;
  Actors: String;
  SearchResponse: String;
  ActorCount: Integer;
  Response: TStringList;
  CacheFile: String;
  i : Integer;
  ActorData: TJSONObject;
  ActorNum: String;
  NotBrotli: TMemoryStream;
  Brotli: TMemoryStream;

begin

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they send a valid search?
  if (RelatedTo <= 0) then raise EXDataHttpUnauthorized.Create('Search not Authorized');

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"Relatives"'+
                    ',"RT":"'+QuotedStr(IntToStr(RelatedTo))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');

  // Setup the client connection for TMDb
  ClientReq := TNetHTTPClient.Create(nil);
  ClientReq.ConnectionTimeout := 900000; // 15 minutes
  ClientReq.ResponseTimeout := 900000; // 15 minutes
  ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

  qry := MainForm.sparqlRelatives.Text;
  qry := StringReplace(qry,':PERSON','Q'+IntToStr(RelatedTo),[rfReplaceAll]);

  // Setup the URL and encode the query in it
  URL := TidURI.URLEncode('https://query.wikidata.org/sparql?query='+qry+'&format=json');
  Data := TJSONArray.Create;
  try
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Searching WikiData for Relatives","TP":'+FloatToStr(Now)+'}';

    try
      SearchResponse := ClientReq.Get(URL).ContentAsString(TEncoding.UTF8);
    except on E:Exception do
      begin
        MainForm.LogEvent('EXCEPTION in WikiData Relatives ClientReq.Get');
        MainForm.LogEvent(E.Classname+': '+E.Message);
      end;
    end;
//    MainForm.LogEvent(qry);
//    MainForm.LogEvent(SearchResponse);

    Data := ((TJSONObject.ParseJSONValue(SearchResponse) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
//    MainForm.LogEvent('Searching for '+IntToStr(Data.Count)+' Relatives');
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Search Results: '+IntToStr(Data.Count)+' hits","TP":'+FloatToStr(Now)+'}';

  except on E: Exception do
    begin
      MainForm.LogEvent('EXCEPTION in WikiData Relatives Results Processing');
      MainForm.LogEvent(E.Classname+': '+E.Message);
    end;
  end;

  Response := TStringList.Create;

  Actors := '[';
  ActorCount := 1;
  for i := 0 to Data.Count - 1 do
  begin
    Response.Text := '';
    ActorNum := RightStr('0000000'+(((data.items[i] as TJSONObject).getValue('TMDB') as TJSONObject).getValue('value') as TJSONString).value,8);
//      MainForm.LogEvent('Searching for #'+IntToStr(i)+' Relative: '+ActorNum);
    CacheFile := 'cache/people/actorious/'+
      RightStr(ActorNum,3)+
      '/person-'+
      ActorNum+
      '.json';
    try
      Response.Text := '';
      Response.LoadFromFile(CacheFile, TEncoding.UTF8)
    except on E: Exception do
      begin
        Response.Text := GetPersonfromTMDb(StrToInt(ActorNum), False);
        ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
        ProcessActor(ActorCount, ActorNum, ActorData.ToString, -1, '[]', ProgressPrefix, ProgressKey, False);
        ActorData.Free;
        Response.LoadFromFile('cache/people/actorious/'+
          RightStr(ActorNum,3)+
          '/person-'+
          ActorNum+
          '.json', TEncoding.UTF8);
      end;
    end;


    if (Response.Text <> '')  then
    begin
      ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
      ActorData.RemovePair('ID');
      ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(ActorCount)));

      if ActorCount = 1
      then Actors := Actors+ActorData.ToString
      else Actors := Actors+','+ActorData.ToString;
      ActorCount := ActorCount + 1;

      ActorData.Free;
    end;
  end;
  Actors := Actors +']';

  Response.Text := Actors;

  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Relatives Results for '+QuotedStr(IntToStr(RelatedTo))+': '+IntToStr(ActorCount)+' Matches Found","TP":'+FloatToStr(Now)+'}';

  // This is what we're sending back
  NotBrotli := TMemoryStream.Create;
  Response.SaveToStream(NotBrotli);
  NotBrotli.Seek(0, soFromBeginning);

  // Compress the stream with Brotli
  Brotli := TMemoryStream.Create;
  BrotliCompressStream(NotBrotli, Brotli, bcGood);
  Brotli.Seek(0, soFromBeginning);

  Result.CopyFrom(Brotli,Brotli.Size);
  TXDataOperationContext.Current.Response.Headers.SetValue('content-length',IntToSTr(Length(Response.Text)));
  TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length',IntToStr(Brotli.Size));
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  NotBrotli.Free;
  Brotli.Free;
  Response.Free;
  ClientReq.Free;
  Data.Free;
end;

function TActorInfoService.ReleaseDay(Secret: String; aMonth, aDay: Integer; Progress: String): TStream;
var
  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  cacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ClientReq: TNetHTTPClient;        // Used to connect to Wikidata
  SPARQL: String;                   // The SPARQL query being passed to Wikidata
  URL: String;                      // Wikidata SPARQL endpoint URL

  Response: TStream;                // Response from Wikidata
  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  CacheFile: String;                // Where the cache for this execution is stored
begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  CacheFile := 'cache/days/wikidata-releases/releaseday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"ReleaseDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');

  // Is there a Cached Response on disk?
  CacheBRResponse := TMemoryStream.Create;
  try
    CacheBRResponse.LoadFromFile(CacheFile+'.br');
  except on E: Exception do
    begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
//      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"WikiData Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if (CacheBRResponse.Size > 0) and (Progress <> MainForm.CurrentProgress.Caption) then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile)));
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
    CacheBRResponse.Free;

  end
  else
  begin
    CacheBRResponse.Free;

    // We don't have data, so lets try and get some
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Contacting Wikidata","TP":'+FloatToStr(Now)+'}';

    // Setup the client connection
    ClientReq := TNetHTTPClient.Create(nil);
    ClientReq.ConnectionTimeout := 900000; // 15 minutes
    ClientReq.ResponseTimeout := 900000; // 15 minutes
    ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    // Get the query from the MainForm, apply parameters
    SPARQL := MainForm.sparqlReleaseDays.Lines.Text;
    SPARQL := StringReplace(StringReplace(SPARQL, ':MONTH', IntToStr(aMonth), [rfReplaceAll]), ':DAY', IntToStr(aDay), [rfReplaceAll]);

    // Setup the URL and encode the query in it
    URL := TidURI.URLEncode('https://query.wikidata.org/sparql?query='+SPARQL+'&format=json');
    try
      Response := ClientReq.Get(URL).ContentStream;

      // Save the response to disk as-is
      ResponseFile := TMemoryStream.Create;
      ResponseFile.CopyFrom(Response,Response.size);
      ResponseFile.SaveToFile(CacheFile);
      ResponseFile.Seek(0, soFromBeginning);

      // Compress the stream with Brotli
      Brotli := TMemoryStream.Create;
      BrotliCompressStream(ResponseFile, Brotli, bcMax);
      Brotli.Seek(0, soFromBeginning);

      // Save the Brotli-compressed response to disk
      Brotli.SaveToFile(CacheFile+'.br');
      ResponseFile.Free;
      Brotli.Free;
    except on E: Exception do
      begin
//        MainForm.LogEvent(E.ClassName+': '+E.Message);
      end;
    end;

    // Now check again - Is there a Cached Response on disk?
    CacheBRResponse := TMemoryStream.Create;
    try
      CacheBRResponse.LoadFromFile(CacheFile+'.br');
    except on E: Exception do
      begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
      end;
    end;

    if CacheBRResponse.Size > 0 then
    begin
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile)));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
      CacheBRResponse.Free;
    end
    else
    begin
      // Well, we STILL don't have data, so let's give up for now
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Available","TP":'+FloatToStr(Now)+'}';
      CacheResponse := TStringList.Create;
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
    end;

    ClientReq.Free;
  end;

//  MainForm.LogEvent(MainForm.Progress[ProgressKey]);
end;


function TActorInfoService.SearchPeople(Secret, SearchTerm, Progress: String): TStream;
var
  ClientReq: TNetHTTPClient;
  URL: String;
  qry: String;
  Data: TJSONArray;
  ProgressPrefix: String;
  ProgressKey: Integer;
  Actors: String;
  SearchResponse: String;
  ActorCount: Integer;
  Response: TStringList;
  CacheFile: String;
  i : Integer;
  ActorData: TJSONObject;
  ActorNum: String;
  NotBrotli: TMemoryStream;
  Brotli: TMemoryStream;

begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they send a valid search?
  if (Length(Trim(SearchTerm)) < 3) or (Length(Trim(SearchTerm)) > 25) then raise EXDataHttpUnauthorized.Create('Search not Authorized');


  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"SearchPerson"'+
                    ',"TM":"'+QuotedStr(SearchTerm)+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');

  // Setup the client connection for TMDb
  ClientReq := TNetHTTPClient.Create(nil);
  ClientReq.ConnectionTimeout := 900000; // 15 minutes
  ClientReq.ResponseTimeout := 900000; // 15 minutes
  ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

  qry := 'search/person/';
  qry := qry+'?api_key='+MainForm.edTMDbAPI.Text;
  qry := qry+'&language=en-US';
  qry := qry+'&page=1';
  qry := qry+'&include_adult=true';
  qry := qry+'&query='+trim(SearchTerm);

  // Setup the URL and encode the query in it
  URL := TidURI.URLEncode('https://api.themoviedb.org/3/'+qry);
  Data := TJSONArray.Create;
  try
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Searching","TP":'+FloatToStr(Now)+'}';

    try
      SearchResponse := ClientReq.Get(URL).ContentAsString(tencoding.UTF8);
    except on E:Exception do
      begin
        MainForm.LogEvent('EXCEPTION in SearchPerson TMDb ClientReq.Get');
        MainForm.LogEvent(E.Classname+': '+E.Message);
      end;
    end;

    Data := (TJSONObject.ParseJSONValue(SearchResponse) as TJSONObject).getValue('results') as TJSONArray;
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Search Results: '+IntToStr(Data.Count)+' hits","TP":'+FloatToStr(Now)+'}';

  except on E: Exception do
    begin
      MainForm.LogEvent('EXCEPTION in SearchPerson Results Processing');
      MainForm.LogEvent(E.Classname+': '+E.Message);
    end;
  end;

  Response := TStringList.Create;

  Actors := '[';
  ActorCount := 1;
  for i := 0 to Data.Count - 1 do
  begin
    ActorNum := RightStr('0000000'+IntToStr(((data.items[i] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8);
    CacheFile := 'cache/people/actorious/'+
      RightStr(ActorNum,3)+
      '/person-'+
      ActorNum+
      '.json';
    try
      Response.Text := '';
      Response.LoadFromFile(CacheFile, TEncoding.UTF8)
    except on E: Exception do
      begin
        // In QuickSearch, we're just returning cached values so we won't do this step.
        // That's what makes it Quick, after all, in addition to only searching one page
        // of results from TMDb
//        UpdatePersonfromTMDb(StrToInt(ActorNum), False);
//        Response.LoadFromFile('cache/people/tmdb/'+
//        RightStr(ActorNum,3)+
//          '/person-'+
//          ActorNum+
//          '.json');
//        ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
//        ProcessActor(ActorCount, ActorNum, ActorData, -1, nil, ProgressPrefix, ProgressKey);
//        ActorData.Free;
//        Response.LoadFromFile('cache/people/actorious/'+
//          RightStr(ActorNum,3)+
//          '/person-'+
//          ActorNum+
//          '.json');
      end;
    end;

    if (Response.Text <> '')  then
    begin
      ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
      ActorData.RemovePair('ID');
      ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(ActorCount)));

      if ActorCount = 1
      then Actors := Actors+ActorData.ToString
      else Actors := Actors+','+ActorData.ToString;
      ActorCount := ActorCount + 1;

      ActorData.Free;
    end;
  end;
  Actors := Actors +']';

  Response.Text := Actors;

  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Search Results for '+QuotedStr(SearchTerm)+': '+IntToStr(ActorCount)+' Matches Found","TP":'+FloatToStr(Now)+'}';

  // This is what we're sending back
  NotBrotli := TMemoryStream.Create;
  Response.SaveToStream(NotBrotli);
  NotBrotli.Seek(0, soFromBeginning);

  // Compress the stream with Brotli
  Brotli := TMemoryStream.Create;
  BrotliCompressStream(NotBrotli, Brotli, bcGood);
  Brotli.Seek(0, soFromBeginning);

  Result.CopyFrom(Brotli,Brotli.Size);
  TXDataOperationContext.Current.Response.Headers.SetValue('content-length',IntToSTr(Length(Response.Text)));
  TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length',IntToStr(Brotli.Size));
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  NotBrotli.Free;
  Brotli.Free;
  Response.Free;
  ClientReq.Free;
  Data.Free;
end;


function TActorInfoService.SearchPeopleExtended(Secret, SearchTerm, Progress: String): TStream;
var
  ClientReq: TNetHTTPClient;
  URL: String;
  qry: String;
  Data: TJSONArray;
  Page: Integer;
  ProgressPrefix: String;
  ProgressKey: Integer;
  Actors: String;
  SearchResponse: String;
  ActorCount: Integer;
  Response: TStringList;
  CacheFile: String;
  i : Integer;
  ActorData: TJSONObject;
  ActorNum: String;
  NotBrotli: TMemoryStream;
  Brotli: TMemoryStream;
  PagesAvail: Integer;
begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they send a valid search?
  if (Length(Trim(SearchTerm)) < 3) or (Length(Trim(SearchTerm)) > 25) then raise EXDataHttpUnauthorized.Create('Search not Authorized');


  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"SearchPerson"'+
                    ',"TM":"'+QuotedStr(SearchTerm)+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');



  Page := 1;
  PagesAvail := 1;
  ActorCount := 1;
  Actors := '[';
  Response := TStringList.Create;

  // Setup the client connection for TMDb
  ClientReq := TNetHTTPClient.Create(nil);
  ClientReq.ConnectionTimeout := 900000; // 15 minutes
  ClientReq.ResponseTimeout := 900000; // 15 minutes
  ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

  while Page <= PagesAvail  do
  begin

    qry := 'search/person/';
    qry := qry+'?api_key='+MainForm.edTMDbAPI.Text;
    qry := qry+'&language=en-US';
    qry := qry+'*&page='+IntToStr(Page);
    qry := qry+'&include_adult=true';
    qry := qry+'&query='+trim(SearchTerm);

//    MainForm.LogEvent('Searching Page '+IntToStr(Page)+' of '+IntToStr(PagesAvail));

    // Setup the URL and encode the query in it
    URL := TidURI.URLEncode('https://api.themoviedb.org/3/'+qry);
    Data := TJSONArray.Create;
    try
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Searching","TP":'+FloatToStr(Now)+'}';

      try
        SearchResponse := ClientReq.Get(URL).ContentAsString(tencoding.UTF8);
      except on E:Exception do
        begin
          MainForm.LogEvent('EXCEPTION in SearchPerson TMDb ClientReq.Get');
          MainForm.LogEvent(E.Classname+': '+E.Message);
        end;
      end;

      Data := (TJSONObject.ParseJSONValue(SearchResponse) as TJSONObject).getValue('results') as TJSONArray;
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Search Results: Page '+IntToStr(Page)+'","TP":'+FloatToStr(Now)+'}';

      if Page = 1 then
      begin
        ActorData := TJSONObject.ParseJSONValue(SearchResponse) as TJSONObject;
        PagesAvail := Min(5,(ActorData.getValue('total_pages') as TJSONNumber).asInt);
        ActorData.Free;
      end;
      Page := Page + 1;

    except on E: Exception do
      begin
        MainForm.LogEvent('EXCEPTION in SearchPerson Results Processing');
        MainForm.LogEvent(E.Classname+': '+E.Message);
      end;
    end;

    for i := 0 to Data.Count - 1 do
    begin
      ActorNum := RightStr('0000000'+IntToStr(((data.items[i] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8);
      CacheFile := 'cache/people/actorious/'+
        RightStr(ActorNum,3)+
        '/person-'+
        ActorNum+
        '.json';
      try
        Response.Text := '';
        Response.LoadFromFile(CacheFile, TEncoding.UTF8)
      except on E: Exception do
        begin
          Response.Text := GetPersonfromTMDb(StrToInt(ActorNum), False);
          try
            ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
            ProcessActor(ActorCount, ActorNum, ActorData.ToString, -1, '[]', ProgressPrefix, ProgressKey, False);
            ActorData.Free;
            Response.Text := '';
            Response.LoadFromFile('cache/people/actorious/'+
              RightStr(ActorNum,3)+
              '/person-'+
              ActorNum+
              '.json', TEncoding.UTF8);
          except on E: Exception do
            begin
              // We didn't get a result?
            end
          end;
        end;
      end;

      if (Response.Text <> '')  then
      begin
        try
          ActorData := TJSONObject.ParseJSONValue(Response.Text) as TJSONObject;
          ActorData.RemovePair('ID');
          ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(ActorCount)));

          if ActorCount = 1
          then Actors := Actors+ActorData.ToString
          else Actors := Actors+','+ActorData.ToString;
          ActorCount := ActorCount + 1;

          ActorData.Free;
        except on E: Exception do
          begin
          end;
        end;
      end;
    end;

    Data.Free;
  end;
    ClientReq.Free;

  Actors := Actors +']';
  Response.Text := Actors;

  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Extended Search Results for '+QuotedStr(SearchTerm)+': '+IntToStr(ActorCount)+' Matches Found","TP":'+FloatToStr(Now)+'}';

  // This is what we're sending back
  NotBrotli := TMemoryStream.Create;
  Response.SaveToStream(NotBrotli);
  NotBrotli.Seek(0, soFromBeginning);

  // Compress the stream with Brotli
  Brotli := TMemoryStream.Create;
  BrotliCompressStream(NotBrotli, Brotli, bcGood);
  Brotli.Seek(0, soFromBeginning);

  Result.CopyFrom(Brotli,Brotli.Size);
  TXDataOperationContext.Current.Response.Headers.SetValue('content-length',IntToSTr(Length(Response.Text)));
  TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length',IntToStr(Brotli.Size));
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  NotBrotli.Free;
  Brotli.Free;
  Response.Free;
end;

function TActorInfoService.ActorBirthDay(Secret: String; aMonth, aDay: Integer; Progress:String): TStream;
var
  Regenerate: Boolean;              // Flag whether cached data can be used

  SPARQL: String;                   // The query sent to WikiData

  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  CacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  Actors: TJSONArray;               // First the list from Wikidata, then the complete processed list
  SortedActors: TJSONArray;         // Actors sroted by popularity
  SortedActors50: TJSONArray;       // Abbreviated list of Actors sorted by popularity

  Reply: String;                    // The generated response JSON

  ActorID: integer;                 // Counter for actors
  ActorRef: String;                 // TMDb for Actor
  ActorRefShort: String;            // Last 3 chars used in cache file naming
  ActorNew: String;                 // Newly generated actor

  i: integer;                       // used for iterating list of roles (movies, tv shows)
  j: integer;                       // used for iterating list of birthdays from original birthday list

  dedupe: integer;                  // Used to de-duplicate JSON data
  dedupes: string;                  // Used to de-duplicate JSON data

  CacheFileDay:     String;  // The final results
  CacheFileDayBR:   String;  // The final results, compressed with Brotli
  CacheFileDay50:   String;  // The abbreviated results
  CacheFileDay50BR: String;  // The abbreviated results, compressed with Brotli
  CacheFileWiki:    String;  // The Wikidata results
  CacheFilePerson:  String;  // The person results

  FirstCache: TStringList;  // Used to generate the first photo cache
  TodayCache: TStringList;  // Used to generate the TopToday cache
  TodayCount: Integer;
  FirstActor: Boolean;

begin
  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  Result := TMemoryStream.Create;
  CacheBRResponse := TMemoryStream.Create;


  // Sometimes we want to force the regeneration of data, particularly after major structural changes
  if ((Pos('::1'           ,TXDataOperationContext.Current.Request.RemoteIP) > 0)  or
      (Pos('23.111.75.19'  ,TXDataOperationContext.Current.Request.RemoteIP) > 0))
     and MainForm.ckRegenerate.Checked
  then Regenerate := True
  else Regenerate := False;


  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  SetBrotliHeaders;

  // We'll be referencing a number of files, so let's identify them now.
  CacheFileDay     := 'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFileDayBR   := 'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json.br';
  CacheFileDay50   := 'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'-50.json';
  CacheFileDay50BR := 'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'-50.json.br';
  CacheFileWiki    := 'cache/days/wikidata-births/birthday-' +RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFilePerson  := 'cache/days/people/';

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"ActorBirthDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+',"PR":"Initialized","TP":'+FloatToStr(Now)+'}');


  // Is there a Cached Response on disk?
  if FileExists(CacheFileDayBR)
  then CacheBRResponse.LoadFromFile(CacheFileDayBR);

  // If we've got data and it isn't empty then send it and be done
  if ((CacheBRResponse.Size > 0) and not(Regenerate)) then
  begin
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    CacheBRResponse.Free;

    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDayBR)));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay)));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete (Cached)","TP":'+FloatToStr(Now)+'}';

    exit;
  end;


  // Otherwise, we've got to go and create the data
  CacheResponse := TStringList.Create;


  // Get Birthdays from Wikidata
  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Retrieving Birthdays from WikiData","TP":'+FloatToStr(Now)+'}';
  SPARQL := MainForm.sparqlBirthDays.Lines.Text;
  SPARQL := StringReplace(StringReplace(SPARQL, ':MONTH', IntToStr(aMonth), [rfReplaceAll]), ':DAY', IntToStr(aDay), [rfReplaceAll]);
  SPARQL := TidURI.URLEncode('https://query.wikidata.org/sparql?query='+SPARQL+'&format=json');
  CacheResponse.Text := GetDataFromWikiData(SPARQL,CacheFileWiki);

  Actors := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing '+IntToStr(Actors.Count)+' Actors","TP":'+FloatToStr(Now)+'}';

  // Gonna build our own JSON string
  Reply := '[';
  ActorID := 1;

//  for j := 0 to 15 do
  for j := 0 to (Actors.Count - 1) do
  begin

    // This is the actor we're looking at via their TMDB ID (should be an integer!!)
    ActorRef := Trim((((Actors.Items[j] as TJSONObject).getValue('TMDbID') as TJSONObject).GetValue('value') as TJSONString).Value);
    if (Pos(' ', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos(' ',ActorRef)-1);
    if (Pos('-', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos('-',ActorRef)-1);
    ActorRef := RightStr('00000000'+IntToStr(StrToIntDef(ActorRef,0)),8);
    ActorRefShort := RightStr(ActorRef,3);


    // For each person, we're looking for either a processed person file on disk (actorious), in which case we're done with this person.
    // If we don't find that, we can look and swee if we have an unprocessed person file on disk (tmdb), in which case we'll process it.
    // Otherwise, we have to go and get the data from TMDB first.
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing TMDb #'+ActorRef+' ( '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+' )","TP":'+FloatToStr(Now)+'}';

    // Get the Actor data
    ActorNew := GetPerson(ActorID, Actors.Count, ActorRef, ActorRefShort, (Actors.Items[j] as TJSONObject).ToString, ProgressPrefix, ProgressKey, Regenerate);

    // If it returns an Actor, then add it to the response.
    // Might not return data if Actor could not be found, or data was otherwise unavailable for some reason
    if ActorNew <> '' then
    begin
      if ActorID = 1
      then Reply := Reply + ActorNew
      else Reply := Reply+','+ActorNew;
      ActorID := ActorID + 1;
    end;

  end;
  Actors.Free;

  // Save our initial results
  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Saving Results","TP":'+FloatToStr(Now)+'}';

  Reply := Reply+']';
  CacheResponse.Text := Reply;
  CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);
  Actors := TJSONObject.ParseJSONValue(Reply) as TJSONArray;

  // Ok, lets sort the reply and change the ID to be the rank.

  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Final Prep Started","TP":'+FloatToStr(Now)+'}';

  CacheResponse.Text := '';
  for i := 0 to Actors.Count-1 do
  begin
    CacheResponse.Add(
      FormatFloat('00000000.0000',1000000.0 - ((Actors.Items[i] as TJSONObject).GetValue('POP') as TJSONNumber).AsDouble)+
      RightStr('00000000'+((Actors.Items[i] as TJSONObject).GetValue('TID') as TJSONString).Value,8)+
      RightStr('00000000'+IntToSTr(i),8)
    );
  end;
  CacheResponse.Sort;

  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Indexed","TP":'+FloatToStr(Now)+'}';

  SortedActors := TJSONArray.Create;
  SortedActors50 := TJSONArray.Create;
  dedupes := '';
  dedupe := 0;
  FirstActor := True;
  TodayCount := 0;
  for i := 0 to CacheResponse.Count - 1 do
  begin
    if Copy(CacheResponse[i],14,8) <> dedupes then
    begin
      dedupes := Copy(CacheResponse[i],14,8);
      (Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).addPair('ID',  TJSONNumber.Create(i+1));

      SortedActors.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);

      if (((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('XXX') as TJSONBool).AsBoolean = False)
      then dedupe := dedupe + 1;

      if (dedupe <= 50)
      then SortedActors50.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);

      // Write the very first PIC for this date out to use as the cached image - but only if it isn't an adult actor
      if (FirstActor and (((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('XXX') as TJSONBool).AsBoolean = False)) then
      begin
        FirstActor := False;
        FirstCache := TStringList.Create;
        try
          FirstCache.Text := '"DAT":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss',Now)+'"';

          if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') <> nil) and
             ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') is TJSONString)
          then FirstCache.Text := FirstCache.Text + ',"PIC":"'+((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') as TJSONString).Value+'"';

          if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('NAM') <> nil) and
             ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('NAM') is TJSONString)
          then FirstCache.Text := FirstCache.Text + ',"NAM":'+REST.JSON.TJSON.JSONEncode((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('NAM') as TJSONString);

          if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('BIO') <> nil) and
             ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('BIO') is TJSONString)
          then FirstCache.Text := FirstCache.Text + ',"BIO":'+REST.JSON.TJSON.JSONEncode((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('BIO') as TJSONString);

          if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') <> nil) and
             ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') is TJSONString)
          then FirstCache.Text := FirstCache.Text + ',"IMG":"'+GetImageURI(((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') as TJSONString).Value)+'"';

          if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('IMG') <> nil) and
             ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('IMG') is TJSONArray)
          then FirstCache.Text := FirstCache.Text + ',"NUM":'+IntToStr(((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('IMG') as TJSONArray).Count);

          FirstCache.SaveToFile('cache/days/first/'+FormatDateTime('mmdd',encodedate(2020,aMonth,aDay))+'.json', TEncoding.UTF8);

        except on E: Exception do
          begin
            MainForm.LogEvent('EXCEPTION in Creating First Entry:');
            MainForm.LogEvent('[ '+E.ClassName+' ] '+E.Message);
            MainForm.LogEvent('[ File ] '+'cache/days/first/'+FormatDateTime('mmdd',encodedate(2020,aMonth,aDay))+'.json');
            MainForm.LogEvent('[ Data ] '+FirstCache.Text);
          end;
        end;
        FirstCache.Free;
      end;

      // Generate the TopToday List
      if ((TodayCount < 30) and (((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('XXX') as TJSONBool).AsBoolean = False)) then
      begin
        if  TodayCount = 0 then
        begin
         TodayCache := TStringList.Create;
         TodayCache.Add('[');
        end;

        TodayCache.Add('{"DAT":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss',Now)+'"');

        TodayCache.Add(',"ORD":'+IntToStr(TodayCount+1));

        if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('TID') <> nil) and
           ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('TID') is TJSONString)
        then TodayCache.Add(',"TID":'+REST.JSON.TJSON.JSONEncode((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('TID') as TJSONString));

        if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('NAM') <> nil) and
           ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('NAM') is TJSONString)
        then TodayCache.Add(',"NAM":'+REST.JSON.TJSON.JSONEncode((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('NAM') as TJSONString));

        if ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') <> nil) and
           ((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') is TJSONString)
        then TodayCache.Add(',"PIC":"'+((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('PIC') as TJSONString).Value+'"');

        todayCount := TodayCount + 1;

        if TodayCount < 30 then
        begin
          todayCache.Add('},')
        end
        else
        begin
          TodayCache.Add('}]');
          TodayCache.SaveToFile('cache/days/toptoday/day-'+FormatDateTime('mmdd',encodedate(2020,aMonth,aDay))+'.json', TEncoding.UTF8);
          TodayCache.Free;
        end;
      end;

    end;
  end;


  try
    CacheResponse.Text := SortedActors.ToString;
    CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);

    // Save the response to disk as-is
    ResponseFile := TMemoryStream.Create;
    ResponseFile.LoadFromFile(CacheFileDay);
    ResponseFile.Seek(0, soFromBeginning);

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing ( Full File )","TP":'+FloatToStr(Now)+'}';

    // Compress the stream with Brotli
    Brotli := TMemoryStream.Create;
    BrotliCompressStream(ResponseFile, Brotli, bcBetter);
    Brotli.Seek(0, soFromBeginning);

    // Save the Brotli-compressed response to disk
    Brotli.SaveToFile(CacheFileDayBR);
    ResponseFile.Free;
    Brotli.Free;

    // Now lets do it all again for the 50 version
    CacheResponse.Text := SortedActors50.ToString;
    CacheResponse.SaveToFile(CacheFileDay50, TEncoding.UTF8);

    // Save the response to disk as-is
    ResponseFile := TMemoryStream.Create;
    ResponseFile.LoadFromFile(CacheFileDay50);
    ResponseFile.Seek(0, soFromBeginning);

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing ( Abbreviated File )","TP":'+FloatToStr(Now)+'}';

    // Compress the stream with Brotli
    Brotli := TMemoryStream.Create;
    BrotliCompressStream(ResponseFile, Brotli, bcMax);
    Brotli.Seek(0, soFromBeginning);

    // Save the Brotli-compressed response to disk
    Brotli.SaveToFile(CacheFileDay50BR);
    ResponseFile.Free;
    Brotli.Free;

  except on E: Exception do
    begin
      MainForm.LogEvent('EXCEPTION in Compressing Results:');
      MainForm.LogEvent('[ '+E.ClassName+' ] '+E.Message);
      MainForm.LogEvent('[ File ] '+CacheFileDay);
    end;
  end;

  try
//    SortedActors.Free;
//    SortedActors50.Free;
    Actors.Free;
  except on E: Exception do
    begin
      MainForm.LogEvent('EXCEPTION in Compression Cleanup:');
      MainForm.LogEvent('[ '+E.ClassName+' ] '+E.Message);
      MainForm.LogEvent('[ File ] '+CacheFileDay);
    end;
  end;


  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Sending Results","TP":'+FloatToStr(Now)+'}';

  try
    CacheBRResponse.LoadFromFile(CacheFileDayBR);
  except on E: Exception do
    begin
      CacheResponse.Text := '[{"ERR":"Data Not Unavailable"}]';
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
      CacheResponse.Free;

      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Failed","TP":'+FloatToStr(Now)+'}';

      MainForm.LogEvent('EXCEPTION in Loading Results:');
      MainForm.LogEvent('[ '+E.ClassName+' ] '+E.Message);
      MainForm.LogEvent('[ File ] '+CacheFileDayBR);

      exit;
    end;
  end;

  if CacheBRResponse.Size > 0 then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    CacheBRResponse.Free;
    CacheResponse.Free;

    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDayBR)));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay)));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers', 'x-uncompressed-content-length');

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
  end
  else
  begin
    // We don't have data
    CacheResponse.Text := '[]';
    CacheResponse.SaveToStream(Result);
    CacheBRResponse.Free;
    CacheResponse.Free;

    TXDataOperationContext.Current.Response.Headers.remove('content-encoding');

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Failed","TP":'+FloatToStr(Now)+'}';
  end;
end;


function TActorInfoService.ActorDeathDay(Secret: String; aMonth, aDay: Integer; Progress:String): TStream;
var
  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  cacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  data: TJSONObject;                          // Response converted to JSON
  actors: TJSONArray;
  sortedactors: TJSONArray;
  sortedactors50: TJSONArray;
  req: String;

  Reply: String;

  actorid: integer;  // counter for actors
  ActorRef: String;   // TMDb for Actor
  ActorRefShort: String; // Last 3 chars used in cache file naming
  actornew: String;


  i: integer;  // used for iterating list of roles (movies, tv shows)
  j: integer;  // used for iterating list of Deathdays from original Deathday list

  dedupe: integer;      // Used to de-duplicate the top movies and tv shows.  Duplicates arise when
                        // the same actor has many different roles on the same show. Which we want to
                        // see, just not in the 'top' section as they tend to crowd out everything else.
  dedupes: string;

  CacheFileDay: String;
  CacheFileDay50: String;
  CacheFilePerson: String;
  CacheFileDeaths: String;

  Regenerate: Boolean;

begin
  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;

  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  CacheFileDay    := 'cache/days/actorious-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFileDay50  := 'cache/days/actorious-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'-50.json';
  CacheFileDeaths := 'cache/days/wikidata-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFilePerson := 'cache/days/people/';
  CacheResponse := TStringList.Create;


  // Sometimes we want to force the regeneration of data, particularly after major structural changes
  if ((Pos('::1'           ,TXDataOperationContext.Current.Request.RemoteIP) > 0)  or
      (Pos('23.111.75.19'  ,TXDataOperationContext.Current.Request.RemoteIP) > 0))
     and MainForm.ckRegenerate.Checked
  then Regenerate := True
  else Regenerate := False;


  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"ActorDeathDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');


  // Is there a Cached Response on disk?
  CacheBRResponse := TMemoryStream.Create;
  try
    CacheBRResponse.LoadFromFile(CacheFileDay+'.br');
  except on E: Exception do
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if ((CacheBRResponse.Size > 0) and not(Regenerate)) then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay+'.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay)));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

    // Cleanup
    CacheBRResponse.Free;
  end
  else
  begin
    // Cleanup for now
    CacheBRResponse.Free;


    Actors := TJSONArray.Create;

    // If the Deathday cache doesn't exist, then get the Deathdays.
    // We need a list of TMDb IDs that comes from that last to generate a new extended list.
    try
      CacheResponse.Text := '';
      CacheResponse.LoadFromFile(CacheFileDeaths, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"DeathDay Data Not Cached","TP":'+FloatToStr(Now)+'}';
      end;
    end;

    if ((CacheResponse.Text = '') or (Regenerate)) then
    begin
      // Get Updated Deathday Data
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"WikiData","TP":'+FloatToStr(Now)+'}';
      DeathDay(Secret, aMonth, aDay, Progress);
      try
        CacheResponse.Text := '';
        CacheResponse.LoadFromFile(CacheFileDeaths, TEncoding.UTF8);
        Actors := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Current WikiData: '+IntToStr(Actors.count)+'People","TP":'+FloatToStr(Now)+'}';
      except on E: Exception do
        begin
          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"DeathDay Data STILL Not Cached","TP":'+FloatToStr(Now)+'}';
        end;
      end;
    end
    else
    begin
      Actors := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Prior WikiData: '+IntToStr(Actors.count)+'People","TP":'+FloatToStr(Now)+'}';
    end;

    if CacheResponse.Text = '' then
    begin
      // Well, we STILL don't have data, so let's give up for now
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"DeathDay Data Not Available","TP":'+FloatToStr(Now)+'}';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
      CacheResponse.Free;
      Exit;
    end
    else
    begin
      // Alright, we've somehow got a list of Deathdays but no data cached.
      // Let's go and get that data from TMDb

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing '+IntToStr(Actors.Count)+' Actors","TP":'+FloatToStr(Now)+'}';

      // Gonna build our own JSON string
      Reply := '[';
      ActorID := 1;


      for j := 0 to (Actors.Count - 1) do
      begin

        // This is the actor we're looking at via their TMDB ID (should be an integer!!)
        ActorRef := Trim((((actors.Items[j] as TJSONObject).getValue('TMDbID') as TJSONObject).GetValue('value') as TJSONString).Value);
        if (Pos(' ', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos(' ',ActorRef)-1);
        if (Pos('-', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos('-',ActorRef)-1);
        ActorRef := RightStr('00000000'+IntToStr(StrToIntDef(ActorRef,0)),8);
        ActorRefShort := RightStr(ActorRef,3);

        CacheFilePerson := 'cache/people/actorious/'+ActorRefShort+'/person-'+ActorRef+'.json';
        ActorNew := '';

        // For each person, we're looking for either a processed person file on disk (actorious), in which case we're done with this person.
        // If we don't find that, we can look and swee if we have an unprocessed person file on disk (tmdb), in which case we'll process it.
        // Otherwise, we have to go and get the data from TMDB first.

        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person TMDb #'+ActorRef+' ( '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+' )","TP":'+FloatToStr(Now)+'}';

        // Do we have the processed data for this person already?
        try
          CacheResponse.Text := '';
          CacheResponse.LoadFromFile(CacheFilePerson, TEncoding.UTF8);
        except on E: Exception do
          begin
            MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person TMDb #'+ActorRef+' ( '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+' )","TP":'+FloatToStr(Now)+'}';
          end;
        end;

        // If we can't, or we're regenerating this data, then lets go and get it again
        if ((CacheResponse.Text = '') or (Regenerate)) then
        begin
          CacheFilePerson := 'cache/people/tmdb/'+RightStr('00000000'+ActorRef,3)+'/person-'+RightStr('00000000'+ActorRef,8)+'.json';

          // We don't have data
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Retrieving Person TMDb #'+ActorRef+' ( '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+' )","TP":'+FloatToStr(Now)+'}';
          CacheResponse.Text := GetPersonfromTMDb(StrToInt(ActorRef), False);

          if CacheResponse.Text = '' then
          begin
            TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
            CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
            CacheResponse.SaveToStream(Result);
            CacheResponse.Free;
            Exit;
          end
          else
          begin
            // Kind of unusual, but with random user-supplied data, perhaps not surprising, here we're
            // passing a quick filter over all of the JSON to get rid of a few random characters that can
            // mess things up - specifically TABS, LINEFEEDS and CARRIAGE RETURNS.  They don't really have
            // a place in JSON, and should be encoded in any place they might be needed.
            req := FilterResponse(CacheResponse.Text);

            try
              Data := TJSONObject.ParseJSONValue(req) as TJSONObject;
//              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Person Data Loaded ('+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+'","TP":'+FloatToStr(Now)+'}';
            except on E: Exception do
              begin
                // Got a response that isn't valid JSON?!
                MainForm.LogEvent('Invalid TMDb Response received');
              end;
            end;

            if (Data <> nil)
              and (req <> '') // Make sure we have some data to work with
//              and ((Data.getValue('adult') = nil)  // no actors without roles
//                   or ((Data.getValue('adult') as TJSONBool).asBoolean = False)) // family-friendly content only, please
              and (Data.getValue('combined_credits') <> nil)  // no actors without roles
              and ((((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Count) > 0) // no actors without roles
            then
            begin
//              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data","TP":'+FloatToStr(Now)+'}';
//              ActorNew := ProcessActor(ActorID, ActorRef, Data.ToString, j, Actors.ToString, ProgressPrefix, ProgressKey, Regenerate);
              ActorNew := ProcessActor(ActorID, ActorRef, Data.ToString, j, Actors.ToString, ProgressPrefix, ProgressKey, False);
            end;
            Data.Free;
          end;
        end
        else
        begin
          // We Do have data - this is already processed
          ActorNew := CacheResponse.Text
        end;

        // Resetting again
        CacheResponse.Text := '';

        if actornew <> '' then
        begin
          if actorid = 1
          then Reply := Reply + actornew
          else Reply := Reply+','+actornew;
          actorid := actorid + 1;
        end;

      end;

      Reply := Reply+']';
      Actors.Free;

      CacheResponse.Text := Reply;
      CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);

      // Ok, lets sort the reply and change the ID to be the rank.

      Actors := TJSONObject.ParseJSONValue(Reply) as TJSONArray;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Final Prep Started","TP":'+FloatToStr(Now)+'}';

      CacheResponse.Text := '';
      for i := 0 to Actors.Count-1 do
      begin
        CacheResponse.Add(
          FormatFloat('00000000.0000',1000000.0 - ((Actors.Items[i] as TJSONObject).GetValue('POP') as TJSONNumber).AsDouble)+
          RightStr('00000000'+((Actors.Items[i] as TJSONObject).GetValue('TID') as TJSONString).Value,8)+
          RightStr('00000000'+IntToSTr(i),8)
        );
      end;
      CacheResponse.Sort;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Indexed","TP":'+FloatToStr(Now)+'}';

      SortedActors := TJSONArray.Create;
      SortedActors50 := TJSONArray.Create;
      dedupes := '';
      dedupe := 0;
      for i := 0 to CacheResponse.Count - 1 do
      begin
        if Copy(CacheResponse[i],14,8) <> dedupes then
        begin
          dedupes := Copy(CacheResponse[i],14,8);
          (Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).addPair('ID',  TJSONNumber.Create(i+1));
          SortedActors.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);

          if (((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('XXX') as TJSONBool).AsBoolean = False)
          then dedupe := dedupe + 1;
          if dedupe <= 50 then SortedActors50.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);

        end;
      end;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing Results","TP":'+FloatToStr(Now)+'}';

      try
        CacheResponse.Text := SortedActors.ToString;
        CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);

        // Save the response to disk as-is
        ResponseFile := TMemoryStream.Create;
        ResponseFile.LoadFromFile(CacheFileDay);
        ResponseFile.Seek(0, soFromBeginning);

        // Compress the stream with Brotli
        Brotli := TMemoryStream.Create;
        BrotliCompressStream(ResponseFile, Brotli, bcBetter);
        Brotli.Seek(0, soFromBeginning);

        // Save the Brotli-compressed response to disk
        Brotli.SaveToFile(CacheFileDay+'.br');
        ResponseFile.Free;
        Brotli.Free;


        // Now lets do it all again for the 50 version
        CacheResponse.Text := SortedActors50.ToString;
        CacheResponse.SaveToFile(CacheFileDay50);

        Actors.Free;

        // Save the response to disk as-is
        ResponseFile := TMemoryStream.Create;
        ResponseFile.LoadFromFile(CacheFileDay50);
        ResponseFile.Seek(0, soFromBeginning);

        // Compress the stream with Brotli
        Brotli := TMemoryStream.Create;
        BrotliCompressStream(ResponseFile, Brotli, bcMax);
        Brotli.Seek(0, soFromBeginning);

        // Save the Brotli-compressed response to disk
        Brotli.SaveToFile(CacheFileDay50+'.br');
        ResponseFile.Free;
        Brotli.Free;

      except on E: Exception do
         begin
//          MainForm.LogEvent(E.ClassName+': '+E.Message);
         end;
      end;
    end;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Sending Results","TP":'+FloatToStr(Now)+'}';

    CacheBRResponse := TMemoryStream.Create;
    try
      CacheBRResponse.LoadFromFile(CacheFileDay+'.br');
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data STILL Not Cached?!","TP":'+FloatToStr(Now)+'}';
        CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
        TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
        CacheResponse.SaveToStream(Result);
      end;
    end;

    if CacheBRResponse.Size > 0 then
    begin
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay+'.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay)));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
      CacheBRResponse.Free;
    end
    else
    begin
      CacheResponse.Text := '[]';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
    end;
  end;

  CacheResponse.Free;
end;

function TActorInfoService.ActorBirthDay50(Secret: String; aMonth, aDay: Integer; Progress:String): TStream;
var
  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  cacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  data: TJSONObject;                          // Response converted to JSON
  actors: TJSONArray;
  sortedactors: TJSONArray;
  sortedactors50: TJSONArray;
  req: String;

  Reply: String;

  actorid: integer;  // counter for actors
  ActorRef: String;   // TMDb for Actor
  ActorRefShort: String; // Last 3 chars used in cache file naming
  actornew: String;


  i: integer;  // used for iterating list of roles (movies, tv shows)
  j: integer;  // used for iterating list of birthdays from original birthday list

  dedupe: integer;      // Used to de-duplicate the top movies and tv shows.  Duplicates arise when
                        // the same actor has many different roles on the same show. Which we want to
                        // see, just not in the 'top' section as they tend to crowd out everything else.
  dedupes: string;

  CacheFileDay: String;
  CacheFileDay50: String;
  CacheFilePerson: String;
  CacheFileBirths: String;

begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  CacheFileDay    := 'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFileDay50  := 'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'-50.json';
  CacheFileBirths := 'cache/days/wikidata-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFilePerson := 'cache/days/people/';
  CacheResponse := TStringList.Create;

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"ActorBirthDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');


  // Is there a Cached Response on disk?
  CacheBRResponse := TMemoryStream.Create;
  try
    CacheBRResponse.LoadFromFile(CacheFileDay50+'.br');
  except on E: Exception do
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if (CacheBRResponse.Size > 0) and (Progress <> MainForm.CurrentProgress.Caption) then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay50+'.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay50)));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete (Cached)","TP":'+FloatToStr(Now)+'}';

    // Cleanup
    CacheBRResponse.Free;
  end
  else
  begin
    // Cleanup for now
    CacheBRResponse.Free;

    Actors := TJSONArray.Create;

    // If the Birthday cache doesn't exist, then get the birthdays.
    // We need a list of TMDb IDs that comes from that last to generate a new extended list.
    try
      CacheResponse.Text := '';
      CacheResponse.LoadFromFile(CacheFileBirths, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"BirthDay Data Not Cached","TP":'+FloatToStr(Now)+'}';
      end;
    end;

    if (CacheResponse.Text = '') or (Progress = MainForm.CurrentProgress.Caption) then
    begin
      // Get Updated Birthday Data
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"WikiData","TP":'+FloatToStr(Now)+'}';
      BirthDay(Secret, aMonth, aDay, Progress);
      try
        CacheResponse.Text := '';
        CacheResponse.LoadFromFile(CacheFileBirths, TEncoding.UTF8);
        Actors := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Current WikiData: '+IntToStr(Actors.count)+'People","TP":'+FloatToStr(Now)+'}';
      except on E: Exception do
        begin
          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"BirthDay Data STILL Not Cached","TP":'+FloatToStr(Now)+'}';
        end;
      end;
    end
    else
    begin
      Actors := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Prior WikiData: '+IntToStr(Actors.count)+'People","TP":'+FloatToStr(Now)+'}';
    end;

    if CacheResponse.Text = '' then
    begin
      // Well, we STILL don't have data, so let's give up for now
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"BirthDay Data Not Available","TP":'+FloatToStr(Now)+'}';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
      CacheResponse.Free;
      Exit;
    end
    else
    begin
      // Alright, we've somehow got a list of birthdays but no data cached.
      // Let's go and get that data from TMDb

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing '+IntToStr(Actors.Count)+' Actors","TP":'+FloatToStr(Now)+'}';

      // Gonna build our own JSON string
      Reply := '[';
      ActorID := 1;


      for j := 0 to (Actors.Count - 1) do
      begin

        // This is the actor we're looking at via their TMDB ID (should be an integer!!)
        // This is the actor we're looking at via their TMDB ID (should be an integer!!)
        ActorRef := Trim((((actors.Items[j] as TJSONObject).getValue('TMDbID') as TJSONObject).GetValue('value') as TJSONString).Value);
        if (Pos(' ', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos(' ',ActorRef)-1);
        if (Pos('-', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos('-',ActorRef)-1);
        ActorRef := RightStr('00000000'+IntToStr(StrToIntDef(ActorRef,0)),8);
        ActorRefShort := RightStr(ActorRef,3);

        CacheFilePerson := 'cache/people/actorious/'+ActorRefShort+'/person-'+ActorRef+'.json';
        ActorNew := '';

        // For each person, we're looking for either a processed person file on disk (actorious), in which case we're done with this person.
        // If we don't find that, we can look and swee if we have an unprocessed person file on disk (tmdb), in which case we'll process it.
        // Otherwise, we have to go and get the data from TMDB first.

        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+': TMDb #'+ActorRef+'","TP":'+FloatToStr(Now)+'}';

        // Do we have the processed data for this person already?
        try
          CacheResponse.Text := '';
          CacheResponse.LoadFromFile(CacheFilePerson, TEncoding.UTF8);
        except on E: Exception do
          begin
            MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+': TMDb #'+ActorRef+' not cached","TP":'+FloatToStr(Now)+'}';
          end;
        end;

        // If we can't, or we're regenerating this data, then lets go and get it again
        if (CacheResponse.Text = '') or (Progress = MainForm.CurrentProgress.Caption) then
        begin
          CacheFilePerson := 'cache/people/tmdb/'+ActorRefShort+'/person-'+ActorRef+'.json';

          // We don't have data
          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+': TMDb #'+ActorRef+' Retrieving","TP":'+FloatToStr(Now)+'}';
          CacheResponse.Text := GetPersonfromTMDb(StrToInt(ActorRef), False);

          // It has just (hopefully) been generated.  So let's try and access it again
          if CacheResponse.Text = '' then
          begin
            TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
            CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
            CacheResponse.SaveToStream(Result);
            CacheResponse.Free;
            Exit;
          end
          else
          begin
            // Kind of unusual, but with random user-supplied data, perhaps not surprising, here we're
            // passing a quick filter over all of the JSON to get rid of a few random characters that can
            // mess things up - specifically TABS, LINEFEEDS and CARRIAGE RETURNS.  They don't really have
            // a place in JSON, and should be encoded in any place they might be needed.
            req := FilterResponse(CacheResponse.Text);

            try
              Data := TJSONObject.ParseJSONValue(req) as TJSONObject;
//              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Person Data Loaded ('+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+'","TP":'+FloatToStr(Now)+'}';
            except on E: Exception do
              begin
                // Got a response that isn't valid JSON?!
                MainForm.LogEvent('Invalid TMDb Response received');
              end;
            end;

            if (Data <> nil)
              and (req <> '') // Make sure we have some data to work with
//              and ((Data.getValue('adult') = nil)  // no actors without roles
//                   or ((Data.getValue('adult') as TJSONBool).asBoolean = False)) // family-friendly content only, please
              and (Data.getValue('combined_credits') <> nil)  // no actors without roles
              and ((((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Count) > 0) // no actors without roles
            then
            begin
              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data","TP":'+FloatToStr(Now)+'}';
              ActorNew := ProcessActor(ActorID, ActorRef, Data.ToString, j, Actors.ToString, ProgressPrefix, ProgressKey, false);
            end;
            Data.Free;
          end;
        end
        else
        begin
          // We Do have data - this is already processed
          ActorNew := CacheResponse.Text
        end;

        // Resetting again
        CacheResponse.Text := '';

        if actornew <> '' then
        begin
          if actorid = 1
          then Reply := Reply + actornew
          else Reply := Reply+','+actornew;
          actorid := actorid + 1;
        end;

      end;

      Reply := Reply+']';
      Actors.Free;
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Person Data Processed","TP":'+FloatToStr(Now)+'}';

      // Ok, lets sort the reply and change the ID to be the rank.

      Actors := TJSONObject.ParseJSONValue(Reply) as TJSONArray;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Final Prep Started","TP":'+FloatToStr(Now)+'}';

      CacheResponse.Text := '';
      for i := 0 to Actors.Count-1 do
        CacheResponse.Add(
          FormatFloat('00000000.0000',1000000.0 - ((Actors.Items[i] as TJSONObject).GetValue('POP') as TJSONNumber).AsDouble)+
          RightStr('00000000'+((Actors.Items[i] as TJSONObject).GetValue('TID') as TJSONString).Value,8)+
          RightStr('00000000'+IntToSTr(i),8)
        );
      CacheResponse.Sort;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Indexed","TP":'+FloatToStr(Now)+'}';

      SortedActors := TJSONArray.Create;
      SortedActors50 := TJSONArray.Create;
      dedupes := '';
      dedupe := 0;
      for i := 0 to CacheResponse.Count - 1 do
      begin
        if Copy(CacheResponse[i],14,8) <> dedupes then
        begin
          dedupes := Copy(CacheResponse[i],14,8);
          (Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).addPair('ID',  TJSONNumber.Create(i+1));
          SortedActors.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);

          if (((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('XXX') as TJSONBool).AsBoolean = False)
          then dedupe := dedupe + 1;
          if dedupe <= 50 then SortedActors50.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);
        end;
      end;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Sorted","TP":'+FloatToStr(Now)+'}';

      try
        CacheResponse.Text := SortedActors.ToString;
        CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);

        // Save the response to disk as-is
        ResponseFile := TMemoryStream.Create;
        ResponseFile.LoadFromFile(CacheFileDay);
        ResponseFile.Seek(0, soFromBeginning);

        // Compress the stream with Brotli
        Brotli := TMemoryStream.Create;
        BrotliCompressStream(ResponseFile, Brotli, bcBetter);
        Brotli.Seek(0, soFromBeginning);

        // Save the Brotli-compressed response to disk
        Brotli.SaveToFile(CacheFileDay+'.br');
        ResponseFile.Free;
        Brotli.Free;


        // Now lets do it all again for the 50 version
        CacheResponse.Text := SortedActors50.ToString;
        CacheResponse.SaveToFile(CacheFileDay50);

        Actors.Free;

        // Save the response to disk as-is
        ResponseFile := TMemoryStream.Create;
        ResponseFile.LoadFromFile(CacheFileDay50);
        ResponseFile.Seek(0, soFromBeginning);

        // Compress the stream with Brotli
        Brotli := TMemoryStream.Create;
        BrotliCompressStream(ResponseFile, Brotli, bcMax);
        Brotli.Seek(0, soFromBeginning);

        // Save the Brotli-compressed response to disk
        Brotli.SaveToFile(CacheFileDay50+'.br');
        ResponseFile.Free;
        Brotli.Free;

      except on E: Exception do
         begin
//          MainForm.LogEvent(E.ClassName+': '+E.Message);
         end;
      end;
    end;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Stored","TP":'+FloatToStr(Now)+'}';

    CacheBRResponse := TMemoryStream.Create;
    try
      CacheBRResponse.LoadFromFile(CacheFileDay50+'.br');
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data STILL Not Cached?!","TP":'+FloatToStr(Now)+'}';
        CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
        TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
        CacheResponse.SaveToStream(Result);
      end;
    end;

    if CacheBRResponse.Size > 0 then
    begin
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay50+'.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay50)));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
      CacheBRResponse.Free;
    end
    else
    begin
      CacheResponse.Text := '[]';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
    end;
  end;

  CacheResponse.Free;
end;

function TActorInfoService.ActorDeathDay50(Secret: String; aMonth, aDay: Integer; Progress:String): TStream;
var
  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
  cacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  data: TJSONObject;                          // Response converted to JSON
  actors: TJSONArray;
  sortedactors: TJSONArray;
  sortedactors50: TJSONArray;
  req: String;

  Reply: String;

  actorid: integer;  // counter for actors
  ActorRef: String;   // TMDb for Actor
  ActorRefShort: String; // Last 3 chars used in cache file naming
  actornew: String;


  i: integer;  // used for iterating list of roles (movies, tv shows)
  j: integer;  // used for iterating list of Deathdays from original Deathday list

  dedupe: integer;      // Used to de-duplicate the top movies and tv shows.  Duplicates arise when
                        // the same actor has many different roles on the same show. Which we want to
                        // see, just not in the 'top' section as they tend to crowd out everything else.
  dedupes: string;

  CacheFileDay: String;
  CacheFileDay50: String;
  CacheFilePerson: String;
  CacheFileDeaths: String;

begin
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  CacheFileDay    := 'cache/days/actorious-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFileDay50  := 'cache/days/actorious-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'-50.json';
  CacheFileDeaths := 'cache/days/wikidata-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
  CacheFilePerson := 'cache/days/people/';
  CacheResponse := TStringList.Create;

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"ActorDeathDay"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');


  // Is there a Cached Response on disk?
  CacheBRResponse := TMemoryStream.Create;
  try
    CacheBRResponse.LoadFromFile(CacheFileDay50+'.br');
  except on E: Exception do
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if (CacheBRResponse.Size > 0) and (Progress <> MainForm.CurrentProgress.Caption) then
  begin
    // We've got data, so just return it and be done
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay50+'.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay50)));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

    // Cleanup
    CacheBRResponse.Free;
  end
  else
  begin
    // Cleanup for now
    CacheBRResponse.Free;

    Actors := TJSONArray.Create;

    // If the Deathday cache doesn't exist, then get the Deathdays.
    // We need a list of TMDb IDs that comes from that last to generate a new extended list.
    try
      CacheResponse.Text := '';
      CacheResponse.LoadFromFile(CacheFileDeaths, TEncoding.UTF8);
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"DeathDay Data Not Cached","TP":'+FloatToStr(Now)+'}';
      end;
    end;

    if (CacheResponse.Text = '') or (Progress = MainForm.CurrentProgress.Caption) then
    begin
      // Get Updated Deathday Data
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"WikiData","TP":'+FloatToStr(Now)+'}';
      DeathDay(Secret, aMonth, aDay, Progress);
      try
        CacheResponse.Text := '';
        CacheResponse.LoadFromFile(CacheFileDeaths, TEncoding.UTF8);
        Actors := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Current WikiData: '+IntToStr(Actors.count)+'People","TP":'+FloatToStr(Now)+'}';
      except on E: Exception do
        begin
          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"DeathDay Data STILL Not Cached","TP":'+FloatToStr(Now)+'}';
        end;
      end;
    end
    else
    begin
      Actors := ((TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Using Prior WikiData: '+IntToStr(Actors.count)+'People","TP":'+FloatToStr(Now)+'}';
    end;

    if CacheResponse.Text = '' then
    begin
      // Well, we STILL don't have data, so let's give up for now
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"DeathDay Data Not Available","TP":'+FloatToStr(Now)+'}';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
      CacheResponse.Free;
      Exit;
    end
    else
    begin
      // Alright, we've somehow got a list of Deathdays but no data cached.
      // Let's go and get that data from TMDb

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing '+IntToStr(Actors.Count)+' Actors","TP":'+FloatToStr(Now)+'}';

      // Gonna build our own JSON string
      Reply := '[';
      ActorID := 1;


      for j := 0 to (Actors.Count - 1) do
      begin

        // This is the actor we're looking at via their TMDB ID (should be an integer!!)
        // This is the actor we're looking at via their TMDB ID (should be an integer!!)
        ActorRef := Trim((((actors.Items[j] as TJSONObject).getValue('TMDbID') as TJSONObject).GetValue('value') as TJSONString).Value);
        if (Pos(' ', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos(' ',ActorRef)-1);
        if (Pos('-', ActorRef) > 0) then ActorRef := Copy(ActorRef,1,Pos('-',ActorRef)-1);
        ActorRef := RightStr('00000000'+IntToStr(StrToIntDef(ActorRef,0)),8);
        ActorRefShort := RightStr(ActorRef,3);

        CacheFilePerson := 'cache/people/actorious/'+ActorRefShort+'/person-'+ActorRef+'.json';
        ActorNew := '';

        // For each person, we're looking for either a processed person file on disk (actorious), in which case we're done with this person.
        // If we don't find that, we can look and swee if we have an unprocessed person file on disk (tmdb), in which case we'll process it.
        // Otherwise, we have to go and get the data from TMDB first.

        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+': TMDb #'+ActorRef+'","TP":'+FloatToStr(Now)+'}';

        // Do we have the processed data for this person already?
        try
          CacheResponse.Text := '';
          CacheResponse.LoadFromFile(CacheFilePerson, TEncoding.UTF8);
        except on E: Exception do
          begin
            MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+': TMDb #'+ActorRef+' not cached","TP":'+FloatToStr(Now)+'}';
          end;
        end;

        // If we can't, or we're regenerating this data, then lets go and get it again
        if (CacheResponse.Text = '') or (Progress = MainForm.CurrentProgress.Caption) then
        begin
          CacheFilePerson := 'cache/people/tmdb/'+ActorRefShort+'/person-'+ActorRef+'.json';

          // We don't have data
          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person '+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+': TMDb #'+ActorRef+' Retrieving","TP":'+FloatToStr(Now)+'}';
          CacheResponse.Text := GetPersonfromTMDb(StrToInt(ActorRef), False);

          if CacheResponse.Text = '' then
          begin
            TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
            CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
            CacheResponse.SaveToStream(Result);
            CacheResponse.Free;
            Exit;
          end
          else
          begin
            // Kind of unusual, but with random user-supplied data, perhaps not surprising, here we're
            // passing a quick filter over all of the JSON to get rid of a few random characters that can
            // mess things up - specifically TABS, LINEFEEDS and CARRIAGE RETURNS.  They don't really have
            // a place in JSON, and should be encoded in any place they might be needed.
            req := FilterResponse(CacheResponse.Text);

            try
              Data := TJSONObject.ParseJSONValue(req) as TJSONObject;
//              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Person Data Loaded ('+IntToStr(j+1)+' of '+IntToStr(Actors.Count)+'","TP":'+FloatToStr(Now)+'}';
            except on E: Exception do
              begin
                // Got a response that isn't valid JSON?!
                MainForm.LogEvent('Invalid TMDb Response received');
              end;
            end;

            if (Data <> nil)
              and (req <> '') // Make sure we have some data to work with
//              and ((Data.getValue('adult') = nil)  // no actors without roles
//                   or ((Data.getValue('adult') as TJSONBool).asBoolean = False)) // family-friendly content only, please
              and (Data.getValue('combined_credits') <> nil)  // no actors without roles
              and ((((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Count) > 0) // no actors without roles
            then
            begin
              MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Person Data","TP":'+FloatToStr(Now)+'}';
              ActorNew := ProcessActor(ActorID, ActorRef, Data.ToString, j, Actors.ToString, ProgressPrefix, ProgressKey, false);
            end;
            Data.Free;
          end;
        end
        else
        begin
          // We Do have data - this is already processed
          ActorNew := CacheResponse.Text
        end;

        // Resetting again
        CacheResponse.Text := '';

        if actornew <> '' then
        begin
          if actorid = 1
          then Reply := Reply + actornew
          else Reply := Reply+','+actornew;
          actorid := actorid + 1;
        end;

      end;

      Reply := Reply+']';
      Actors.Free;
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Person Data Processed","TP":'+FloatToStr(Now)+'}';

      // Ok, lets sort the reply and change the ID to be the rank.

      Actors := TJSONObject.ParseJSONValue(Reply) as TJSONArray;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Final Prep Started","TP":'+FloatToStr(Now)+'}';

      CacheResponse.Text := '';
      for i := 0 to Actors.Count-1 do
        CacheResponse.Add(
          FormatFloat('00000000.0000',1000000.0 - ((Actors.Items[i] as TJSONObject).GetValue('POP') as TJSONNumber).AsDouble)+
          RightStr('00000000'+((Actors.Items[i] as TJSONObject).GetValue('TID') as TJSONString).Value,8)+
          RightStr('00000000'+IntToSTr(i),8)
        );
      CacheResponse.Sort;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Indexed","TP":'+FloatToStr(Now)+'}';

      SortedActors := TJSONArray.Create;
      SortedActors50 := TJSONArray.Create;
      dedupes := '';
      dedupe := 0;
      for i := 0 to CacheResponse.Count - 1 do
      begin
        if Copy(CacheResponse[i],14,8) <> dedupes then
        begin
          dedupes := Copy(CacheResponse[i],14,8);
          (Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).addPair('ID',  TJSONNumber.Create(i+1));
          SortedActors.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);

          if (((Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject).getValue('XXX') as TJSONBool).AsBoolean = False)
          then dedupe := dedupe + 1;
          if dedupe <= 50 then SortedActors50.AddElement(Actors.Items[StrToInt(RightStr(CacheResponse[i],8))] as TJSONObject);
        end;
      end;

      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Sorted","TP":'+FloatToStr(Now)+'}';

      try
        CacheResponse.Text := SortedActors.ToString;
        CacheResponse.SaveToFile(CacheFileDay, TEncoding.UTF8);

        // Save the response to disk as-is
        ResponseFile := TMemoryStream.Create;
        ResponseFile.LoadFromFile(CacheFileDay);
        ResponseFile.Seek(0, soFromBeginning);

        // Compress the stream with Brotli
        Brotli := TMemoryStream.Create;
        BrotliCompressStream(ResponseFile, Brotli, bcBetter);
        Brotli.Seek(0, soFromBeginning);

        // Save the Brotli-compressed response to disk
        Brotli.SaveToFile(CacheFileDay+'.br');
        ResponseFile.Free;
        Brotli.Free;


        // Now lets do it all again for the 50 version
        CacheResponse.Text := SortedActors50.ToString;
        CacheResponse.SaveToFile(CacheFileDay50);

        Actors.Free;

        // Save the response to disk as-is
        ResponseFile := TMemoryStream.Create;
        ResponseFile.LoadFromFile(CacheFileDay50);
        ResponseFile.Seek(0, soFromBeginning);

        // Compress the stream with Brotli
        Brotli := TMemoryStream.Create;
        BrotliCompressStream(ResponseFile, Brotli, bcMax);
        Brotli.Seek(0, soFromBeginning);

        // Save the Brotli-compressed response to disk
        Brotli.SaveToFile(CacheFileDay50+'.br');
        ResponseFile.Free;
        Brotli.Free;

      except on E: Exception do
         begin
//          MainForm.LogEvent(E.ClassName+': '+E.Message);
         end;
      end;
    end;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Results Stored","TP":'+FloatToStr(Now)+'}';

    CacheBRResponse := TMemoryStream.Create;
    try
      CacheBRResponse.LoadFromFile(CacheFileDay50+'.br');
    except on E: Exception do
      begin
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data STILL Not Cached?!","TP":'+FloatToStr(Now)+'}';
        CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
        TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
        CacheResponse.SaveToStream(Result);
      end;
    end;

    if CacheBRResponse.Size > 0 then
    begin
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDay50+'.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay50)));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
      CacheBRResponse.Free;
    end
    else
    begin
      CacheResponse.Text := '[]';
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.SaveToStream(Result);
      CacheBRResponse.Free;
    end;
  end;

  CacheResponse.Free;
end;

function TActorInfoService.TopOneThousand(Secret, Progress: String): TStream;
var
  Page: Integer;
  Popular: Integer;

  CacheFile: String;
  CacheResponse: TStringList;
  cacheBRResponse: TMemoryStream;

  ProgressPrefix: String;
  ProgressKey: Integer;

  ClientReq: TNetHTTPClient;
  qry: String;
  URL: String;
  req: String;

  Response: TStream;                // Response from Wikidata
  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk
  Data: TJSONArray;
  ActorData: TJSONObject;

  Actors: string;
  AdActors: String;
  TotalActors: Integer;
  AdultActors: Integer;

  Actor: String;
  Unique: String;

  Regenerate: Boolean;

begin

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;
  CacheBRResponse := TMemoryStream.Create;
  CacheResponse := TStringList.Create;
  Response := TStream.Create;

  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  CacheFile := 'cache/people/top1000/top1000';


  // Sometimes we want to force the regeneration of data, particularly after major structural changes
  if ((Pos('::1'           ,TXDataOperationContext.Current.Request.RemoteIP) > 0)  or
      (Pos('23.111.75.19'  ,TXDataOperationContext.Current.Request.RemoteIP) > 0))
     and MainForm.ckRegenerate.Checked
  then Regenerate := True
  else Regenerate := False;


  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"Top1000"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');

  // Is there a Cached Response on disk?
  try
    if Pos('[ADULT]',Progress) > 0
    then CacheBRResponse.LoadFromFile(CacheFile+'-adult.json.br')
    else CacheBRResponse.LoadFromFile(CacheFile+'.json.br');
  except on E: Exception do
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if ((CacheBRResponse.Size > 0) and not(Regenerate)) then
  begin
    // We've got data, so just return it and be done
    if Pos('[ADULT]',Progress) > 0
   then CacheFile := CacheFile+'-adult';
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.json.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile+'.json')));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
    CacheBRResponse.Free;
  end
  else
  begin

    // Time to Regenerate this data
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Regenerating Top 1,000 Actors","TP":'+FloatToStr(Now)+'}';

    Actors := '[';
    AdActors := '[';
    TotalActors := 0;
    AdultActors := 0;
    Unique := '';

    for Page := 0 to 74 do
    begin

      // Time to Regenerate this data
//      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Requesting Top 1,000 Actors ( Page '+InttoStr(Page)+' of 70 )","TP":'+FloatToStr(Now)+'}';
      Data := TJSONArray.Create;

      if (Page > 0) then
      begin
        // Setup the client connection for TMDb
        ClientReq := TNetHTTPClient.Create(nil);
        ClientReq.ConnectionTimeout := 900000; // 15 minutes
        ClientReq.ResponseTimeout := 900000; // 15 minutes
        ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

        qry := 'person/popular';
        qry := qry+'?api_key='+MainForm.edTMDbAPI.Text;
        qry := qry+'&language=en-US';
        qry := qry+'*&page='+IntToStr(Page);

        req := '';

        // Setup the URL and encode the query in it
        URL := TidURI.URLEncode('https://api.themoviedb.org/3/'+qry);
        try
          try
            Response := ClientReq.Get(URL).ContentStream;
          except on E:Exception do
            begin
              MainForm.LogEvent('EXCEPTION in TopOneThousand TMDb ClientReq.Get');
              MainForm.LogEvent(E.Classname+': '+E.Message);
            end;
          end;

          // One Page At A Time
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Top 1,000 Actors ( Page '+InttoStr(Page)+' of 70 )","TP":'+FloatToStr(Now)+'}';

          // Save the response to disk as-is
          ResponseFile := TMemoryStream.Create;
          ResponseFile.CopyFrom(Response,Response.size);
          ResponseFile.SaveToFile(CacheFile+'-'+IntToStr(Page)+'.json');
          ResponseFile.Seek(0, soFromBeginning);

          // Cleanup
          ResponseFile.Free;
          ClientReq.Free;

        except on E: Exception do
          begin
            MainForm.LogEvent('EXCEPTION in TopOneThousand TMDb File Processing');
            MainForm.LogEvent(E.ClassName+': '+E.Message);
          end;
        end;
      end;


//      Data := TJSONArray.Create;
      try
        CacheResponse.LoadFromFile(CacheFile+'-'+IntToStr(Page)+'.json', TEncoding.UTF8);
        Data := (TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONArray;
      except on E:Exception do
        begin
          MainForm.LogEvent('EXCEPTION in TopOneThousand TMDb Data Conversion');
          MainForm.LogEvent(E.Classname+': '+E.Message);
        end;
      end;

      for Popular := 0 to Data.Count -1 do
      begin
        if Pos('['+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt)+']',Unique) = 0 then
        begin

          Unique := Unique+'['+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt)+']';
          CacheResponse.Text := '';

          try

            // If regenerating, we want to regenerate this file, not its contents as that will wipe out all the Wikidata content
            Actor := GetPerson(Popular + (Page*20), 1500, RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8),  RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),3), '', ProgressPrefix, ProgressKey, False);

//            try
//
//              CacheResponse.Text := '';
//              CacheResponse.LoadFromFile('cache/people/actorious/'+
//                RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),3)+
//                '/person-'+
//                RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8)+
//                '.json', TEncoding.UTF8);
//            except on E: Exception do
//              begin
//                // missing file - try and get it?
//                try
//                  MainForm.LogEvent('- Generating Top1000 Person #'+IntToStr((Page*20)+Popular)+' Without Birthday: '+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt));
//                  CacheResponse.Text := GetPersonfromTMDb(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt, False);
//                  ActorData := TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject;
//                  ProcessActor(Popular, IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt), ActorData.ToString, -1, '[]', ProgressPrefix, ProgressKey, Regenerate);
//                  ActorData.Free;
//                  CacheResponse.LoadFromFile('cache/people/actorious/'+
//                    RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),3)+
//                    '/person-'+
//                    RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8)+
//                    '.json', TEncoding.UTF8);
//                except on E: Exception do
//                  begin
//                    // We tried!
//                  end;
//                end;
//              end;
//            end;

            if Actor <> '' then
            begin
              ActorData := TJSONObject.ParseJSONValue(Actor) as TJSONObject;
              ActorData.RemovePair('ID');

              if (ActorData.getValue('XXX') <> nil) and ((ActorData.getValue('XXX') as TJSONBool).AsBoolean = true)
              then ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(AdultActors + 1)))
              else ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(TotalActors + 1)));

              // One last time - exclude anyone without any roles
              if ((ActorData.getValue('NUM') as TJSONNumber).AsInt <> 0) then
              begin
                if (ActorData.getValue('XXX') <> nil) and ((ActorData.getValue('XXX') as TJSONBool).AsBoolean = true) then
                begin
                  if (AdultActors < 1000) then
                  begin
                    if AdultActors = 0
                    then AdActors := AdActors+ActorData.ToString
                    else AdActors := AdActors+','+ActorData.ToString;
                    AdultActors := AdultActors + 1;
                  end;
                end
                else
                begin
                  if (TotalActors < 1000) then
                  begin
                    if TotalActors = 0
                    then Actors := Actors+ActorData.ToString
                    else Actors := Actors+','+ActorData.ToString;
                    TotalActors := TotalActors + 1;
                  end;
                end;
              end;
              ActorData.Free;
            end;
          except on E: Exception do
            begin
               MainForm.LogEvent('EXCEPTION in TopOneThousand Adding Actors');
            end;
          end;
        end;
      end;
      Data.Free;
    end;

    Actors := Actors + ']';
    AdActors := AdActors + ']';
    CacheResponse.Text := '';


    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Saving Top 1,000 Actors 1/2","TP":'+FloatToStr(Now)+'}';
    try
      TFile.WriteAllText(CacheFile+'.json', Actors, TEncoding.UTF8);
    except on E: exception do
      begin
        MainForm.LogEvent(E.ClassName+': '+E.Message);
      end
    end;
    Actors := '';

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Saving Top 1,000 Actors 2/2","TP":'+FloatToStr(Now)+'}';
    try
      TFile.WriteAllText(CacheFile+'-adult.json', AdActors);
    except on E: exception do
      begin
        MainForm.LogEvent(E.ClassName+': '+E.Message);
      end
    end;
    AdActors := '';

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing Top 1,000 Actors 1/2","TP":'+FloatToStr(Now)+'}';

    ResponseFile := TMemoryStream.Create;
    ResponseFile.LoadFromFile(CacheFile+'.json');
    ResponseFile.Seek(0, soFromBeginning);

    // Compress the stream with Brotli
    Brotli := TMemoryStream.Create;
    BrotliCompressStream(ResponseFile, Brotli, bcBetter);
    Brotli.Seek(0, soFromBeginning);

    // Save the Brotli-compressed response to disk
    Brotli.SaveToFile(CacheFile+'.json.br');
    ResponseFile.Free;
    Brotli.Free;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing Top 1,000 Actors 2/2","TP":'+FloatToStr(Now)+'}';

    ResponseFile := TMemoryStream.Create;
    ResponseFile.LoadFromFile(CacheFile+'-adult.json');
    ResponseFile.Seek(0, soFromBeginning);

    // Compress the stream with Brotli
    Brotli := TMemoryStream.Create;
    BrotliCompressStream(ResponseFile, Brotli, bcBetter);
    Brotli.Seek(0, soFromBeginning);

    // Save the Brotli-compressed response to disk
    Brotli.SaveToFile(CacheFile+'-adult.json.br');
    ResponseFile.Free;
    Brotli.Free;

    try
      if Pos('[ADULT]',Progress) > 0
      then CacheFile := CacheFile+'-adult';
      CacheBRResponse.LoadFromFile(CacheFile+'.json.br');
    except on E: Exception do
      begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
      end;
    end;


    if CacheBRResponse.Size > 0 then
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Top1000 Data Ready","TP":'+FloatToStr(Now)+'}';
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.json.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile+'.json')));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
    end
    else
    begin
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
    end;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Cleanup","TP":'+FloatToStr(Now)+'}';

    // Cleanup
    cacheBRResponse.Free;
    CacheResponse.Free;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
  end;
end;

function TActorInfoService.TopToday(Secret: String; aMonth, aDay: Integer): TStream;
var
//  Regenerate: Boolean;              // Flag whether cached data can be used

  CacheIndex: Integer;              // The Julian day of the request (Jan1=1, Dec31=366)
  CacheResponse: TStringList;       // The JSON response, ideally loaded from disk cache
//  CacheBRResponse: TMemoryStream;   // The JSON-BR response, ideally loaded from disk cache

  ProgressPrefix: String;           // Used to pass progress information back to client
  ProgressKey: Integer;             // Location of progress data in common array

//  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
//  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk

  CacheFileDay:     String;  // The final results
//  CacheFileDayBR:   String;  // The final results, compressed with Brotli
//  CacheFileDay50:   String;  // The abbreviated results
//  CacheFileDay50BR: String;  // The abbreviated results, compressed with Brotli
//  CacheFileWiki:    String;  // The Wikidata results
//  CacheFilePerson:  String;  // The person results

  ReplyStream: TStringStream;
begin
  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    CacheIndex := DayOfTheYear(EncodeDate(2020, aMonth, aDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Day');
    end;
  end;

  // Alright, seems like we've got a valid request.
  Result := TMemoryStream.Create;
//  CacheBRResponse := TMemoryStream.Create;
  CacheResponse := TStringList.Create;


//  // Sometimes we want to force the regeneration of data, particularly after major structural changes
//  if ((Pos('::1'           ,TXDataOperationContext.Current.Request.RemoteIP) > 0)  or
//      (Pos('23.111.75.19'  ,TXDataOperationContext.Current.Request.RemoteIP) > 0))
//     and MainForm.ckRegenerate.Checked
//  then Regenerate := True
//  else Regenerate := False;


  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
//  SetBrotliHeaders;

  // We'll be referencing a number of files, so let's identify them now.
  CacheFileDay     := 'cache/days/toptoday/day-'+FormatDateTime('mmdd',EncodeDate(2020, aMonth, aDay))+'.json';
//  CacheFileDayBR   := 'cache/days/toptoday/day-'+FormatDateTime('mmdd',EncodeDate(2020, aMonth, aDay))+'.json.br';

  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"TopToday"'+
                    ',"DY":"'+IntToStr(cacheindex)+'"'+
                    ',"DT":"'+FormatDateTime('MMMdd',EncodeDate(2020, aMonth, aDay))+'"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+',"PR":"Initialized","TP":'+FloatToStr(Now)+'}');


  // Is there a Cached Response on disk?
  if FileExists(CacheFileDay)
  then CacheResponse.LoadFromFile(CacheFileDay);


  // If we've got data and it isn't empty then send it and be done
  if (CacheResponse.Text.Length > 0) then
  begin
    ReplyStream := TStringStream.Create(CacheResponse.Text);
    ReplyStream.Seek(0, soFromBeginning);
    Result.CopyFrom(ReplyStream, ReplyStream.size);
    ReplyStream.Free;
//    CacheBRResponse.Free;

//    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFileDayBR)));
//    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFileDay)));
//    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete (Cached)","TP":'+FloatToStr(Now)+'}';
  end
  else
  begin
    TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
    ReplyStream := TStringStream.Create('{"ERR":"Data Not Available"}');
    ReplyStream.Seek(0, soFromBeginning);
    Result.CopyFrom(ReplyStream, ReplyStream.size);
    ReplyStream.Free;
  end;

  // Cleanup
//  cacheBRResponse.Free;
  CacheResponse.Free;

  MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
end;

function TActorInfoService.TopFiveThousand(Secret, Progress: String): TStream;
var
  Page: Integer;
  Popular: Integer;

  CacheFile: String;
  CacheResponse: TStringList;
  cacheBRResponse: TMemoryStream;

  ProgressPrefix: String;
  ProgressKey: Integer;

  ClientReq: TNetHTTPClient;
  qry: String;
  URL: String;
  req: String;

  Response: TStream;                // Response from Wikidata
  ResponseFile :TMemoryStream;      // Used to Write JSON to disk
  Brotli: TMemoryStream;            // Used to Write compressed JSON to disk
  Data: TJSONArray;
  ActorData: TJSONObject;

  Actor: String;
  Actors: string;
  AdActors: String;
  TotalActors: Integer;
  AdultActors: Integer;

  Unique: String;

  Regenerate: Boolean;
begin

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Alright, seems like we've got a valid request.
  // Here, we're creating the desired result - JSON.
  // For extra credit, we'll actually compress it with Brotli and store it.
  // JSON compresses really well, and Brotli is the best compressor, given time.
  Result := TMemoryStream.Create;
  CacheBRResponse := TMemoryStream.Create;
  CacheResponse := TStringList.Create;
  Response := TStream.Create;

  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');
  TXDataOperationContext.Current.Response.Headers.SetValue('content-encoding', 'br');
  TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');

  CacheFile := 'cache/people/top5000/top5000';


  // Sometimes we want to force the regeneration of data, particularly after major structural changes
  if ((Pos('::1'           ,TXDataOperationContext.Current.Request.RemoteIP) > 0)  or
      (Pos('23.111.75.19'  ,TXDataOperationContext.Current.Request.RemoteIP) > 0))
     and MainForm.ckRegenerate.Checked
  then Regenerate := True
  else Regenerate := False;


  // Set up our progress system for this request
  ProgressPrefix := '{"ST":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'"'+
                    ',"TS":'+FloatToStr(Now)+
                    ',"ID":"'+Progress+'"'+
                    ',"IP":"'+TXDataOperationContext.Current.Request.RemoteIP+'"'+
                    ',"RQ":"Top5000"';
  ProgressKey := MainForm.Progress.Add(ProgressPrefix+'}');


  // Is there a Cached Response on disk?
  try
    if Pos('[ADULT]',Progress) > 0
    then CacheBRResponse.LoadFromFile(CacheFile+'-adult.json.br')
    else CacheBRResponse.LoadFromFile(CacheFile+'.json.br');
  except on E: Exception do
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
    end;
  end;

  if ((CacheBRResponse.Size > 0) and not(Regenerate)) then
  begin
    // We've got data, so just return it and be done
    if Pos('[ADULT]',Progress) > 0
    then CacheFile := CacheFile+'-adult';
    Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
    TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.json.br')));
    TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile+'.json')));
    TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
    CacheBRResponse.Free;
    CacheResponse.Free;
    exit;
  end
  else
  begin

    // Time to Regenerate this data
    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Regenerating Top 5,000 Actors","TP":'+FloatToStr(Now)+'}';

    Actors := '[';
    AdActors := '[';
    TotalActors := 0;
    AdultActors := 0;
    Unique := '';

    for Page := 0 to 500 do
    begin

      // Time to Regenerate this data
//      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Requesting Top 5,000 Actors ( Page '+InttoStr(Page)+' of 500 )","TP":'+FloatToStr(Now)+'}';
      Data := TJSONArray.Create;

      if (Page > 0) then
      begin
        // Setup the client connection for TMDb
        ClientReq := TNetHTTPClient.Create(nil);
        ClientReq.ConnectionTimeout := 900000; // 15 minutes
        ClientReq.ResponseTimeout := 900000; // 15 minutes
        ClientReq.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

        qry := 'person/popular';
        qry := qry+'?api_key='+MainForm.edTMDbAPI.Text;
        qry := qry+'&language=en-US';
        qry := qry+'*&page='+IntToStr(Page);

        req := '';

        // Setup the URL and encode the query in it
        URL := TidURI.URLEncode('https://api.themoviedb.org/3/'+qry);
        try
          try
            Response := ClientReq.Get(URL).ContentStream;
          except on E:Exception do
            begin
              MainForm.LogEvent('EXCEPTION in TopFiveThousand TMDb ClientReq.Get');
              MainForm.LogEvent(E.Classname+': '+E.Message);
            end;
          end;

          // One Page At A Time
//          MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Processing Top 5,000 Actors ( Page '+InttoStr(Page)+' of 500 )","TP":'+FloatToStr(Now)+'}';

          // Save the response to disk as-is
          ResponseFile := TMemoryStream.Create;
          ResponseFile.CopyFrom(Response,Response.size);
          ResponseFile.SaveToFile(CacheFile+'-'+IntToStr(Page)+'.json');
          ResponseFile.Seek(0, soFromBeginning);

          // Cleanup
          ResponseFile.Free;
          ClientReq.Free;

        except on E: Exception do
          begin
            MainForm.LogEvent('EXCEPTION in TopFiveThousand TMDb File Processing');
            MainForm.LogEvent(E.ClassName+': '+E.Message);
          end;
        end;
      end;


//      Data := TJSONArray.Create;
      try
        CacheResponse.LoadFromFile(CacheFile+'-'+IntToStr(Page)+'.json', TEncoding.UTF8);
        Data := (TJSONObject.ParseJSONValue(CacheResponse.Text) as TJSONObject).getValue('results') as TJSONArray;
      except on E:Exception do
        begin
          MainForm.LogEvent('EXCEPTION in TopFiveThousand TMDb Data Conversion');
          MainForm.LogEvent(E.Classname+': '+E.Message);
        end;
      end;

      for Popular := 0 to Data.Count -1 do
      begin
        ActorData := TJSONObject.Create;

        if Pos('['+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt)+']',Unique) = 0 then
        begin

          Unique := Unique+'['+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt)+']';
          CacheResponse.Text := '';

          try

            // If regenerating, we want to regenerate this file, not its contents as that will wipe out all the Wikidata content
            Actor := GetPerson(TotalActors+AdultActors, 10000, RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8),  RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),3), '', ProgressPrefix, ProgressKey, False);

          except on E: Exception do
            begin
              MainForm.LogException('Top5000/GetPerson', E.ClassName, E.Message,  RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8));
            end;
          end;

          try
            if Actor <> '' then
            begin
            
              try
                ActorData := TJSONObject.ParseJSONValue(Actor) as TJSONObject;
              except on E: Exception do
                begin
                  MainForm.LogException('Top5000/JSONPerson', E.ClassName, E.Message,  RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8));
                  ActorData.Free;
                end;
              end;

              try
                if (ActorData.getValue('ID') <> nil) then ActorData.RemovePair('ID');
              except on E: Exception do
                begin
                  MainForm.LogException('Top5000/RemoveID', E.ClassName, E.Message,  RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8));
                  ActorData.Free;
                end;
              end;

              try
                if (ActorData.getValue('XXX') <> nil) and ((ActorData.getValue('XXX') as TJSONBool).AsBoolean = true)
                then ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(AdultActors + 1)))
                else ActorData.AddPair(TJSONPair.Create('ID', TJSONNumber.Create(TotalActors + 1)));
              except on E: Exception do
                begin
                  MainForm.LogException('Top5000/AddID', E.ClassName, E.Message,  RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8));
                  ActorData.Free;
                end;
              end;

              // One last time - exclude anyone without any roles
              
              if (ActorData.getValue('NUM') <> nil) and (ActorData.getValue('NUM') is TJSONNumber) and  ((ActorData.getValue('NUM') as TJSONNumber).AsInt <> 0) then
              begin
                if (ActorData.getValue('XXX') <> nil) and ((ActorData.getValue('XXX') as TJSONBool).AsBoolean = true) then
                begin
                  if (AdultActors < 5000) then
                  begin
                    if AdultActors = 0
                    then AdActors := AdActors+ActorData.ToString
                    else AdActors := AdActors+','+ActorData.ToString;
                    AdultActors := AdultActors + 1;
                  end;
                end
                else
                begin
                  if (TotalActors < 5000) then
                  begin
                    if TotalActors = 0
                    then Actors := Actors+ActorData.ToString
                    else Actors := Actors+','+ActorData.ToString;
                    TotalActors := TotalActors + 1;
                  end;
                end;
              end;

              
              ActorData.Free;
            end;


          except on E: Exception do
            begin
              MainForm.LogException('Top5000/AddActors', E.ClassName, E.Message,  RightStr('00000000'+IntToStr(((Data.Items[Popular] as TJSONObject).getValue('id') as TJSONNumber).AsInt),8));
              ActorData.Free;
            end;
          end;
        end;
      end;
      Data.Free;
    end;

    Actors := Actors + ']';
    AdActors := AdActors + ']';
    CacheResponse.Text := '';


    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Saving Top 5,000 Actors 1/2","TP":'+FloatToStr(Now)+'}';
    try
      TFile.WriteAllText(CacheFile+'.json', Actors, TEncoding.UTF8);
    except on E: exception do
      begin
        MainForm.LogEvent(E.ClassName+': '+E.Message);
      end
    end;
    Actors := '';

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Saving Top 5,000 Actors 2/2","TP":'+FloatToStr(Now)+'}';
    try
      TFile.WriteAllText(CacheFile+'-adult.json', AdActors);
    except on E: exception do
      begin
        MainForm.LogEvent(E.ClassName+': '+E.Message);
      end
    end;
    AdActors := '';


    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing Top 5,000 Actors 1/2","TP":'+FloatToStr(Now)+'}';

    ResponseFile := TMemoryStream.Create;
    ResponseFile.LoadFromFile(CacheFile+'.json');
    ResponseFile.Seek(0, soFromBeginning);

    // Compress the stream with Brotli
    Brotli := TMemoryStream.Create;
    BrotliCompressStream(ResponseFile, Brotli, bcBetter);
    Brotli.Seek(0, soFromBeginning);

    // Save the Brotli-compressed response to disk
    Brotli.SaveToFile(CacheFile+'.json.br');
    ResponseFile.Free;
    Brotli.Free;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Compressing Top 5,000 Actors 2/2","TP":'+FloatToStr(Now)+'}';

    ResponseFile := TMemoryStream.Create;
    ResponseFile.LoadFromFile(CacheFile+'-adult.json');
    ResponseFile.Seek(0, soFromBeginning);

    // Compress the stream with Brotli
    Brotli := TMemoryStream.Create;
    BrotliCompressStream(ResponseFile, Brotli, bcBetter);
    Brotli.Seek(0, soFromBeginning);

    // Save the Brotli-compressed response to disk
    Brotli.SaveToFile(CacheFile+'-adult.json.br');
    ResponseFile.Free;
    Brotli.Free;

    try
      if Pos('[ADULT]',Progress) > 0
      then CacheFile := CacheFile+'-adult';
      CacheBRResponse.LoadFromFile(CacheFile+'.json.br');
    except on E: Exception do
      begin
//      MainForm.LogEvent(E.ClassName+': '+E.Message);
        MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Data Not Cached","TP":'+FloatToStr(Now)+'}';
      end;
    end;


    if CacheBRResponse.Size > 0 then
    begin
      MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Top5000 Data Ready","TP":'+FloatToStr(Now)+'}';
      // We've got data, so just return it and be done
      Result.CopyFrom(CacheBRResponse,CacheBRResponse.size);
      TXDataOperationContext.Current.Response.Headers.SetValue('content-length', IntToStr(FileSizeByName(CacheFile+'.json.br')));
      TXDataOperationContext.Current.Response.Headers.SetValue('x-uncompressed-content-length', IntToStr(FileSizeByName(CacheFile+'.json')));
      TXDataOperationContext.Current.Response.Headers.SetValue('Access-Control-Expose-Headers','x-uncompressed-content-length');
    end
    else
    begin
      TXDataOperationContext.Current.Response.Headers.remove('content-encoding');
      CacheResponse.Text := '{"ERR":"Data Not Unavailable"}';
      CacheResponse.SaveToStream(Result);
    end;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Cleanup","TP":'+FloatToStr(Now)+'}';

    // Cleanup
    cacheBRResponse.Free;
    CacheResponse.Free;

    MainForm.Progress[ProgressKey] := ProgressPrefix+',"PR":"Complete","TP":'+FloatToStr(Now)+'}';
  end;
end;


initialization
  RegisterServiceType(TActorInfoService);

end.
