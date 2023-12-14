unit ActorInfoService;

interface

uses
  XData.Service.Common,
  XData.Security.Attributes,
  Aurelius.Mapping.Attributes,
  System.Classes;


type
  [ServiceContract]
  [Model('Actorious')]
  IActorInfoService = interface(IInvokable)
    ['{D0697F1E-EE4C-47D1-A29E-0B19B5D396FD}']

    ///  <summary>
    ///    Progress
    ///  </summary>
    ///  <remarks>
    ///    Returns the current processing status of a request.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="Progress">
    ///    Key.
    ///  </param>

    [HttpGet] function Progress(Secret: String; Progress: String):String;

    ///  <summary>
    ///    BirthDay
    ///  </summary>
    ///  <remarks>
    ///    Returns a JSON object containing all of the birthdays for people that
    ///    are listed in the Wikidata database with a TMDb ID.  This is cached on disk.
    ///    If the cache is not available, a small delay will be incurred while the data
    ///    is retrieved from Wikidata in real-time.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Birth Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Birth Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function BirthDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;

    ///  <summary>
    ///    DeathDay
    ///  </summary>
    ///  <remarks>
    ///    Returns a JSON object containing all of the deathdays for people that
    ///    are listed in the Wikidata database.  This is cached on disk.  If the
    ///    cache is not available, a small delay will be incurred while the data
    ///    is retrieved from Wikidata in real-time.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Death Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Death Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function DeathDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;


    ///  <summary>
    ///    ReleaseDay
    ///  </summary>
    ///  <remarks>
    ///    Returns a JSON object containing all of the release days for mvoies that
    ///    are listed in the Wikidata database.  This is cached on disk.  If the
    ///    cache is not available, a small delay will be incurred while the data
    ///    is retrieved from Wikidata in real-time.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Release Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Release Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function ReleaseDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;

    ///  <summary>
    ///    ActorBirthday
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors with a birthday that falls on the supplied month and day.
    ///    NOTE: This data is cached.  If you reqest a birthday that has not currently cached,
    ///    there will be a substantial delay in processsing this request.  Please pass in a
    ///    Progress parameter (something like a GUID) and then check on the progress of the
    ///    request by passing the same parameter to the Progress endpoint.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Birth Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Birth Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function ActorBirthDay(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;

    ///  <summary>
    ///    ActorBirthday50
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors with a birthday that falls on the supplied month and day.
    ///    NOTE: This data is cached.  If you reqest a birthday that has not currently cached,
    ///    there will be a substantial delay in processsing this request.  Please pass in a
    ///    Progress parameter (something like a GUID) and then check on the progress of the
    ///    request by passing the same parameter to the Progress endpoint.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Birth Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Birth Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function ActorBirthDay50(Secret: String; aMonth: Integer; aDay: Integer; Progress: String):TStream;

    ///  <summary>
    ///    ActorDeathDay
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that passed away on the specified month and day.
    ///    NOTE: This data is cached.  If you reqest a birthday that has not currently cached,
    ///    there will be a substantial delay in processsing this request.  Please pass in a
    ///    Progress parameter (something like a GUID) and then check on the progress of the
    ///    request by passing the same parameter to the Progress endpoint.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Death Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Death Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function ActorDeathDay(Secret: String; aMonth: Integer; ADay: Integer; Progress: String):TStream;


    ///  <summary>
    ///    ActorDeathDay50
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of the top 50 Actors that passed away on the specified month and day.
    ///    NOTE: This data is cached.  If you reqest a birthday that has not currently cached,
    ///    there will be a substantial delay in processsing this request.  Please pass in a
    ///    Progress parameter (something like a GUID) and then check on the progress of the
    ///    request by passing the same parameter to the Progress endpoint.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Death Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Death Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function ActorDeathDay50(Secret: String; aMonth: Integer; ADay: Integer; Progress: String):TStream;


    ///  <summary>
    ///    MovieReleaseDay
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of the Movies released on the specified month and day.
    ///    NOTE: This data is cached.  If you reqest a day that has not currently cached,
    ///    there will be a substantial delay in processsing this request.  Please pass in a
    ///    Progress parameter (something like a GUID) and then check on the progress of the
    ///    request by passing the same parameter to the Progress endpoint.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Release Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Release Day (1-31).
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function MovieReleaseDay(Secret: String; aMonth: Integer; ADay: Integer; Progress: String):TStream;

    ///  <summary>
    ///    TopOneThousand
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that are rated as in the Top 1000 according
    ///    to TMDb, using the same format as the other actor-related endpoints.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="Segment">
    ///    This is ignored in this endpoint, but is included to keep the same
    ///    calling convention as TopFiveThousand, where it is not ignored.
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function TopOneThousand(Secret: String; Segment: String; Progress: String):TStream;

    ///  <summary>
    ///    TopFiveThousand
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that are rated as in the Top 5000 according
    ///    to TMDb, using the same format as the other actor-related endpoints.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="Segment">
    ///    This JSON is split into 5 segments. A separate request is need for each.
    ///    Segment values should be one of [A, B, C, D, E].
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function TopFiveThousand(Secret: String; Segment: String; Progress: String):TStream;

    ///  <summary>
    ///    TopToday
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that are the top-rated for a given day
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="aMonth">
    ///    Release Month (1-12).
    ///  </param>
    ///  <param name="aDay">
    ///    Release Day (1-31).
    ///  </param>

    [HttpGet] function TopToday(Secret: String; aMonth: Integer; aDay: Integer):TStream;

    ///  <summary>
    ///    SearchPeople
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that match whatever search term is passed. the
    ///    search results include only those poeple that have been cached already,
    ///    which is limited to anyone in the Top5000 list or that has a defined
    ///    birthday in Wikidata, or that has been cached due to an extended search.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="SearchTerm">
    ///    Term to search for in People. Must be at least three characters.
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function SearchPeople(Secret: String; SearchTerm: String; Progress: String):TStream;

    ///  <summary>
    ///    SearchLocal
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that match whatever search term is passed. the
    ///    search results include only those poeple that have been cached already,
    ///    which is limited to anyone in the Top5000 list or that has a defined
    ///    birthday in Wikidata, or that has been cached due to an extended search.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="SearchTerm">
    ///    Term to search for in People. Must be at least three characters.
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function SearchLocal(Secret: String; SearchTerm: String; Progress: String):TStream;

    ///  <summary>
    ///    SearchPeopleExtended
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that match whatever search term is passed.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="SearchTerm">
    ///    Term to search for in People. Must be at least three characters.
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function SearchPeopleExtended(Secret: String; SearchTerm: String; Progress: String):TStream;

    ///  <summary>
    ///    Relatives
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors that are related to the indicated person.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="RelatedTo">
    ///    Searches for people related to this person, a WikiData ID.
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function Relatives(Secret: String; RelatedTo:Integer; Progress: String):TStream;

    ///  <summary>
    ///    GetClientVersion
    ///  </summary>
    ///  <remarks>
    ///    Returns a JSON object showing the current version number and also the image
    ///    to load initially for the main display (the first person on the current date).
    ///  </remarks>
    ///  <param name="Day">
    ///    The current day (local to the user) that we're looking up (MMDD)
    ///  </param>
    [HttpGet] function GetClientVersion(Day:String):TStream;

    ///  <summary>
    ///    Lookup
    ///  </summary>
    ///  <remarks>
    ///    Returns whatever lookup request is received.  Might be a one or more
    ///    actors, movies or tv shows, specified in the request.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="Lookup">
    ///    JSON Lookup request.
    ///  </param>
    ///  <param name="Progress">
    ///    Key to lookup progress with the Progress endpoint.
    ///  </param>

    [HttpGet] function Lookup(Secret: String; Lookup:String; Progress: String):TStream;

  end;

implementation

initialization
  RegisterServiceType(TypeInfo(IActorInfoService));

end.

