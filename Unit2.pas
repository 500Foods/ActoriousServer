unit Unit2;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.shellapi,

  PsAPI,
  TlHelp32,

  System.SysUtils,
  System.Variants,
  System.Math,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.NetEncoding,
  System.DateUtils,
  System.StrUtils,
  System.IOUTils,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  idURI,
  IdGlobalProtocols,
  IdStack,
  IdGlobal,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  IdMessageClient,
  IdMessage,
  IdMessageBuilder,
  IdAttachment,
  IdMessageParts,
  IdEMailAddress,
  IdAttachmentFile,
  IdSMTPBase,
  IdSMTP,
  IdAttachmentMemory,

  System.Net.URLClient,
  System.Net.HttpClientComponent,
  System.Net.HttpClient,

  Vcl.WinXPickers,
  Vcl.ComCtrls,

  Unit1, Vcl.Imaging.pngimage;

type
  TMainForm = class(TForm)
    sparqlBirthDays: TMemo;
    CacheTimer: TTimer;
    StartTimer: TTimer;
    sparqlDeathDays: TMemo;
    sparqlReleaseDays: TMemo;
    Panel1: TPanel;
    btStart: TButton;
    btStop: TButton;
    btSwagger: TButton;
    edSecret: TEdit;
    lbSecret: TLabel;
    Label1: TLabel;
    lbTMDbAPI: TLabel;
    edSecretBase64: TEdit;
    edTMDbAPI: TEdit;
    btTimer: TButton;
    btRecentProgress: TButton;
    btClear: TButton;
    progDay: TEdit;
    progMonth: TEdit;
    CurrentProgress: TLabel;
    DateTimePicker1: TDateTimePicker;
    DateTimePicker2: TDateTimePicker;
    DateTimePicker3: TDateTimePicker;
    btTop1000: TButton;
    btTop5000: TButton;
    btAll: TButton;
    edtClientVersion: TEdit;
    btUpdateVersion: TButton;
    tmrVersionCheck: TTimer;
    tmrTopUpdate: TTimer;
    Image1: TImage;
    sparqlRelatives: TMemo;
    tmrWaiting: TTimer;
    btInternal: TButton;
    tmrProgress: TTimer;
    ProgressStep: TLabel;
    ProgressDetail: TLabel;
    mmStats: TMemo;
    btClean: TButton;
    ckRegenerate: TCheckBox;
    btRedoc: TButton;
    mmInfo: TMemo;
    btEmail: TButton;
    shapeProgressBG: TShape;
    shapeProgressFG: TShape;
    procedure GetAppVersionString;
    procedure btStartClick(ASender: TObject);
    procedure btStopClick(ASender: TObject);
    procedure FormCreate(ASender: TObject);
    procedure edSecretChange(Sender: TObject);
    procedure CacheTimerTimer(Sender: TObject);
    procedure btRecentProgressClick(Sender: TObject);
    procedure NetHTTPClient1RequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
    procedure ManualRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
    procedure ManualRequestError(const Sender: TObject; const AError: string);
    procedure NetHTTPClient1RequestError(const Sender: TObject; const AError: string);
    procedure btSwaggerClick(Sender: TObject);
    procedure StartTimerTimer(Sender: TObject);
    procedure btTimerClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DateTimePicker1CloseUp(Sender: TObject);
    procedure DateTimePicker2CloseUp(Sender: TObject);
    procedure btTop1000Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function GetProcessThreadCount(ProcessId: Cardinal): Integer;
    procedure btTop5000Click(Sender: TObject);
    procedure btAllClick(Sender: TObject);
    procedure edtClientVersionChange(Sender: TObject);
    procedure btUpdateVersionClick(Sender: TObject);
    procedure UpdateHomeAssistant;
    procedure tmrVersionCheckTimer(Sender: TObject);
    procedure tmrTopUpdateTimer(Sender: TObject);
    procedure DateTimePicker3CloseUp(Sender: TObject);
    procedure tmrWaitingTimer(Sender: TObject);
    procedure btInternalClick(Sender: TObject);
    procedure tmrProgressTimer(Sender: TObject);
    procedure btCleanClick(Sender: TObject);
    procedure btRedocClick(Sender: TObject);
    procedure LogEvent(Details: String);
    procedure LogException(Source: String; EClass: String; EMessage: String; Data: String);
    procedure SendActivityLog(Subject: String);
    procedure btEmailClick(Sender: TObject);
    procedure SetProgressStep(Progress: String);

  public
    Progress: TStringList;
    WaitingMessage: String;
    LastURL: String;

    PersonCacheRequests,
    PersonCacheHit,
    PersonCacheForce,
    PersonCacheMiss,
    PersonCacheAge: Integer;

    MovieCacheRequests,
    MovieCacheHit,
    MovieCacheForce,
    MovieCacheMiss,
    MovieCacheAge: Integer;

    TVShowCacheRequests,
    TVShowCacheHit,
    TVShowCacheForce,
    TVShowCacheMiss,
    TVShowCacheAge: Integer;

    CleanRequests,
    CleanSmall,
    CleanPeople,
    CleanDays,
    CleanMovies,
    CleanTVShows: Integer;
    CleanSize: Int64;
    CleanFiles: Integer;

    AppStartup: TDateTime;
    AppConfiguration: TJSONObject;
    AppBaseURL: String;
    AppURL: String;
    AppSwagger: String;
    AppRedoc: String;
    AppHAURL: String;
    AppHAToken: String;
    LastException: TDateTime;

    MailServerAvailable: Boolean;
    MailServerHost: String;
    MailServerPort: Integer;
    MailServerUser: String;
    MailServerPass: String;
    MailServerFrom: String;
    MailServerName: String;

  strict private
    procedure UpdateGUI;
  end;

  TFancyNetHTTPClient = class(TNetHTTPClient)
  private
    local_Description: String;
    local_CacheFile: String;
    local_URL: String;
  public
    property Description: String read local_Description write local_Description;
    property CacheFile: String read local_CacheFile write local_CacheFile;
    property URL: String read local_URL write local_URL;
  end;


var
  MainForm: TMainForm;
  AppVersionString: String;
  AppVersion: String;
  AppRelease: String;
  MemoryUsage: String;
  MemoryUsageNice: String;

implementation

{$R *.dfm}

resourcestring
  SServerStopped = 'Server stopped';
  SServerStartedAt = 'Server started at ';

{ TMainForm }

procedure TMainForm.btTimerClick(Sender: TObject);
begin
  if btTimer.Tag = 0 then
  begin
    btTimer.Tag := 1;
    btTimer.Caption := 'Disable Timer';
    tmrTopUpdate.Enabled := True;
    tmrVersionCheck.Enabled := True;
  end
  else
  begin
    btTimer.Tag := 0;
    btTimer.Caption := 'Enable Timer';
    tmrTopUpdate.Enabled := False;
    tmrVersionCheck.Enabled := False;
  end;
end;

procedure TMainForm.btTop1000Click(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  LogEvent('');
  LogEvent('Regenerating Top1000 Data');
  CurrentProgress.Caption := 'ManReGenTop1000: '+TGUID.NewGUID.ToString;
  CacheTimer.Enabled := False;

  URL := AppURL+'/ActorInfoService/TopOneThousand';

  // Setup the Request
  URL := URL+'?Secret='+edSecret.Text;
  URL := URL+'&Segment=A';
  URL := URL+'&Progress='+CurrentProgress.Caption;

  // Submit the request (asynchronously)
  Client := TFancyNetHTTPClient.Create(nil);
  Client.Tag := DateTimeToUnix(Now);
  Client.Description := 'Top 1000';
  Client.Asynchronous := True;
  Client.ConnectionTimeout := 3600000;  // 1 hour
  Client.ResponseTimeout := 3600000;  // 1 hour
  Client.onRequestCompleted := ManualRequestCompleted;
  Client.onRequestError := ManualRequestError;
  Client.URL := TidURI.URLEncode(URL);
  if Pos('https', URL) > 0
  then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];
  try
    Client.Get(Client.URL);
  except on E: Exception do
    begin
      LogException('Regen Top1000', E.ClassName, E.Message, Client.URL);
    end;
  end;
end;

procedure TMainForm.btTop5000Click(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  LogEvent('');
  LogEvent('Regenerating Top5000 Data');
  CurrentProgress.Caption := 'ManReGenTop5000: '+TGUID.NewGUID.ToString;
  CacheTimer.Enabled := False;

  URL := AppURL+'/ActorInfoService/TopFiveThousand';

  // Setup the Request
  URL := URL+'?Secret='+edSecret.Text;
  URL := URL+'&Segment=A';
  URL := URL+'&Progress='+CurrentProgress.Caption;

  // Submit the request (asynchronously)
  Client := TFancyNetHTTPClient.Create(nil);
  Client.Tag := DateTimeToUnix(Now);
  Client.Description := 'Top 5000';
  client.Asynchronous := True;
  Client.ConnectionTimeout := 3600000; // 1 hour
  Client.ResponseTimeout := 3600000; // 1 hour
  Client.onRequestCompleted := ManualRequestCompleted;
  Client.onRequestError := ManualRequestError;
  Client.URL := TidURI.URLEncode(URL);
  if Pos('https', URL) > 0
  then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];
  try
    Client.Get(Client.URL);
   except on E: Exception do
     begin
       LogException('Regen Top5000', E.ClassName, E.Message, Client.URL);
     end;
   end;
end;

procedure TMainForm.btUpdateVersionClick(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
  Response: String;
  NewVersion: String;
begin
  URL := 'https://www.actorious.com/ActoriousClient.version';

  // Submit the request (asynchronously)
  Client := TFancyNetHTTPClient.Create(nil);
  Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];
  try
    Response := Client.Get(TidURI.URLEncode(URL)).ContentAsString;
  except on E: Exception do
    begin
      LogException('Version Check', E.ClassName, E.Message, URL);
    end;
  end;

  if (Pos('ActoriousClient', Response) > 0) then
  begin
    // Just want the last digits
    Response := Trim(Response);
    NewVersion := Copy(Response,Pos('ActoriousClient_',Response)+16,length(Response));
    NewVersion := Copy(NewVersion,1,Pos('";',NewVersion)-1);
    NewVersion := StringReplace(NewVersion,'_','.',[rfReplaceAll]);
    if (edtClientVersion.Text <> NewVersion) then
    begin
      edtClientVersion.Text := NewVersion;
      LogEvent('- Actorious Client Version Updated: '+NewVersion);
    end;
  end;

  Client.Free;
  UpdateHomeAssistant;
end;

procedure TMainForm.UpdateHomeAssistant;
var
  Client: TNetHTTPClient;
  URL: String;
  Token: String;
  Endpoint: String;

  Data: TStringStream;
  Response: String;

begin

  // NOTE: AppStartup, MemoryUsage, AppVersion and AppRelease are Form Variables defined elsewhere

  // Decide if you're going to use a Home Assistant Internal vs. External URL
  // And that they might differ in whether SSL is used
  URL := AppHAURL;
  Token := AppHAToken;

  if AppHAURL <> '' then
  begin

    // Setup the Main Request
    Client := TNetHTTPClient.Create(nil);
    if Pos('https', URL) > 0
    then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];
    Client.ContentType := 'application/json';
    Client.CustomHeaders['Authorization'] := 'Bearer '+Token;

    try

      Endpoint := '/api/states/sensor.actorious_server_start';
      Data := TStringStream.Create('{"state": "'+FormatDateTime('mmm dd (ddd) hh:nn', AppStartup)+'" }');
      Response := Client.Post(URL+Endpoint, Data).ContentAsString;
      if Pos('"entity_id"', Response) = 0 then LogEvent(Response);
      Data.Free();

      Endpoint := '/api/states/sensor.actorious_server_runtime';
      Data := TStringStream.Create('{"state": "'+IntToStr(DaysBetween(Now, AppStartup))+'d '+FormatDateTime('h"h "n"m"', Now-AppStartup)+'" }');
      Response := Client.Post(URL+Endpoint, Data).ContentAsString;
      if Pos('"entity_id"', Response) = 0 then LogEvent(Response);
      Data.Free();

      Endpoint := '/api/states/sensor.actorious_server_version';
      Data := TStringStream.Create('{"state": "'+AppVersion+'" }');
      Response := Client.Post(URL+Endpoint, Data).ContentAsString;
      if Pos('"entity_id"', Response) = 0 then LogEvent(Response);
      Data.Free();

      Endpoint := '/api/states/sensor.actorious_server_release';
      Data := TStringStream.Create('{"state": "'+AppRelease+'" }');
      Response := Client.Post(URL+Endpoint, Data).ContentAsString;
      if Pos('"entity_id"', Response) = 0 then LogEvent(Response);
      Data.Free();

      Endpoint := '/api/states/sensor.actorious_server_memory';
      Data := TStringStream.Create('{"state":'+MemoryUsage+', "attributes":{"unit_of_measurement":"MB"}}');
      Response := Client.Post(URL+Endpoint, Data).ContentAsString;
      if Pos('"entity_id"', Response) = 0 then LogEvent(Response);
      Data.Free();

      Endpoint := '/api/states/sensor.actorious_server_memory_nice';
      Data := TStringStream.Create('{"state":"'+MemoryUsageNice+'", "attributes":{"unit_of_measurement":"MB"}}');
      Response := Client.Post(URL+Endpoint, Data).ContentAsString;
      if Pos('"entity_id"', Response) = 0 then LogEvent(Response);
      Data.Free();

      Endpoint := '/api/states/sensor.actorious_server_working_date';
      Data := TStringStream.Create('{"state":"'+ProgMonth.text+' '+ProgDay.Text+'"}');
      Response := Client.Post(URL+Endpoint, Data).ContentAsString;
      if Pos('"entity_id"', Response) = 0 then LogEvent(Response);
       Data.Free();

      Client.Free;

    except on E: Exception do
      begin
        LogException('Update Home Assistant', E.ClassName, E.Message, URL+Endpoint);
      end;
    end;

  end;

end;

procedure TMainForm.btAllClick(Sender: TObject);
var
  i: integer;
  d: TJSONObject;
  d1,
  d2,
  d3: String;
begin
  // What are the last 20 progress items recorded?
//  if Progress.Count > 0 then
//  begin
    mmInfo.DisableAlign;
    mmInfo.Lines.BeginUpdate;
    LogEvent('______________________________________________________');
    LogEvent('EXTERNAL PROGRESS INFORMATION');

    // What are the last 50 progress items recorded?
    for i := Progress.Count -1 downto 0 do
    begin
      try
        d := TJSONObject.ParseJSONValue(Progress[i]) as TJSONObject;
        if ((Pos('::1'           ,(d.getValue('IP') as TJSONString).Value) = 0)  and
            (Pos('174.7.120.10'  ,(d.getValue('IP') as TJSONString).Value) = 0)  and
            (Pos('23.111.75.19'  ,(d.getValue('IP') as TJSONString).Value) = 0)) then
        begin
          d1 := (d.getValue('ST') as TJSONString).Value;
          d2 := FormatDateTime('hh:nn:ss.zzz',(d.getValue('TP') as TJSONNumber).AsDouble-(d.getValue('TS') as TJSONNumber).AsDouble);

          d3 := '';
          if (d.getValue('DY') <> nil)
          then d3 := D3 +'DY:'+(d.getValue('DY') as TJSONString).Value.PadLeft(4)+'  ';
          if (d.getValue('DT') <> nil)
          then d3 := D3 +'DT:'+(d.getValue('DT') as TJSONString).Value.PadLeft(6)+'  ';

          LogEvent(
            d1+'  '+
            d2+'  '+
            (d.getValue('IP') as TJSONString).Value.PadRight(17)+
            (d.getValue('RQ') as TJSONString).Value.PadRight(16)+'  '+
            d3.PadRight(20)+
            (d.getValue('PR') as TJSONString).Value
          );
        end;
        d.Free;
      except on E: Exception do
        begin
          LogException('External Progress', E.ClassName, E.Message, Progress[i]);
        end;
      end;
    end;

    LogEvent('');
    mmInfo.Lines.EndUpdate;
    mmInfo.EnableAlign;
//  end;
//  mmInfo.scrolltoBottom;
//  mmInfo.Repaint;
end;

procedure TMainForm.btRecentProgressClick(Sender: TObject);
var
  i: Integer;
  ProgressSize: Integer;
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
  mem1: UInt64;
  mem2: UInt64;
begin
  if (Sender is TButton) then
  begin
    if tmrProgress.Interval = 1000
    then tmrProgress.Interval := 10000
    else tmrProgress.Interval := 1000;
  end;

  mmStats.Lines.BeginUpdate;
  mmStats.Clear;

  mmStats.Lines.Add('');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('  SERVER INFORMATION ');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  '+AppVersionString);
  mmStats.Lines.Add('  Running on '+GetEnvironmentVariable('COMPUTERNAME'));
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  Started: '+FormatDateTime('yyyy-mm-dd HH:nn:ss', AppStartup));
  mmStats.Lines.Add('  RunTime: '+IntToStr(DaysBetween(Now, AppStartup))+'d '+FormatDateTime('HH:nn:ss', Now-AppStartup));
  mmStats.Lines.Add('');

{$WARN SYMBOL_PLATFORM OFF}
  GetMemoryManagerState(st);
  mem1 := 0;
  for sb in st.SmallBlockTypeStates do
    mem1 := mem1 + (sb.UseableBlockSize * sb.AllocatedBlockCount);
  mem2 := mem1 + st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
{$WARN SYMBOL_PLATFORM ON}

  mmStats.Lines.Add('  Memory (Large  ): '+FloatToStrF(st.TotalAllocatedLargeBlockSize/1024/1024,ffNumber,10,3).PadLeft(9)+' MB');
  mmStats.Lines.Add('  Memory (Medium ): '+FloatToStrF(st.TotalAllocatedMediumBlockSize/1024/1024,ffNumber,10,3).PadLeft(9)+' MB');
  mmStats.Lines.Add('  Memory (Small  ): '+FloatToStrF(mem1/1024/1024,ffNumber,10,3).PadLeft(9)+' MB');
  mmStats.Lines.Add('  Memory (Total  ): '+FloatToStrF(mem2/1024/1024,ffNumber,10,3).PadLeft(9)+' MB');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  Threads: '+IntToStr(GetProcessThreadCount(GetCurrentProcessId)));

  MemoryUsage := FloatToStrF(mem2/1024/1024,ffFixed,10,3);
  MemoryUsageNice := FloatToStrF(mem2/1024/1024,ffNumber,10,1);

  // Whoa - something has fallen off the rails, so abort and try again:  Limit set at 5 GB
  if ((mem2/1024/1024) > ( 5 * 1024)) then
  begin
    SendActivityLog('Resource Limit Restart');
    Halt;
  end;

  // How much memory is our progress data taking up?
  ProgressSize := 0;
  for i := 0 to Progress.Count-1 do
    ProgressSize := ProgressSize + Length(Progress[i]);
  mmStats.Lines.Add('  History: '+IntToStr(Progress.Count)+' entries / '+FloatToStrF(ProgressSize/1024,ffNumber,10,1)+' KB');

  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('  PERSON CACHE INFORMATION');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  Person Cache Requests:'+FloatToStrF(PersonCacheRequests,ffNumber,9,0).PadLeft(9));
  if (PersonCacheRequests > 0) then
  begin
    mmStats.Lines.Add('  Person Cache Hits:    '+FloatToStrF(PersonCacheHit,ffNumber,9,0).PadLeft(9)  +FloatToStrF(100.0*PersonCacheHit/PersonCacheRequests,ffNumber,6,1).PadLeft(6)  +' %');
    mmStats.Lines.Add('  Person Cache Misses:  '+FloatToStrF(PersonCacheMiss,ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*PersonCacheMiss/PersonCacheRequests,ffNumber,6,1).PadLeft(6) +' %');
    mmStats.Lines.Add('  Person Cache Aged:    '+FloatToStrF(PersonCacheAge,ffNumber,9,0).PadLeft(9)  +FloatToStrF(100.0*PersonCacheAge/PersonCacheRequests,ffNumber,6,1).PadLeft(6)  +' %');
    mmStats.Lines.Add('  Person Cache Force:   '+FloatToStrF(PersonCacheForce,ffNumber,9,0).PadLeft(9)+FloatToStrF(100.0*PersonCacheForce/PersonCacheRequests,ffNumber,6,1).Padleft(6)+' %');
  end;

  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('  MOVIE CACHE INFORMATION');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  Movie Cache Requests: '+FloatToStrF(MovieCacheRequests,ffNumber,9,0).PadLeft(9));
  if (MovieCacheRequests > 0) then
  begin
    mmStats.Lines.Add('  Movie Cache Hits:     '+FloatToStrF(MovieCacheHit,ffNumber,9,0).PadLeft(9)  +FloatToStrF(100.0*MovieCacheHit/MovieCacheRequests,ffNumber,6,1).PadLeft(6)  +' %');
    mmStats.Lines.Add('  Movie Cache Misses:   '+FloatToStrF(MovieCacheMiss,ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*MovieCacheMiss/MovieCacheRequests,ffNumber,6,1).PadLeft(6) +' %');
    mmStats.Lines.Add('  Movie Cache Aged:     '+FloatToStrF(MovieCacheAge,ffNumber,9,0).PadLeft(9)  +FloatToStrF(100.0*MovieCacheAge/MovieCacheRequests,ffNumber,6,1).PadLeft(6)  +' %');
    mmStats.Lines.Add('  Movie Cache Force:    '+FloatToStrF(MovieCacheForce,ffNumber,9,0).PadLeft(9)+FloatToStrF(100.0*MovieCacheForce/MovieCacheRequests,ffNumber,6,1).Padleft(6)+' %');
  end;

  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('  TVSHOW CACHE INFORMATION');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  TVShow Cache Requests:'+FloatToStrF(TVShowCacheRequests,ffNumber,9,0).PadLeft(9));
  if (TVShowCacheRequests > 0) then
  begin
    mmStats.Lines.Add('  TVShow Cache Hits:    '+FloatToStrF(TVShowCacheHit,ffNumber,9,0).PadLeft(9)  +FloatToStrF(100.0*TVShowCacheHit/TVShowCacheRequests,ffNumber,6,1).PadLeft(6)  +' %');
    mmStats.Lines.Add('  TVShow Cache Misses:  '+FloatToStrF(TVShowCacheMiss,ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*TVShowCacheMiss/TVShowCacheRequests,ffNumber,6,1).PadLeft(6) +' %');
    mmStats.Lines.Add('  TVShow Cache Aged:    '+FloatToStrF(TVShowCacheAge,ffNumber,9,0).PadLeft(9)  +FloatToStrF(100.0*TVShowCacheAge/TVShowCacheRequests,ffNumber,6,1).PadLeft(6)  +' %');
    mmStats.Lines.Add('  TVShow Cache Force:   '+FloatToStrF(TVShowCacheForce,ffNumber,9,0).PadLeft(9)+FloatToStrF(100.0*TVShowCacheForce/TVShowCacheRequests,ffNumber,6,1).Padleft(6)+' %');
  end;

  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('  CACHE CLEANING INFORMATION');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  Cache Clean Requests: '+FloatToStrF(CleanRequests,ffNumber,9,0).PadLeft(9));
  if (CleanRequests > 0) then
  begin
    mmStats.Lines.Add('  Cache Clean Days:     '+FloatToStrF(CleanDays,    ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*CleanDays    /CleanRequests,ffNumber,6,1).PadLeft(6)+' %');
    mmStats.Lines.Add('  Cache Clean People:   '+FloatToStrF(CleanPeople,  ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*CleanPeople  /CleanRequests,ffNumber,6,1).PadLeft(6)+' %');
    mmStats.Lines.Add('  Cache Clean Movies:   '+FloatToStrF(CleanMovies,  ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*CleanMovies  /CleanRequests,ffNumber,6,1).PadLeft(6)+' %');
    mmStats.Lines.Add('  Cache Clean TVShows:  '+FloatToStrF(CleanTVShows, ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*CleanTVShows /CleanRequests,ffNumber,6,1).PadLeft(6)+' %');
    mmStats.Lines.Add('  Cache Clean Files:    '+FloatToStrF(CleanFiles,   ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*CleanFiles   /CleanRequests,ffNumber,6,1).PadLeft(6)+' %');
    mmStats.Lines.Add('  Cache Clean Small:    '+FloatToStrF(CleanSmall,   ffNumber,9,0).PadLeft(9) +FloatToStrF(100.0*CleanSmall   /CleanRequests,ffNumber,6,1).PadLeft(6)+' %');
    mmStats.Lines.Add('  Cache Clean Size:     '+FloatToStrF(CleanSize/(1024*1024),ffNumber,9,0).PadLeft(9) +' MB');
  end;

  mmStats.Lines.EndUpdate;
end;

procedure TMainForm.btRedocClick(Sender: TObject);
var
  URL: String;
begin
  URL := AppRedoc;
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.btStartClick(ASender: TObject);
begin
  ServerContainer.SparkleHttpSysDispatcher.Start;
  UpdateGUI;
end;

procedure TMainForm.btStopClick(ASender: TObject);
begin
  ServerContainer.SparkleHttpSysDispatcher.Stop;
  UpdateGUI;
end;

procedure TMainForm.btSwaggerClick(Sender: TObject);
var
  URL: String;
begin
  URL := AppSwagger;
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;


procedure TMainForm.GetAppVersionString;
var
  verblock:PVSFIXEDFILEINFO;
  versionMS,versionLS:cardinal;
  verlen:cardinal;
  rs:TResourceStream;
  m:TMemoryStream;
  p:pointer;
  s:cardinal;
  ReleaseDate: TDateTime;
begin
  // Lot of work to just get the Application Version Information
  m:=TMemoryStream.Create;
  try
    rs:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      m.CopyFrom(rs,rs.Size);
    finally
      rs.Free;
    end;
    m.Position:=0;
    if VerQueryValue(m.Memory,'\',pointer(verblock),verlen) then
      begin
        VersionMS:=verblock.dwFileVersionMS;
        VersionLS:=verblock.dwFileVersionLS;
//        AppVersionString:=Application.Title+' '+
        AppVersion :=
          IntToStr(versionMS shr 16)+'.'+
          IntToStr(versionMS and $FFFF)+'.'+
          IntToStr(VersionLS shr 16)+'.'+
          IntToStr(VersionLS and $FFFF);
      end;
    if VerQueryValue(m.Memory,PChar('\\StringFileInfo\\'+
      IntToHex(GetThreadLocale,4)+IntToHex(GetACP,4)+'\\FileDescription'),p,s) or
        VerQueryValue(m.Memory,'\\StringFileInfo\\040904E4\\FileDescription',p,s) then //en-us
          AppVersionString:=PChar(p)+' v'+AppVersion;
  finally
    m.Free;
  end;
  Application.Title := AppVersionString;
  FileAge(ParamStr(0), ReleaseDate);
  AppRelease := FormatDateTime('yyyy-MMM-dd', ReleaseDate);
  Caption :=  'Actorious XData Server     Ver '+AppVersion+'     Rel '+AppRelease;

end;

function TMainForm.GetProcessThreadCount(ProcessId: Cardinal): Integer;
var
  SnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;
  CanContinue: Boolean;
begin
  Result := 0;
  SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapshotHandle <> INVALID_HANDLE_VALUE then
    try
      ProcessEntry32.dwSize := SizeOf(ProcessEntry32);
      CanContinue := Process32First(SnapshotHandle, ProcessEntry32);
      while CanContinue do
      begin
        if ProcessEntry32.th32ProcessID = ProcessId then
        begin
          Result := ProcessEntry32.cntThreads;
          Exit;
        end;
        CanContinue := Process32Next(SnapshotHandle, ProcessEntry32);
      end;
    finally
      CloseHandle(SnapshotHandle);
    end;
end;

procedure TMainForm.LogEvent(Details: String);
begin
  try
    mmInfo.Lines.Add(FormatDateTime('yyyy-mm-dd HH:nn:ss.zzz', Now)+'  '+Details);
    SendMessage(mmInfo.Handle, EM_LINESCROLL, 0, mmInfo.Lines.Count);
  except on E: Exception do
    begin
    end;
  end;
end;

procedure TMainForm.LogException(Source, EClass, EMessage, Data: String);
begin
  LogEvent('');
  LogEvent('[ EXCEPTION ] '+Source);
  LogEvent('[ '+EClass+' ] '+EMessage);
  LogEvent('[ Data ] '+Data);

  if (MinutesBetween(now, LastException) > 60) and (Source <> 'Update Home Assistant') then
  begin
    LastException := Now;
    SendActivityLog('Exception Detected');
  end;

end;

procedure TMainForm.btCleanClick(Sender: TObject);
var
  CleanTime :TDateTime;
  CleanData: Int64;
  CleanNum: Integer;
  OlderThan: TDateTime;
  CacheDir: String;
  i: Integer;

  function CleanDir: Integer;
  var
    FileName: String;
    FileSize: Int64;
  begin
    Result := 0;
    if DirectoryExists(CacheDir) then
    begin
      for FileName in TDirectory.GetFiles(CacheDir, '*') do
      begin
        CleanRequests := CleanRequests + 1;
        if (TFile.GetLastWriteTime(FileName) < OlderThan) or (Pos('json.working',Filename) > 0) or (FileSizeByName(FileName) < 120) then
        begin
          FileSize := FileSizeByName(FileName);
          Application.ProcessMessages;
          Result := Result + 1;
          CleanSize := CleanSize + FileSize;
          CleanFiles := CleanFiles + 1;
          TFile.Delete(FileName);

          if FileSize < 250
          then CleanSmall := CleanSmall + 1;

        end;
      end;
    end;
  end;


begin
  CleanTime := Now;
  CleanData := 0;
  CleanNum := 0;

  CleanRequests := 0;
  CleanSmall := 0;
  CleanDays := 0;
  CleanPeople := 0;
  CleanMovies := 0;
  CleanTVShows := 0;
  CleanSize := 0;
  CleanFiles := 0;

  LogEvent('');
  LogEvent('Cache Clean Started');

  // Cleaning anything older than 10 days
  OlderThan := Now() - 10;

  // Days
  LogEvent('- Cleaning Days');
  CacheDir := 'cache\days\actorious-births';
  CleanDays := CleanDays + CleanDir;
  CacheDir := 'cache\days\actorious-deaths';
  CleanDays := CleanDays + CleanDir;
  CacheDir := 'cache\days\actorious-releases';
  CleanDays := CleanDays + CleanDir;
  CacheDir := 'cache\days\first';
  CleanDays := CleanDays + CleanDir;
  CacheDir := 'cache\days\wikidata-births';
  CleanDays := CleanDays + CleanDir;
  CacheDir := 'cache\days\wikidata-deaths';
  CleanDays := CleanDays + CleanDir;
  CacheDir := 'cache\days\wikidata-releases';
  CleanDays := CleanDays + CleanDir;
  btRecentProgressClick(nil);

  // People
  LogEvent('- Cleaning People/Top');
  CacheDir := 'cache\people\top1000';
  CleanPeople := CleanPeople + CleanDir;
  CacheDir := 'cache\people\top5000';
  CleanPeople := CleanPeople + CleanDir;
  btRecentProgressClick(nil);

  LogEvent('- Cleaning People/Actorious');
  for i :=  0 to 999 do
  begin
    CacheDir := 'cache\people\actorious\'+RightStr('000'+IntToStr(i),3);
    try
      CleanPeople := CleanPeople + CleanDir;
    except on E: Exception do
      begin
        LogException('Cleaning People/Actorious', E.ClassName, E.Message, CacheDir);
      end;
    end;
    Application.ProcessMessages;
    btRecentProgressClick(nil);
  end;

  LogEvent('- Cleaning People/TMDb');
  for i :=  0 to 999 do
  begin
    CacheDir := 'cache\people\tmdb\'+RightStr('000'+IntToStr(i),3);
    try
      CleanPeople := CleanPeople + CleanDir;
    except on E: Exception do
      begin
        LogException('Cleaning People/TMDb', E.ClassName, E.Message, CacheDir);
      end;
    end;
    Application.ProcessMessages;
    btRecentProgressClick(nil);
  end;


  // Movies
  LogEvent('- Cleaning Movies/Top');
  CacheDir := 'cache\movies\top1000';
  CleanPeople := CleanPeople + CleanDir;
  CacheDir := 'cache\movies\top5000';
  CleanPeople := CleanPeople + CleanDir;
  btRecentProgressClick(nil);

  LogEvent('- Cleaning Movies/Actorious');
  for i :=  0 to 999 do
  begin
    CacheDir := 'cache\movies\actorious\'+RightStr('000'+IntToStr(i),3);
    try
      CleanPeople := CleanPeople + CleanDir;
    except on E: Exception do
      begin
        LogException('Cleaning Movies/Actorious', E.ClassName, E.Message, CacheDir);
      end;
    end;
    Application.ProcessMessages;
    btRecentProgressClick(nil);
  end;

  LogEvent('- Cleaning Movies/TMDb');
  for i :=  0 to 999 do
  begin
    CacheDir := 'cache\movies\tmdb\'+RightStr('000'+IntToStr(i),3);
    try
      CleanMovies := CleanMovies + CleanDir;
    except on E: Exception do
      begin
        LogException('Cleaning Movies/TMDb', E.ClassName, E.Message, CacheDir);
      end;
    end;
    Application.ProcessMessages;
    btRecentProgressClick(nil);
  end;


  // TV Shows
  LogEvent('- Cleaning TVShows/Top');
  CacheDir := 'cache\tvshows\top1000';
  Cleantvshows := Cleantvshows + CleanDir;
  CacheDir := 'cache\tvshows\top5000';
  Cleantvshows := Cleantvshows + CleanDir;
  btRecentProgressClick(nil);

  LogEvent('- Cleaning TVShows/Actorious');
  for i :=  0 to 999 do
  begin
    CacheDir := 'cache\tvshows\actorious\'+RightStr('000'+IntToStr(i),3);
    try
      CleanTVShows := CleanTVShows + CleanDir;
    except on E: Exception do
      begin
        LogException('Cleaning TVShows/Actorious', E.ClassName, E.Message, CacheDir);
      end;
    end;
    Application.ProcessMessages;
    btRecentProgressClick(nil);
  end;

  LogEvent('- Cleaning TVShows/TMDb');
  for i :=  0 to 999 do
  begin
    CacheDir := 'cache\tvshows\tmdb\'+RightStr('000'+IntToStr(i),3);
    try
      Cleantvshows := Cleantvshows + CleanDir;
    except on E: Exception do
      begin
        LogException('Cleaning TVShows/TMDb', E.ClassName, E.Message, CacheDir);
      end;
    end;
    Application.ProcessMessages;
    btRecentProgressClick(nil);
  end;

  LogEvent('Cache Clean Completed: '+FloatToStrF(CleanFiles-CleanNum,ffNumber,8,0)+' Files, '+FloatToStrF((CleanSize - CleanData)/(1024*1024),ffNumber,8,0)+' MB ('+FormatDateTime('hh:nn:ss.zzz',Now-CleanTime)+')');

end;

procedure TMainForm.btClearClick(Sender: TObject);
begin
  mmInfo.Clear;
end;

procedure TMainForm.btEmailClick(Sender: TObject);
begin
  SendActivityLog('Activity Log');
end;

procedure TMainForm.btInternalClick(Sender: TObject);
var
  i: integer;
  d: TJSONObject;
  d1,
  d2,
  d3: String;
  PR: String;
begin
  // What are the last 20 progress items recorded?
//  if Progress.Count > 0 then
//  begin
    mmInfo.DisableAlign;
    mmInfo.Lines.BeginUpdate;
    LogEvent('______________________________________________________');
    LogEvent('INTERNAL PROGRESS INFORMATION');

    // What are the last 50 progress items recorded?
    for i := Progress.Count -1 downto 0 do
    begin
      try
        d := TJSONObject.ParseJSONValue(Progress[i]) as TJSONObject;
        if ((Pos('::1'           ,(d.getValue('IP') as TJSONString).Value) > 0)  or
            (Pos('174.7.120.10'  ,(d.getValue('IP') as TJSONString).Value) > 0)  or
            (Pos('23.111.75.19'  ,(d.getValue('IP') as TJSONString).Value) > 0)) then
        begin
          d1 := (d.getValue('ST') as TJSONString).Value;
          d2 := FormatDateTime('hh:nn:ss.zzz',(d.getValue('TP') as TJSONNumber).AsDouble-(d.getValue('TS') as TJSONNumber).AsDouble);

          d3 := '';
          if (d.getValue('DY') <> nil)
          then d3 := D3 +'DY:'+(d.getValue('DY') as TJSONString).Value.PadLeft(4)+'  ';
          if (d.getValue('DT') <> nil)
          then d3 := D3 +'DT:'+(d.getValue('DT') as TJSONString).Value.PadLeft(6)+'  ';

          PR := '[No Report]';
          if d.getValue('PR') <> nil
          then PR := '['+(d.getValue('PR') as TJSONString).Value+']';

          LogEvent(
            d1+'  '+
            d2+'  '+
            (d.getValue('RQ') as TJSONString).Value.PadRight(16)+'  '+
            d3.PadRight(20)+
            PR
          );
        end;
        d.Free;
      except on E: Exception do
        begin
          LogException('Internal Progress', E.ClassName, E.Message, Progress[i]);
        end;
      end;
    end;

    LogEvent('');
    mmInfo.Lines.EndUpdate;
    mmInfo.EnableAlign;
//  end;
//  mmInfo.scrolltoBottom;
//  mmInfo.Repaint;
end;

procedure TMainForm.CacheTimerTimer(Sender: TObject);
var
  CacheDate: TDate;
  CacheIndex: Integer;
  CacheFile: String;
  Client: TFancyNetHTTPClient;
  URL: String;
  Update: String;
  QuietWindow: TTime;
begin
  CacheTimer.Enabled := False;

  if btTimer.Tag = 1 then
  begin

    // First check and see if we're in a 'quiet time' and want to pause and
    // let the Top1000 and Top5000 functions operate without interferenece
    QuietWindow := TimeOf(Now);
    if ((QuietWindow >= EncodeTime( 5,0,0,0)) and (QuietWindow <= EncodeTime( 5,59,59,0))  or
        (QuietWindow >= EncodeTime(17,0,0,0)) and (QuietWindow <= EncodeTime(17,59,59,0))) then
    begin
      CurrentProgress.Caption := 'Waiting for Top Refresh to Complete (Retry in 60s)';
      CacheTimer.Interval := 60000; // Wait 1 minute and check again
      CacheTimer.Enabled := True;

      WaitingMessage := 'Waiting for Top Refresh to Complete (Retry in %s)';
      tmrWaiting.Tag := 60;
      tmrWaiting.Enabled := True;
      Exit;
    end;

    // We store the next date to check in the Timer's tag property
    CacheIndex := CacheTimer.Tag;

    // If somehow it is less than Jan 01, set it to Jan 01
    if CacheIndex < 1 then
    begin
      CacheIndex := 1;
      CacheTimer.Tag := 1;
    end;

    // If somehow it is greater than Dec 31, set it to Jan 01
    if CacheIndex > 366 then
    begin
      CacheIndex := 1;
      CacheTimer.Tag := 1;
    end;

    // Get the actual date, update the "now serving" part of the form
    CacheDate := EncodeDate(2020, 1, 1)+(CacheIndex - 1);
    progMonth.Text := FormatDateTime('mmm', CacheDate);
    progDay.Text := FormatDateTime('dd', CacheDate);

    // We're deciding whether to do work based on whether a cached file exists and is current
    Update := 'Waiting';

    // BirthDay Data
    if (update = 'Waiting') then
    begin
      CacheFile := 'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
      if not(FileExists(CacheFile+'.working')) then
      begin
        if not(FileExists(CacheFile)) then
        begin
          Update := 'Generating BirthDay'
        end
        else
        begin
          if TFile.GetLastWriteTime(CacheFile) < (Now - 5)
          then Update := 'Refreshing BirthDay';
        end;
      end;
    end;

    // DeathDay Data
    if (update = 'Waiting') then
    begin
      CacheFile := 'cache/days/actorious-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
      if not(FileExists(CacheFile+'.working')) then
      begin
        if not(FileExists(CacheFile)) then
        begin
          Update := 'Generating DeathDay'
        end
        else
        begin
          if TFile.GetLastWriteTime(CacheFile) < (Now - 5)
          then Update := 'Refreshing DeathDay';
        end;
      end;
    end;

    // ReleaseDay Data
    if (update = 'Waiting') then
    begin
      CacheFile := 'cache/days/actorious-releases/releaseday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
      if not(FileExists(CacheFile+'.working')) then
      begin
        if not(FileExists(CacheFile)) then
        begin
          Update := 'Generating Releases'
        end
        else
        begin
          if TFile.GetLastWriteTime(CacheFile) < (Now - 5)
          then Update := 'Refreshing Releases';
        end;
      end;
    end;

    if (Update <> 'Waiting') then
    begin

      // Set a timer so we can track how long it is taking
      CurrentProgress.Caption := Update+': '+TGUID.NewGUID.ToString;

      // Enable as indication we're working
      progMonth.Enabled := True;
      progDay.Enabled := True;

      // Change URL of server depending on machine it is running on
      if (Pos('BirthDay',Update) > 0) then
      begin
        URL := AppURL+'/ActorInfoService/ActorBirthDay';
      end
      else if (Pos('DeathDay',Update) > 0) then
      begin
        URL := AppURL+'/ActorInfoService/ActorDeathDay';
      end
      else if (Pos('Releases',Update) > 0) then
      begin
        URL := AppURL+'/ActorInfoService/MovieReleaseDay';
      end;


      // Setup the Request
      URL := URL+'?Secret='+edSecret.Text;
      URL := URL+'&aMonth='+IntToStr(MonthOf(CacheDate));
      URL := URL+'&aDay='+IntToStr(DayOf(CacheDate));
      URL := URL+'&Progress='+CurrentProgress.Caption;

      // sometiems we can get in a loop if a date isn't working, in which case we want to skip
      // this and move on to the next date.
      if URL <> LastURL then
      begin
        LastURL := URL;

        // Submit the request (asynchronously)
        Client := TFancyNetHTTPClient.Create(nil);
        Client.Tag := DateTimeToUnix(Now);
        Client.Description := Update+' for '+FormatDateTime('mmmdd',CacheDate)+' / d'+IntToStr(CacheIndex);
        Client.CacheFile := CacheFile;
        Client.Asynchronous := True;
        Client.ConnectionTimeout := 3600000;  // 60 minutes
        Client.ResponseTimeout := 3600000;    // 60 minutes
        Client.onRequestCompleted := NetHTTPClient1RequestCompleted;
        Client.onRequestError := NetHTTPClient1RequestError;
        Client.URL := TidURI.URLEncode(URL);

        if Pos('https', URL) > 0
        then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

        // Write out a file indicating something is being worked on
        Progress.SaveToFile(Client.CacheFile+'.working');

        try
          Client.Get(TidURI.URLEncode(URL));
        except on E: Exception do
          begin
            LogException(Client.Description, E.ClassName, E.Message, Client.URL);
          end;
        end;
      end
      else
      begin
        // If already processed, let's check the next one
        CacheTimer.Tag := CacheTimer.Tag + 1;
        CacheTimer.Interval := 1000;
        CacheTimer.Enabled := True;
        CurrentProgress.Caption := 'Scanning';
      end
    end
    else
    begin
      // If already processed, let's check the next one
      CacheTimer.Tag := CacheTimer.Tag + 1;
      CacheTimer.Interval := 1000;
      CacheTimer.Enabled := True;
      CurrentProgress.Caption := 'Scanning';
    end
  end
  else
  begin
    // Disabled, so wait a minute and check again
    CurrentProgress.Caption := 'Timer Disabled (Retry in 30s)';
    CacheTimer.Interval := 30000;
    CacheTimer.Enabled := True;

    WaitingMessage := 'Timer Disabled (Retry in %s)';
    tmrWaiting.Tag := 30;
    tmrWaiting.Enabled := True;
  end;

end;

procedure TMainForm.DateTimePicker1CloseUp(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  if (DateTimePicker1.Tag <> DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePicker1.Date),DayOfTheMonth(DateTimePicker1.Date)))) then
  begin
    DateTimePicker1.Tag := DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePicker1.Date),DayOfTheMonth(DateTimePicker1.Date)));
    CacheTimer.Tag := DateTimePicker1.Tag;
    LogEvent('');
    LogEvent('Regenerating BirthDay Data [ '+FormatDateTime('mmmdd',DateTimePicker1.Date)+' / d'+IntToStr(DateTimePicker1.Tag)+' ]');
    CurrentProgress.Caption := 'ManReGenBirthDay['+FormatDateTime('mmmdd',DateTimePicker1.Date)+'/d'+IntToStr(DateTimePicker1.Tag)+']: '+TGUID.NewGUID.ToString;
    CacheTimer.Enabled := False;

    progMonth.Text := FormatDateTime('mmm', DateTimePicker1.date);
    progDay.Text := FormatDateTime('dd', DateTimePicker1.date);
    progMonth.Enabled := True;
    progDay.Enabled := True;
    CacheTimer.Tag := DayOfTheYear(DateTimePicker1.Date);

    URL := AppURL+'/ActorInfoService/ActorBirthDay';

    // Setup the Request
    URL := URL+'?Secret='+edSecret.Text;
    URL := URL+'&aMonth='+IntToStr(MonthOf(DateTimePicker1.Date));
    URL := URL+'&aDay='+IntToStr(DayOf(DateTimePicker1.Date));
    URL := URL+'&Progress='+CurrentProgress.Caption;

    // Submit the request (asynchronously)
    Client := TFancyNetHTTPClient.Create(nil);
    Client.Tag := DateTimeToUnix(Now);
    Client.Description := 'BirthDay:'+FormatDateTime('mmmdd',DateTimePicker1.Date)+'/d'+IntToStr(DateTimePicker1.Tag);
    client.Asynchronous := True;
    Client.ConnectionTimeout := 1200000;
    Client.ResponseTimeout := 1200000;
    Client.onRequestCompleted := NetHTTPClient1RequestCompleted;
    Client.onRequestError := NetHTTPClient1RequestError;
    Client.URL := TidURI.URLEncode(URL);
    if Pos('https', URL) > 0
    then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    try
      Client.Get(Client.URL);
     except on E: Exception do
       begin
         LogException(Client.Description, E.ClassName, E.Message, Client.URL);
       end;
     end;
  end;
end;

procedure TMainForm.DateTimePicker2CloseUp(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  if (DateTimePicker2.Tag <> DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePicker2.Date),DayOfTheMonth(DateTimePicker2.Date)))) then
  begin
    DateTimePicker2.Tag := DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePicker2.Date),DayOfTheMonth(DateTimePicker2.Date)));
    CacheTimer.Tag := DateTimePicker2.Tag;
    LogEvent('');
    LogEvent('Regenerating DeathDay Data [ '+FormatDateTime('mmmdd',DateTimePicker2.Date)+' / d'+IntToStr(DateTimePicker2.Tag)+' ]');
    CurrentProgress.Caption := 'ManReGenDeathDay['+FormatDateTime('mmmdd',DateTimePicker2.Date)+'/d'+IntToStr(DateTimePicker2.Tag)+']: '+TGUID.NewGUID.ToString;
    CacheTimer.Enabled := False;

    progMonth.Text := FormatDateTime('mmm', DateTimePicker2.date);
    progDay.Text := FormatDateTime('dd', DateTimePicker2.date);
    progMonth.Enabled := True;
    progDay.Enabled := True;
    CacheTimer.Tag := DayOfTheYear(DateTimePicker2.Date);

    URL := AppURL+'/ActorInfoService/ActorDeathDay';

    // Setup the Request
    URL := URL+'?Secret='+edSecret.Text;
    URL := URL+'&aMonth='+IntToStr(MonthOf(DateTimePicker2.Date));
    URL := URL+'&aDay='+IntToStr(DayOf(DateTimePicker2.Date));
    URL := URL+'&Progress='+CurrentProgress.Caption;

    // Submit the request (asynchronously)
    Client := TFancyNetHTTPClient.Create(nil);
    Client.Tag := DateTimeToUnix(Now);
    Client.Description := 'DeathDay:'+FormatDateTime('mmmdd',DateTimePicker2.Date)+'/d'+IntToStr(DateTimePicker2.Tag);
    client.Asynchronous := True;
    Client.ConnectionTimeout := 1200000;
    Client.ResponseTimeout := 1200000;
    Client.onRequestCompleted := NetHTTPClient1RequestCompleted;
    Client.onRequestError := NetHTTPClient1RequestError;
    Client.URL := TidURI.URLEncode(URL);
    if Pos('https', URL) > 0
    then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    try
      Client.Get(Client.URL);
     except on E: Exception do
       begin
         LogException(Client.Description, E.ClassName, E.Message, Client.URL);
       end;
     end;
  end;
end;

procedure TMainForm.DateTimePicker3CloseUp(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  if (DateTimePicker3.Tag <> DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePicker3.Date),DayOfTheMonth(DateTimePicker3.Date)))) then
  begin
    DateTimePicker3.Tag := DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePicker3.Date),DayOfTheMonth(DateTimePicker3.Date)));
    CacheTimer.Tag := DateTimePicker3.Tag;
    LogEvent('');
    LogEvent('Regenerating Releases Data [ '+FormatDateTime('mmmdd',DateTimePicker3.Date)+' / d'+IntToStr(DateTimePicker3.Tag)+' ]');
    CurrentProgress.Caption := 'ManReGenReleases['+FormatDateTime('mmmdd',DateTimePicker3.Date)+'/d'+IntToStr(DateTimePicker3.Tag)+']: '+TGUID.NewGUID.ToString;
    CacheTimer.Enabled := False;

    progMonth.Text := FormatDateTime('mmm', DateTimePicker3.date);
    progDay.Text := FormatDateTime('dd', DateTimePicker3.date);
    progMonth.Enabled := True;
    progDay.Enabled := True;
    CacheTimer.Tag := DayOfTheYear(DateTimePicker3.Date);

    URL := AppURL+'/ActorInfoService/MovieReleaseDay';

    // Setup the Request
    URL := URL+'?Secret='+edSecret.Text;
    URL := URL+'&aMonth='+IntToStr(MonthOf(DateTimePicker3.Date));
    URL := URL+'&aDay='+IntToStr(DayOf(DateTimePicker3.Date));
    URL := URL+'&Progress='+CurrentProgress.Caption;

    // Submit the request (asynchronously)
    Client := TFancyNetHTTPClient.Create(nil);
    Client.Tag := DateTimeToUnix(Now);
    Client.Description := 'Releases:'+FormatDateTime('mmmdd',DateTimePicker3.Date)+'/d'+IntToStr(DateTimePicker3.Tag);
    client.Asynchronous := True;
    Client.ConnectionTimeout := 1200000;
    Client.ResponseTimeout := 1200000;
    Client.onRequestCompleted := NetHTTPClient1RequestCompleted;
    Client.onRequestError := NetHTTPClient1RequestError;
    Client.URL := TidURI.URLEncode(URL);
    if Pos('https', URL) > 0
    then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];

    try
      Client.Get(Client.URL);
     except on E: Exception do
       begin
         LogException(Client.Description, E.ClassName, E.Message, Client.URL);
       end;
     end;
  end;
end;

procedure TMainForm.edtClientVersionChange(Sender: TObject);
var
  ClientVer: TStringList;
begin
  ClientVer := TStringList.Create;
  ClientVer.Text := edtClientVersion.Text;
  ClientVer.SaveToFile('clientversion.txt');
  ClientVer.Free;
end;

procedure TMainForm.ManualRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
var
  MainHandle : THandle;
begin
  LogEvent('Manual Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  CurrentProgress.Caption := 'Waiting';
  CacheTimer.Enabled := True;
  Sender.Free;

  try
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessID) ;
    SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF) ;
    CloseHandle(MainHandle) ;
  except on E: Exception do
    begin
    end;
  end;
  Application.ProcessMessages;

end;

procedure TMainForm.ManualRequestError(const Sender: TObject; const AError: string);
begin
  LogEvent('| ');
  LogEvent('| Manual Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('nn:ss',-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  LogEvent('| ERROR: '+AError);
  LogEvent('| COMMS: '+(Sender as TFancyNetHTTPClient).uRL);
  LogEvent('| ');

  CacheTimer.Enabled := True;
  CurrentProgress.Caption := 'Waiting';
  Sender.Free;
end;

procedure TMainForm.NetHTTPClient1RequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
var
  MainHandle : THandle;
begin
  if Pos('BirthDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('');
    LogEvent('BirthDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
    CurrentProgress.Caption := 'Short API Delay (Continue in 10s)';
    CacheTimer.Interval := 90000; // 90 seconds
    CacheTimer.Enabled := True;

    WaitingMessage := 'Short API Delay (Continue in %s)';
    tmrWaiting.Tag := 90;
    tmrWaiting.Enabled := True;
  end
  else if Pos('DeathDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('DeathDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
    CurrentProgress.Caption := 'Short API Delay (Continue in 90s)';
    CacheTimer.Interval := 90000; // 90 seconds
    CacheTimer.Enabled := True;

    WaitingMessage := 'Short API Delay (Continue in %s)';
    tmrWaiting.Tag := 90;
    tmrWaiting.Enabled := True;
  end
  else if Pos('Releases',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('Releases Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
    CurrentProgress.Caption := 'Long API Delay (Continue in 300s)';
    CacheTimer.Interval := 300000;  // 5 minutes
    CacheTimer.Enabled := True;

    WaitingMessage := 'Long API Delay (Continue in %s)';
    tmrWaiting.Tag := 300;
    tmrWaiting.Enabled := True;
  end;

  // Delete the .working file as this request was successfully completed
  DeleteFile((Sender as TFancyNetHTTPClient).CacheFile+'.working');

  Sender.Free;

  progDay.Enabled := False;
  progMonth.Enabled := False;

  try
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessID) ;
    SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF) ;
    CloseHandle(MainHandle) ;
  except on E: Exception do
    begin
    end;
  end;
  Application.ProcessMessages;

end;

procedure TMainForm.NetHTTPClient1RequestError(const Sender: TObject; const AError: string);
begin
  if Pos('BirthDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('');
    LogEvent('BirthDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  end
  else if Pos('DeathDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('DeathDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  end
  else if Pos('Releases',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('Releases Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  end;
  LogEvent('| ');
  LogEvent('| ERROR: '+AError);
  LogEvent('| COMMS: '+(Sender as TFancyNetHTTPClient).uRL);
  LogEvent('| ');

  Sender.Free;

  progDay.Enabled := False;
  progMonth.Enabled := False;

  // Got an error, so let's skip this date and continue on
  CacheTimer.Tag := CacheTimer.Tag + 1;

  CurrentProgress.Caption := 'Short API Delay (Continue in 10s)';
  CacheTimer.Interval := 90000; // 90 seconds
  CacheTimer.Enabled := True;

  WaitingMessage := 'Short API Delay (Continue in %s)';
  tmrWaiting.Tag := 90;
  tmrWaiting.Enabled := True;
end;

procedure TMainForm.SendActivityLog(Subject: String);
var
  SMTP1: TIdSMTP;
  Msg1: TIdMessage;
  Addr1: TIdEmailAddressItem;
  Html1: TIdMessageBuilderHtml;
  SMTPResult: WideString;
begin
  if not(MailServerAvailable) then
  begin
    LogEvent('WARNING: '+Subject+' e-mail not sent (Mail services not configured)');
  end
  else
  begin

    // Send warning email
    Msg1  := nil;
    Addr1 := nil;
    SMTP1 := TIdSMTP.Create(nil);
    SMTP1.Host     := MainForm.MailServerHost;
    SMTP1.Port     := MainForm.MailServerPort;
    SMTP1.Username := MainForm.MailServerUser;
    SMTP1.Password := MainForm.MailServerPass;

    try
      Html1 := TIdMessageBuilderHtml.Create;
      try
        Html1.Html.Add('<html>');
        Html1.Html.Add('<head>');
        Html1.Html.Add('</head>');
        Html1.Html.Add('<body><pre>');
        Html1.Html.Add(mmStats.Lines.Text);
        Html1.Html.Add(mmInfo.Lines.Text);
        Html1.Html.Add('</pre></body>');
        Html1.Html.Add('</html>');
        Html1.HtmlCharSet := 'utf-8';

        Msg1 := Html1.NewMessage(nil);

        // Startup should be < 10s but otherwise send the running time
        if MillisecondsBetween(Now, AppStartup) < 30000
        then Msg1.Subject := '['+GetEnvironmentVariable('COMPUTERNAME')+'] '+Subject+': '+MainForm.Caption+' ('+IntToStr(MillisecondsBetween(Now, AppStartup))+'ms)'
        else Msg1.Subject := '['+GetEnvironmentVariable('COMPUTERNAME')+'] '+Subject+': '+MainForm.Caption+' ('+FormatDateTime('hh:nn:ss', Now - AppStartup)+'}';

        Msg1.From.Text := MainForm.MailServerFrom;
        Msg1.From.Name := MainForm.MailServerName;

        Addr1 := Msg1.Recipients.Add;
        Addr1.Address := MainForm.MailserverFrom;

        SMTP1.Connect;
        try
          try
            SMTP1.Send(Msg1);
          except on E: Exception do
            begin
              SMTPResult := SMTPResult+'[ '+E.ClassName+' ] '+E.Message+Chr(10);
            end;
          end;
        finally
          SMTP1.Disconnect();
        end;
      finally
        Addr1.Free;
        Msg1.Free;
        Html1.Free;
      end;
    except on E: Exception do
      begin
        SMTPResult := SMTPResult+'[ '+E.ClassName+' ] '+E.Message+Chr(10);
      end;
    end;
    SMTP1.Free;

    if SMTPResult = ''
    then LogEvent('NOTICE: '+Subject+' e-mail sent to '+MailServerName+' <'+MailServerFrom+'>')
    else
    begin
      LogEvent('WARNING: '+Subject+' e-mail to '+MailServerName+' <'+MailServerFrom+'> FAILED.');
      LogEvent('WARNING: SMTP Error: '+SMTPResult);
    end;
  end;
end;

procedure TMainForm.SetProgressStep(Progress: String);
var
  ProgCur: Integer;
  ProgTot: Integer;
begin
  ProgressStep.Caption := Progress;
  if Pos(' of ', Progress) > 0 then
  begin
    ProgCur := StrToIntDef(Trim(Copy(Progress,1, Pos(' of ', Progress) -1)),0);
    ProgTot := StrToIntDef(Trim(Copy(Progress, Pos(' of ', Progress)+4,Length(Progress))),10001);
    if (ProgCur = 0) or (ProgTot = 10001) or (ProgTot = 0) then
    begin
      ShapeProgressFG.Visible := False;
    end
    else
    begin
      if ProgCur > ProgTot then ProgCur := ProgTot;
      shapeProgressFG.Width := Trunc((ProgCur / ProgTot) * 799.0);
      shapeProgressFG.Visible := True;
    end;
  end
  else
  begin
    shapeProgressFG.Visible := False;
  end;
end;

procedure TMainForm.StartTimerTimer(Sender: TObject);
var
  AppConfigFile: String;
  ConfigFile: TStringList;
begin
  StartTimer.Enabled := False;

  ProgressDetail.Caption := 'Startup';
  SetProgressStep('0 of 16');

  LogEvent('');
  LogEvent('______________________________________________________');
  LogEvent('');
  LogEvent('SERVER STARTUP.');

  // Load JSON Configuration
  LogEvent('');
  LogEvent('Loading Configuration.');
  AppConfigFile := 'Actorious.json';
  ConfigFile := TStringList.Create;
  if FileExists(AppConfigFile) then
  begin
    try
      ConfigFile.LoadFromFile(AppConfigFile);
      LogEvent('- Loaded Configuration from '+AppConfigFile);
      AppConfiguration := TJSONObject.ParseJSONValue(ConfigFile.Text) as TJSONObject;
    except on E: Exception do
      begin
        LogException('Load Configuration', E.ClassName, E.Message, AppConfigFile);
      end;
    end;
  end
  else // File doesn't exist
  begin
    LogEvent('File Not Found: '+AppConfigFile);
  end;
  ConfigFile.Free;
  Application.ProcessMessages;
  SetProgressStep('1 of 16');

  if Appconfiguration = nil then
  begin
    // Create an empty AppConfiguration
    LogEvent('- Invalid Configuration');
    AppConfiguration := TJSONObject.Create;
  end;

  // Used to access this Actorious REST API
  if AppConfiguration.getValue('Actorious API Secret') <> nil then
  begin
    edSecret.Text := (AppConfiguration.getValue('Actorious API Secret') as TJSONString).Value;
    LogEvent('- Actorious API Secret Loaded');
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [Actorious API Secret]');
  end;
  SetProgressStep('2 of 16');

  // Used to access The Movie Database API
  if AppConfiguration.getValue('TMDb API Key') <> nil then
  begin
    edTMDbAPI.Text := (AppConfiguration.getValue('TMDb API Key') as TJSONString).Value;
    LogEvent('- TMDb API Key Loaded');
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [TMDb API Key]');
  end;
  SetProgressStep('3 of 16');

  // BaseURL
  if AppConfiguration.getValue('BaseURL') <> nil then
  begin
    AppBaseURL := (AppConfiguration.getValue('BaseURL') as TJSONString).Value;
    LogEvent('- BaseURL set to '+AppBaseURL);
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [BaseURL]');
  end;
  SetProgressStep('4 of 16');

  // AppURL
  if AppConfiguration.getValue('AppURL') <> nil then
  begin
    AppURL := (AppConfiguration.getValue('AppURL') as TJSONString).Value;
    LogEvent('- AppURL set to '+AppURL);
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [AppURL]');
  end;
  SetProgressStep('5 of 16');

  // Swagger Support
  if AppConfiguration.getValue('Swagger') <> nil then
  begin
    AppSwagger := (AppConfiguration.getValue('Swagger') as TJSONString).Value;
    LogEvent('- Swagger configured at '+AppSwagger);
    btSwagger.Enabled := True;
  end
  else
  begin
    btSwagger.Enabled := False;
    LogEvent('- Swagger not configured');
  end;
  SetProgressStep('6 of 16');

  // Redoc Support
  if AppConfiguration.getValue('Redoc') <> nil then
  begin
    AppRedoc := (AppConfiguration.getValue('Redoc') as TJSONString).Value;
    LogEvent('- Redoc configured at '+AppRedoc);
    btRedoc.Enabled := True;
  end
  else
  begin
    btRedoc.Enabled := False;
    LogEvent('- Redoc not configured');
  end;
  SetProgressStep('7 of 16');

  // HomeAssistant Support
  if AppConfiguration.getValue('HA_URL') <> nil then
  begin
    AppHAURL := (AppConfiguration.getValue('HA_URL') as TJSONString).Value;
    LogEvent('- Home Assistant configured at '+AppHAURL);
  end
  else
  begin
    AppHAURL := '';
    LogEvent(' - HomeAssistant not configured');
  end;
  SetProgressStep('8 of 16');

  // HomeAssistant Token
  if AppConfiguration.getValue('HA_Token') <> nil then
  begin
    AppHAToken := (AppConfiguration.getValue('HA_Token') as TJSONString).Value;
    LogEvent('- Home Assistant Token loaded');
  end
  else
  begin
    AppHAToken := '';
  end;
  SetProgressStep('9 of 16');


  // Get Mail Configuration
  MailServerAvailable := False;
  if AppConfiguration.GetValue('Mail Services') <> nil then
  begin
    btEMail.Enabled := True;
    MailServerAvailable := True;
    MailServerHost := ((AppConfiguration.GetValue('Mail Services') as TJSONObject).GetValue('SMTP Host') as TJSONString).Value;
    MailServerPort := ((AppConfiguration.GetValue('Mail Services') as TJSONObject).GetValue('SMTP Port') as TJSONNumber).AsInt;
    MailServerUser := ((AppConfiguration.GetValue('Mail Services') as TJSONObject).GetValue('SMTP User') as TJSONString).Value;
    MailServerPass := ((AppConfiguration.GetValue('Mail Services') as TJSONObject).GetValue('SMTP Pass') as TJSONString).Value;
    MailServerFrom := ((AppConfiguration.GetValue('Mail Services') as TJSONObject).GetValue('SMTP From') as TJSONString).Value;
    MailServerName := ((AppConfiguration.GetValue('Mail Services') as TJSONObject).GetValue('SMTP Name') as TJSONString).Value;
    LogEvent('- SMTP Mail Server: '+MailServerHost+' / '+IntToStr(MailServerPort));
  end
  else
  begin
    LogEvent('- SMTP Mail Server: Unavailable');
  end;

  LogEvent('Configuration Loaded.');
  LogEvent('');

  Application.ProcessMessages;

  // Kick off Cache Populator
  CurrentProgress.Caption := 'Startup Delay (Continue in 15s)';
  CacheTimer.Interval := 15000;
  CacheTimer.Enabled := True;
  SetProgressStep('10 of 16');

  WaitingMessage := 'Startup Delay (Continue in %s)';
  tmrWaiting.Tag := 15;
  tmrWaiting.Enabled := True;
  SetProgressStep('11 of 16');

  // Change URL of server depending on machine it is running on
  if AppBaseURL <> '' then
  begin
    ServerContainer.XDataServer.BaseURL := AppBaseURL;
    ServerContainer.SparkleHttpSysDispatcher.Active := True;
  end;
  SetProgressStep('12 of 16');

  // Create Cache directory structure
  LogEvent('Creating Cache Directories');

  CreateDir('cache'); // Cache Root

  CreateDir('cache\people');                  // Data cached by TMDb ID, either Actors or Directors or Writers
  CreateDir('cache\people\tmdb');             // JSON as it originated from TMDb
  CreateDir('cache\people\actorious');        // JSON formatted for Actorious
  CreateDir('cache\people\top1000');          // Top 1000 all ready to go
  CreateDir('cache\people\top5000');          // Top 5000 all ready to go

  CreateDir('cache\days');                    // Data cached by Julian Day
  CreateDir('cache\days\actorious-births');   // People with this birthday
  CreateDir('cache\days\actorious-deaths');   // People with this birthday
  CreateDir('cache\days\actorious-releases'); // People with this birthday
  CreateDir('cache\days\first');              // The first person to appear for this given birthday
  CreateDir('cache\days\wikidata-births');    // Wikidata response to this birthday
  CreateDir('cache\days\wikidata-deaths');    // Wikidata response to this deathday
  CreateDir('cache\days\wikidata-releases');  // Wikidata response to this releaseday
  CreateDir('cache\days\toptoday');

  CreateDir('cache\movies');
  CreateDir('cache\movies\tmdb');
  CreateDir('cache\movies\actorious');
  CreateDir('cache\movies\top1000');
  CreateDir('cache\movies\top5000');

  CreateDir('cache\tvshows');
  CreateDir('cache\tvshows\tmdb');
  CreateDir('cache\tvshows\actorious');
  CreateDir('cache\tvshows\top1000');
  CreateDir('cache\tvshows\top5000');
  SetProgressStep('13 of 16');

  // Show encoded Base64 version of secret
  LogEvent('Encoding Actorious API Secret');
  edSecretChange(nil);
  SetProgressStep('14 of 16');

  // Check for new ActoriousClient Version right away
  LogEvent('Updating Client Version');
  btUpdateVersionClick(nil);
  SetProgressStep('15 of 16');

  // Display the progress at start
  LogEvent('Updating Progress');
  btRecentProgressClick(Sender);
  SetProgressStep('16 of 16');

  LogEvent('');
  LogEvent('SERVER STARTUP COMPLETE ('+IntToStr(MillisecondsBetween(Now, AppStartup))+'ms).');
  LogEvent('______________________________________________________');
  LogEvent('');

  // Send an email if so configured
  SendActivityLog('Startup Confirmation');

  ProgressDetail.Caption := 'Startup Complete';

  // Default XData UpdateGUI call
  UpdateGUI;

end;

procedure TMainForm.tmrProgressTimer(Sender: TObject);
var
  i: Integer;
  PReport: String;
begin
  btRecentProgressClick(Sender);

  // We're only going to do this if something is going on
  if Length(CurrentProgress.Caption) > 30 then
  begin

    // If the server has been running for awhile. Progress.Count will be large
    // So only check the most recent entries.  How far back?  Well, that will
    // depend somewhat on how much activity is going on.

    i := Progress.Count-1;
    while i >= Max(0, Progress.Count-25) do
    begin

      // Just want the non-complete record that is currently running
      if (Pos(CurrentProgress.Caption, Progress[i]) > 0) and
         (Pos('"PR":"Complete', Progress[i]) = 0)  then
      begin

        // Just want the PR element
        PReport := Copy(Progress[i], Pos('"PR":', Progress[i])+6, 999);
        PReport := Copy(PReport,1,Pos('"TP":', PReport)-3);

        // Show extra data if it is available
        if (Pos('(', PReport) > 0) then
        begin
          ProgressDetail.Caption := Copy(PReport,1,Pos('(',PReport)-2);
          SetProgressStep(Trim(StringReplace(StringReplace(Copy(PReport,Pos('(',PReport),99),'(','',[rfReplaceAll]),')','',[rfReplaceAll])));
        end

        // Otherwise, just the PR
        else
        begin
          ProgressDetail.Caption := PReport;
          SetProgressStep('');
        end;

        // And we don't need to do much else
        exit;
      end;
      i := i - 1;
    end;
  end;
  ProgressDetail.Caption := '';
  SetProgressStep('');
end;

procedure TMainForm.tmrTopUpdateTimer(Sender: TObject);
var
  WindowStart :TDateTime;
  WindowEnd: TDateTime;
begin
  // This timer kicks off the Top1000 and Top5000 daily updates.
  // These updates are scheduled so as to happen at about the same time each day.
  // Note also that the Top5000 update is what generates the ranking history.


  // 6AM Run

  WindowStart := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 05, 00, 0, 0);
  WindowEnd   := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 05, 05, 0, 0);
  if (WindowStart < Now) and (WindowEnd > Now) and (btClean.Tag <> Today) then
  begin
    btClean.Tag := Trunc(Today);
    btCleanClick(Sender);
    exit;
  end;

  WindowStart := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 05, 15, 0, 0);
  WindowENd   := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 05, 20, 0, 0);
  if (WindowStart < Now) and (WindowEnd > Now) and (btTop1000.Tag <> Today) then
  begin
    btTop1000.Tag := Trunc(Today);
    btTop5000.Tag := 0;
    btTop1000Click(Sender);
    exit;
  end;

//  WindowStart := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 05, 30, 0, 0);
//  WindowEnd   := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 05, 35, 0, 0);
//  if (WindowStart < Now) and (WindowEnd > Now) and (btTop5000.Tag <> Today) then
//  begin
//    btTop5000.Tag := Trunc(Today);
//    btTop1000.Tag := 0;
//    btClean.Tag   := 0;
//    btTop5000Click(Sender);
//    exit;
//  end;


  // 6PM Run

  WindowStart := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 00, 0, 0);
  WindowEnd   := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 05, 0, 0);
  if (WindowStart < Now) and (WindowEnd > Now) and (btClean.Tag <> Today) then
  begin
    btClean.Tag := Trunc(Today);
    btCleanClick(Sender);
    exit;
  end;
//
//  WindowStart := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 15, 0, 0);
//  WindowEnd   := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 20, 0, 0);
//  if (WindowStart < Now) and (WindowEnd > Now) and (btTop1000.Tag <> Today) then
//  begin
//    btTop1000.Tag := Trunc(Today);
//    btTop5000.Tag := 0;
//    btTop1000Click(Sender);
//    exit;
//  end;

  WindowStart := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 15, 0, 0);
  WindowEnd   := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 20, 0, 0);
  if (WindowStart < Now) and (WindowEnd > Now) and (btTop5000.Tag <> Today) then
  begin
    btTop5000.Tag := Trunc(Today);
    btTop1000.Tag := 0;
    btClean.Tag   := 0;
    btTop5000Click(Sender);
    exit;
  end;

end;

procedure TMainForm.tmrVersionCheckTimer(Sender: TObject);
begin
  btUpdateVersionClick(Sender)
end;

procedure TMainForm.tmrWaitingTimer(Sender: TObject);
begin
  if (Pos('Waiting', CurrentProgress.Caption) > 0) or
     (Pos('Delay',   CurrentProgress.Caption) > 0) or
     (Pos('Retry',   CurrentProgress.Caption) > 0) then
  begin
    tmrWaiting.Tag := tmrWaiting.Tag - 1;
    CurrentProgress.Caption := StringReplace(WaitingMessage, '%s', IntToStr(tmrWaiting.Tag)+'s',[]);
    if tmrWaiting.Tag = 0 then tmrWaiting.Enabled := False;
  end
  else
  begin
    tmrWaiting.Enabled := False;
  end;
end;

procedure TMainForm.edSecretChange(Sender: TObject);
begin
  edSecretBase64.Text := TNetEncoding.Base64.encode(edSecret.Text);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Progress.Free;
end;

procedure TMainForm.FormCreate(ASender: TObject);
begin

  // How long has server been running?
  AppStartup := Now;
  MemoryUsage := '0.0';
  MemoryUsageNice := '0.0';

  // Help with flooding exception emails
  LastException := Now - 1;

  // Sort out the Server Version
  GetAppVersionString;

  // Having a dark form in the IDE makes it hard to read component names
  MainForm.Color := clBlack;

  // Initialize Progress History
  Progress := TStringList.Create;
  CurrentProgress.Caption := '';
  ProgressDetail.Caption := '';
  SetProgressStep('');

  // Avoid divide by zero errors
  PersonCacheRequests := 0;
  MovieCacheRequests  := 0;
  TVShowCacheRequests := 0;
  CleanRequests       := 0;

  // Set starting time for cache to today
  progMonth.Text := FormatDateTime('mmm',Now);
  progDay.Text   := FormatDateTime('dd',Now);
  CacheTimer.Tag := DayOfTheYear(EncodeDate(2020,MonthOf(Now),DayOf(Now)));

  // Do it this way so we don't wait for screen to appear
  StartTimer.Enabled := True;

end;


procedure TMainForm.FormResize(Sender: TObject);
begin
  Panel1.Left := Max(((MainForm.Width - Panel1.Width) div 2),0);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if MainForm.Tag = 0 then
  begin
    MainForm.Tag := 1;
    MainForm.WindowState := wsMaximized;
    MainForm.WindowState := wsMinimized;
  end;
end;

procedure TMainForm.UpdateGUI;
const
  cHttp = 'http://+';
  cHttpLocalhost = 'http://localhost';
begin
  btStart.Enabled := not ServerContainer.SparkleHttpSysDispatcher.Active;
  btStop.Enabled := not btStart.Enabled;

  if ServerContainer.SparkleHttpSysDispatcher.Active then
  begin
    LogEvent(SServerStartedAt + StringReplace(ServerContainer.XDataServer.BaseUrl, cHttp, cHttpLocalhost, [rfIgnoreCase]));
    tmrVersionCheck.Enabled := True;
    tmrTopUpdate.Enabled := True;
    btTimer.Enabled := True;
    btTimer.Tag := 1;
    btTimer.Caption := 'Disable Timer';
    btSwagger.Enabled := True;
    btRedoc.Enabled := True;
  end
  else
  begin
    LogEvent(SServerStopped);
    tmrVersionCheck.Enabled := False;
    tmrTopUpdate.Enabled := False;
    btTimer.Enabled := False;
    btTimer.Tag := 0;
    btTimer.Caption := 'Enable Timer';
    btSwagger.Enabled := False;
    btRedoc.Enabled := False;
  end;
  LogEvent('');
end;

end.
