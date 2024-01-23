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
    DateTimePickerBirthday: TDateTimePicker;
    DateTimePickerDeathDay: TDateTimePicker;
    DateTimePickerReleaseDay: TDateTimePicker;
    btTop1000: TButton;
    btTop5000: TButton;
    btAll: TButton;
    edtClientVersion: TEdit;
    btUpdateVersion: TButton;
    tmrVersionCheck: TTimer;
    tmrTopUpdate: TTimer;
    sparqlRelatives: TMemo;
    tmrWaiting: TTimer;
    btInternal: TButton;
    tmrProgress: TTimer;
    mmStats: TMemo;
    btClean: TButton;
    ckRegenerate: TCheckBox;
    btRedoc: TButton;
    mmInfo: TMemo;
    btEmail: TButton;
    lblBirthDays: TLabel;
    lblDeathDays: TLabel;
    lblReleaseDays: TLabel;
    lblSearchPeople: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Shape1: TShape;
    PanelA: TPanel;
    ProgressDetailA: TLabel;
    CurrentProgressA: TLabel;
    shapeProgressFGA: TShape;
    shapeProgressBG: TShape;
    ProgressStepA: TLabel;
    PanelB: TPanel;
    Shape2: TShape;
    shapeProgressFGB: TShape;
    ProgressStepB: TLabel;
    ProgressDetailB: TLabel;
    CurrentProgressB: TLabel;
    PanelC: TPanel;
    Shape4: TShape;
    shapeProgressFGC: TShape;
    ProgressStepC: TLabel;
    ProgressDetailC: TLabel;
    CurrentProgressC: TLabel;
    PanelD: TPanel;
    Shape6: TShape;
    shapeProgressFGD: TShape;
    ProgressStepD: TLabel;
    ProgressDetailD: TLabel;
    CurrentProgressD: TLabel;
    PanelE: TPanel;
    Shape8: TShape;
    shapeProgressFGE: TShape;
    ProgressStepE: TLabel;
    ProgressDetailE: TLabel;
    CurrentProgressE: TLabel;
    PanelF: TPanel;
    Shape10: TShape;
    shapeProgressFGF: TShape;
    ProgressStepF: TLabel;
    ProgressDetailF: TLabel;
    CurrentProgressF: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
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
    procedure DateTimePickerBirthdayCloseUp(Sender: TObject);
    procedure DateTimePickerDeathDayCloseUp(Sender: TObject);
    procedure btTop1000Click(Sender: TObject);
    procedure GetTopResults(TopURL: String; TopURLName: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function GetProcessThreadCount(ProcessId: Cardinal): Integer;
    procedure btTop5000Click(Sender: TObject);
    procedure btAllClick(Sender: TObject);
    procedure edtClientVersionChange(Sender: TObject);
    procedure btUpdateVersionClick(Sender: TObject);
    procedure UpdateHomeAssistant;
    procedure tmrVersionCheckTimer(Sender: TObject);
    procedure tmrTopUpdateTimer(Sender: TObject);
    procedure DateTimePickerReleaseDayCloseUp(Sender: TObject);
    procedure tmrWaitingTimer(Sender: TObject);
    procedure btInternalClick(Sender: TObject);
    procedure tmrProgressTimer(Sender: TObject);
    procedure btCleanClick(Sender: TObject);
    procedure btRedocClick(Sender: TObject);
    procedure LogEvent(Details: String);
    procedure LogException(Source: String; EClass: String; EMessage: String; Data: String);
    procedure SendActivityLog(Subject: String);
    procedure btEmailClick(Sender: TObject);
    procedure GetAppParameters(List: TStringList);
    function SearchPeople(ActorID: Integer):Integer;
    procedure UpdateSearch(SearchIndex: Integer; SearchData: String);
    function LocalSearch(SearchTerm: String; Adult: Boolean):String;
    procedure lblSearchPeopleClick(Sender: TObject);
    function Occurrences(const Substring, Text: string): integer;
    procedure SaveSearchIndexes;
    procedure LoadSearchIndexes;
    procedure UpdateProgress(PNum: Integer; PInfo: String; PDetail:String; PStep:String; PReport: String);
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
    BirthDaysCount: String;

    AppStartup: TDateTime;
    AppParameters: TStringList;
    AppConfiguration: TJSONObject;
    AppBaseURL: String;
    AppURL: String;
    AppSwagger: String;
    AppRedoc: String;
    AppHAURL: String;
    AppHAToken: String;
    AppCacheDir: String;
    AppCacheSkips: Integer;
    LastException: TDateTime;

    MailServerAvailable: Boolean;
    MailServerHost: String;
    MailServerPort: Integer;
    MailServerUser: String;
    MailServerPass: String;
    MailServerFrom: String;
    MailServerName: String;

    LookupCache: TDictionary<String,String>;

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

  IndexPeople: Array of String;
  IndexMovies: Array of String;
  IndexTVShow: Array of String;
  IndexPeopleSize: Integer;
  IndexMoviesSize: Integer;
  IndexTVShowSize: Integer;

implementation

{$R *.dfm}

resourcestring
  SServerStopped = 'Server stopped';
  SServerStartedAt = 'Server started at ';

{ TMainForm }



function TMainForm.Occurrences(const Substring, Text: string): integer;
var
  offset: integer;
begin
  result := 0;
  offset := PosEx(Substring, Text, 1);
  while offset <> 0 do
  begin
    inc(result);
    offset := PosEx(Substring, Text, offset + length(Substring));
  end;
end;


procedure TMainForm.GetAppParameters(List: TStringList);
var
  i: Integer;
begin
  i := 1;
  while i <= ParamCount do
  begin
    List.Add('"'+ParamStr(i)+'"');
    i := i + 1;
  end;
end;

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
  CurrentProgressA.Caption := 'ManReGenTop1000: '+TGUID.NewGUID.ToString;
  CacheTimer.Enabled := False;

  URL := AppURL+'/ActorInfoService/TopOneThousand';

  // Setup the Request
  URL := URL+'?Secret='+edSecret.Text;
  URL := URL+'&Segment=A';
  URL := URL+'&Progress='+CurrentProgressA.Caption;

  // Submit the request (asynchronously)
  Client := TFancyNetHTTPClient.Create(nil);
  Client.Tag := DateTimeToUnix(Now);
  Client.Description := 'Top 1000';
  Client.Asynchronous := True;
  Client.ConnectionTimeout := 5400000;  // 90 minutes
  Client.ResponseTimeout := 5400000;  // 90 minutes
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

procedure TMainForm.GetTopResults(TopURL: String; TopURLName: String);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  LogEvent('');
  LogEvent('Caching '+TopURLName+' ['+TopURL+']');
  CurrentProgressA.Caption := 'ManReGen'+TopURLName+': '+TGUID.NewGUID.ToString;
  CacheTimer.Enabled := False;

  URL := TopURL;

  // Submit the request (asynchronously)
  Client := TFancyNetHTTPClient.Create(nil);
  Client.Tag := DateTimeToUnix(Now);
  Client.Description := TopURLName;
  Client.Asynchronous := True;
  Client.ConnectionTimeout := 30000;  // 5 minutes
  Client.ResponseTimeout := 30000;  // 5 minutes
  Client.onRequestCompleted := ManualRequestCompleted;
  Client.onRequestError := ManualRequestError;
  Client.URL := TidURI.URLEncode(URL);
  if Pos('https', URL) > 0
  then Client.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS12];
  try
    Client.Get(Client.URL);
  except on E: Exception do
    begin
      LogException('Get '+TopURLName, E.ClassName, E.Message, Client.URL);
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
  CurrentProgressA.Caption := 'ManReGenTop5000: '+TGUID.NewGUID.ToString;
  CacheTimer.Enabled := False;

  URL := AppURL+'/ActorInfoService/TopFiveThousand';

  // Setup the Request
  URL := URL+'?Secret='+edSecret.Text;
  URL := URL+'&Segment=A';
  URL := URL+'&Progress='+CurrentProgressA.Caption;

  // Submit the request (asynchronously)
  Client := TFancyNetHTTPClient.Create(nil);
  Client.Tag := DateTimeToUnix(Now);
  Client.Description := 'Top 5000';
  client.Asynchronous := True;
  Client.ConnectionTimeout := 5400000; // 90 minutes
  Client.ResponseTimeout := 5400000; // 90 minutes
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

      Endpoint := '/api/states/sensor.actorious_server_statistics';
      Data := TStringStream.Create('{"state":"'+BirthDaysCount+' / '+lblBirthDays.Caption+' / '+lblDeathDays.Caption+' / '+lblReleaseDays.Caption+'"}');
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

procedure TMainForm.UpdateSearch(SearchIndex: Integer; SearchData: String);
var
  SearchFormat: String;
begin
  SearchFormat := Uppercase(SearchData);
  SearchFormat := StringReplace(SearchFormat,' ','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'''','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'"','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'/','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'-','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,',','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'.','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'(','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,')','',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'HERSELFGUEST:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'HIMSELFGUEST:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'HERSELF:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'HIMSELF:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'SELFGUEST:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'SELF:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'GUEST:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'VOICE:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'VOICES:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'UNCREDITED:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'ARCHIVALFOOTAGE:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'ARCHIVEFOOTAGE:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'FOOTAGE:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,':HERSELF:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,':HIMSELF:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,':SELF:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,':VOICE:',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'::',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'::',':',[rfReplaceAll]);
  SearchFormat := StringReplace(SearchFormat,'::',':',[rfReplaceAll]);

  IndexPeople[SearchIndex] := SearchFormat;
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
          d2 := FormatDateTime('HH:nn:ss.zzz',(d.getValue('TP') as TJSONNumber).AsDouble-(d.getValue('TS') as TJSONNumber).AsDouble);

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
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('  SEARCH INDEX INFORMATION');
  mmStats.Lines.Add('  ========================================');
  mmStats.Lines.Add('');
  mmStats.Lines.Add('  Person Index Size:    '+FloatToStrF(IndexPeopleSize/1024/1024,ffNumber,9,3).PadLeft(9)+' MB');

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

procedure TMainForm.lblSearchPeopleClick(Sender: TObject);
var
  i,j : Integer;
begin

  // Shows the most recent additions to the People search index
  btClearClick(Sender);

  i := Length(IndexPeople) - 1;
  j := 0;
  while i >= 0 do
  begin
    if Length(IndexPeople[i]) >= 150
    then LogEvent(RightStr('00000'+IntToStr(i),5)+' '+LeftStr(IndexPeople[i],100)+' ... '+RightStr(IndexPeople[i],50))
    else LogEvent(RightStr('00000'+IntToStr(i),5)+' '+IndexPeople[i]);
    i := i - 1;

    // Only want the most recent 50
    j := j + 1;
    if j = 50 then i := -1;
  end;

  SaveSearchIndexes;
end;

function TMainForm.LocalSearch(SearchTerm: String; Adult: Boolean): String;
var
  i, j: Integer;
  SearchEnd: Boolean;
  SearchAdult: String;
  SearchKey: String;
  SearchKey1: String;
  SearchKey2: String;
  SearchKey3: String;
  SearchKey4: String;
  Matches: TStringList;
  Matched: Boolean;
  Points: Integer;
  Relevance: Double;

  Search1: Integer;
  Search2: Integer;
  Search3: Integer;
  Search4: Integer;

  Search1OK: Boolean;
  Search2OK: Boolean;
  Search3OK: Boolean;
  Search4OK: Boolean;

  MatchName: String;

  MatchList: TStringList;
//  ElapsedTime: TDateTime;

begin
//  ElapsedTime := Now;
  Result := '[';
  Matches := TStringList.Create;

  // Cleanup our search value so we don't accidentally hit every entry
  // And so we don't also accidentally miss every entry
  SearchKey := Uppercase(Trim(SearchTerm));
  SearchKey := StringReplace(SearchKey,'.',' ',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,':',' ',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'''','',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'"','',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'(','',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,')','',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'     ',' ',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'    ',' ',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'   ',' ',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'  ',' ',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'  ',' ',[rfReplaceAll]);
  SearchKey := StringReplace(SearchKey,'  ',' ',[rfReplaceAll]);

  // Want to separate out keys so they can be searched independently
  SearchKey1 := SearchKey;
  SearchKey2 := '';
  SearchKey3 := '';
  SearchKey4 := '';
  if Pos(' ', SearchKey1) > 0 then
  begin
    SearchKey1 := Copy(SearchKey,1,Pos(' ',SearchKey)-1);
    SearchKey := Copy(SearchKey,Length(SearchKey1)+2,Length(SearchKey));
    SearchKey2 := SearchKey;
  end;
  if Pos(' ', SearchKey2) > 0 then
  begin
    SearchKey2 := Copy(SearchKey,1,Pos(' ',SearchKey)-1);
    SearchKey := Copy(SearchKey,Length(SearchKey2)+2,Length(SearchKey));
    SearchKey3 := SearchKey;
  end;
  if Pos(' ', SearchKey3) > 0 then
  begin
    SearchKey3 := Copy(SearchKey,1,Pos(' ',SearchKey)-1);
    SearchKey := Copy(SearchKey,Length(SearchKey3)+2,Length(SearchKey));
    SearchKey4 := SearchKey;
  end;
  if Pos(' ', SearchKey4) > 0 then
  begin
    SearchKey4 := StringReplace(SearchKey,' ','',[rfReplaceAll]);
  end;

//  LogEvent('Search1: ['+SearchKey1+']');
//  LogEvent('Search2: ['+SearchKey2+']');
//  LogEvent('Search3: ['+SearchKey3+']');
//  LogEvent('Search4: ['+SearchKey4+']');
  if Length(Trim(SearchKey1)) >= 3 then Search1OK := True else Search1OK := False;
  if Length(Trim(SearchKey2)) >= 3 then Search2OK := True else Search2OK := False;
  if Length(Trim(SearchKey3)) >= 3 then Search3OK := True else Search3OK := False;
  if Length(Trim(SearchKey4)) >= 3 then Search4OK := True else Search4OK := False;


  // The first character of the index is used to flag as Adult
  if Adult
  then SearchAdult := 'Y'
  else SearchAdult := 'N';

  // Quick pass to get all the candidates. Might be a lot for a short search key
  // If we don't have any decent-sized keys, then not much point in searching
  SearchEnd := False;
  if not(Search1OK or Search2OK or Search3OK or Search4OK)
  then SearchEnd := True;

  i := 0;
  while not(SearchEnd) do
  begin
    Matched := False;
    if i > Length(IndexPeople) - 1 then
    begin
      SearchEnd := True;
    end
    else
    begin
      // Filter for Adult content
      if (Copy(IndexPeople[i],1,1) = SearchAdult) then
      begin
        // Include if something matches
        if Search1OK then Search1 := Pos(SearchKey1, IndexPeople[i]) else Search1 := 0;
        if Search2OK then Search2 := Pos(SearchKey2, IndexPeople[i]) else Search2 := 0;
        if Search3OK then Search3 := Pos(SearchKey3, IndexPeople[i]) else Search3 := 0;
        if Search4OK then Search4 := Pos(SearchKey4, IndexPeople[i]) else Search4 := 0;

        if (Search1OK and (Search1 > 0)) then Matched := True;
        if (Search2OK and (Search2 > 0)) then Matched := True;
        if (Search3OK and (Search3 > 0)) then Matched := True;
        if (Search4OK and (Search4 > 0)) then Matched := True;

        // Exclude if something doesn't match
        if Matched then
        begin
          if (Search1OK and (Search1 = 0)) then Matched := False;
          if (Search2OK and (Search2 = 0)) then Matched := False;
          if (Search3OK and (Search3 = 0)) then Matched := False;
          if (Search4OK and (Search4 = 0)) then Matched := False;
        end;
      end;
    end;

    if Matched then Matches.Add(IndexPeople[i]);
    i := i + 1;
  end;

//  LogEvent('First Matches '+IntToStr(Matches.Count)+' out of '+IntToStr(Length(IndexPeople)));

  // Do a more exhaustive review to come up with a relevance score for each
  i := 0;
  while i < Matches.Count do
  begin
    // These are the points assigned by the Actorious ranking algorithm (as opposed to the TMDb ranking)
    Points := StrToIntDef(RightStr(Matches[i],6),0);

    // Get the list - 1st is id, 2nd is name, then all the roles, and last is points
    MatchList := TStringLIst.Create;
    MatchList.Delimiter := ':';
    MatchList.DelimitedText := Matches[i];

    if MatchList.Count < 3
    then MatchList.DelimitedText := 'N0:DoNotMatchName:DoNotMatchRole0';

    // We don't need to search the whole thing technically, but most of it
    // Here we're just removing the ranking at the end as we don't want it to be matched
    // We could do the same for the ref# in the beginning, but it might be fun to match that in some cases
//    MatchSearch := LeftStr(Matches[i], Length(Matches[i]) - 6);
//    MatchSearch := Copy(Matches[i], 10, Length(Matches[i]));    // Remove the Ref# at the beginning of the searched string

    // To start with, the relevance will be the points from the Actorious algorithm
    Relevance := Points / 1000;

    // If the search terms appear early in the search (starting within the name),
    // we want the relevance to be much higher (as in, if it is the name and not a role)
    // and a super-bonus if the name matches exactly.
    MatchName := MatchList[1];
    if Trim(MatchName) = '' then Relevance := 0;

//    LogEvent(Copy(MatchSearch,1,9)+': '+MatchName.PadRight(20)+': ' + IntToStr(Points).PadLeft(10) + ' -> ' +IntToStr(Trunc(Relevance)).PadLeft(10));

    // Search term is a perfect match to name
    if (SearchKey1 = MatchName) or
       (SearchKey2 = MatchName) or
       (SearchKey3 = MatchName) or
       (SearchKey4 = MatchName)
    then Relevance := Relevance + 10000

    // Search term combo is perfect match to name
    else if (Search1OK and Search2OK) and
            (SearchKey1 + SearchKey2 = MatchName)
    then Relevance := Relevance + 10000

    else if (Search1OK and Search2OK and Search3OK) and
            (SearchKey1 + SearchKey2 + SearchKey3 = MatchName)
    then Relevance := Relevance + 10000

    else if (Search1OK and Search3OK) and
            (SearchKey1 + SearchKey3 = MatchName)
    then Relevance := Relevance + 10000

    else if (Search2OK and Search3OK) and
            (SearchKey2 + SearchKey3 = MatchName)
    then Relevance := Relevance + 10000;

    for j := 2 to MatchList.Count - 2 do // Skip the id, name and points
    begin

      // Search term is a perfect match to role
      if (SearchKey1 = MatchList[j]) or
         (SearchKey2 = MatchList[j]) or
         (SearchKey3 = MatchList[j]) or
         (SearchKey4 = MatchList[j])
      then Relevance := Relevance + 1000

      // Search term combo is a perfect match to role
      else if (SearchKey1 + SearchKey2 = MatchList[j]) or
              (SearchKey1 + SearchKey2 + SearchKey3 = MatchList[j]) or
              (SearchKey1 + SearchKey3 = MatchList[j]) or
              (SearchKey2 + SearchKey3 = MatchList[j])
      then Relevance := Relevance + 1000;

//    LogEvent(Copy(MatchSearch,1,9)+': '+MatchName.PadRight(20)+': ' + IntToStr(Points).PadLeft(10) + ' -> ' +IntToStr(Trunc(Relevance)).PadLeft(10));

      // Does the search appear in a role?
      Search1 := Pos(SearchKey1, MatchList[j]);
      Search2 := Pos(SearchKey2, MatchList[j]);
      Search3 := Pos(SearchKey3, MatchList[j]);
      Search4 := Pos(SearchKey4, MatchList[j]);

      // Otherwise, we want the relevance to increase if the name appears a lot.  For example, if the search is for Spok,
      // an index item where this appears many times should be ranked higher than if it appears just once. But we also
      // factor in how long the search term is, and give a heigher weighting for longer terms.
      // Successive terms are weighted slightly less as well
      if (Search1OK and (Search1 = 0)) or
         (Search2OK and (Search2 = 0)) or
         (Search3OK and (Search3 = 0)) or
         (Search4OK and (Search4 = 0))
      then
      begin
        // We've got an initial match based on a crude search, but here we see it isn't
        // really a match, so nothing needs to be done. If no matches are found, then
        // the relevance will not be improved, and thus will be eliminated
      end
      else
      begin
        if Search1OK and (Search1 > 0) then Relevance := Relevance + Points/1000;
        if Search2OK and (Search2 > 0) then Relevance := Relevance + Points/2000;
        if Search3OK and (Search3 > 0) then Relevance := Relevance + Points/3000;
        if Search4OK and (Search4 > 0) then Relevance := Relevance + Points/4000;
      end;

    end;
    MatchList.Free;

//    LogEvent(Copy(MatchSearch,1,9)+': '+MatchName.PadRight(20)+': ' + IntToStr(Points).PadLeft(10) + ' -> ' +IntToStr(Trunc(Relevance)).PadLeft(10));
//    LogEvent(' ');

    if Relevance = Points / 1000
    then Relevance := 0;

    // Relevance is added to beginning of term
    Matches[i] := RightStr('0000000000'+IntToStr(Trunc(Relevance)),10)+Copy(Matches[i],1,10);

    i := i + 1;
  end;

  // Sort based on relevance. Might still be a large number here.
  Matches.Sort;

  // Return the ___bottom___  50
  i := Matches.Count - 1;
  j := 0;
  while i >= 0 do
  begin
    if ((Matches[i] <> '') and (Copy(Matches[i],1,10) <> '0000000000')) then
    begin
      if Result = '['
      then Result := Result + '{"Person":"'+Copy(Matches[i],12,8)+'","Relevance":"'+Copy(Matches[i],1,10)+'"}'
      else Result := Result + ',{"Person":"'+Copy(Matches[i],12,8)+'","Relevance":"'+Copy(Matches[i],1,10)+'"}';
      i := i - 1;

      j := j + 1;
      if j = 50 then i := -1;
    end
    else
    begin
      i := i - 1;
    end;
  end;

  Result := Result + ']';

//  LogEvent('Second Matches '+IntToStr(Matches.Count)+' out of '+IntToStr(Length(IndexPeople)));
//  LogEvent(Result);

  Matches.Free;

//  LogEvent('Search Time: '+IntToStr(MillisecondsBetween(Now, ElapsedTime))+'ms');
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
    CheckFile: TStringList;
  begin
    Result := 0;
    if DirectoryExists(CacheDir) then
    begin
      for FileName in TDirectory.GetFiles(CacheDir, '*') do
      begin
        CleanRequests := CleanRequests + 1;
        FileSize := FileSizeByName(FileName);
        
        if (TFile.GetLastWriteTime(FileName) < OlderThan) or (Pos('json.working',Filename) > 0) or (FileSize < 130) then
        begin
          if FileSize < 130 then
          begin
            CheckFile := TStringList.Create;
            CheckFile.LoadFromFile(FileName);
            if (Trim(CheckFile.Text) = '[]') or
               (Trim(CheckFile.Text) = '{}') or
               (Pos(Uppercase(Copy(Trim(CheckFile.Text),1,20)),'HTML') > 0) or
               (Pos(Uppercase(Copy(Trim(CheckFile.Text),1,20)),'ERROR') > 0) or
               (Pos('.working', FileName) > 0)  then
             begin
               // Curious when this condition is met, unless it is just an old file
               if (TFile.GetLastWriteTime(FileName) >= OlderThan) and (Pos('.br', Filename) = 0)
               then LogEvent('    - Small File Detected: '+StringReplace(Filename,'\','/',[rfReplaceAll])+ ' [ '+IntToStr(Filesize)+' bytes ]');

               Result := Result + 1;
               CleanFiles := CleanFiles + 1;
               CleanSmall := CleanSmall + 1;
               CleanSize := CleanSize + FileSize;
               TFile.Delete(FileName);
             end;
             CheckFile.Free;
          end
          else
          begin
            Result := Result + 1;
            CleanFiles := CleanFiles + 1;
            CleanSize := CleanSize + FileSize;
            TFile.Delete(FileName);
          end;
        end;

        Application.ProcessMessages;

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

  // Cleaning anything older than 15 days
  OlderThan := Now() - 15;

  // Days
  LogEvent('- Cleaning 01/11 Days');
  CacheDir := AppCacheDir+'cache/days/actorious-births';
  CleanDays := CleanDays + CleanDir;
  CacheDir := AppCacheDir+'cache/days/actorious-deaths';
  CleanDays := CleanDays + CleanDir;
  CacheDir := AppCacheDir+'cache/days/actorious-releases';
  CleanDays := CleanDays + CleanDir;
  CacheDir := AppCacheDir+'cache/days/first';
  CleanDays := CleanDays + CleanDir;
  CacheDir := AppCacheDir+'cache/days/wikidata-births';
  CleanDays := CleanDays + CleanDir;
  CacheDir := AppCacheDir+'cache/days/wikidata-deaths';
  CleanDays := CleanDays + CleanDir;
  CacheDir := AppCacheDir+'cache/days/wikidata-releases';
  CleanDays := CleanDays + CleanDir;
  btRecentProgressClick(nil);

  // People
  LogEvent('- Cleaning 02/11 People/Top');
  CacheDir := AppCacheDir+'cache/people/top1000';
  CleanPeople := CleanPeople + CleanDir;
  CacheDir := AppCacheDir+'cache/people/top5000';
  CleanPeople := CleanPeople + CleanDir;
  btRecentProgressClick(nil);

  LogEvent('- Cleaning 03/11 People/Actorious');
  for i :=  0 to 999 do
  begin
    CacheDir := AppCacheDir+'cache/people/actorious/'+RightStr('000'+IntToStr(i),3);
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

  LogEvent('- Cleaning 04/11 People/TMDb');
  for i :=  0 to 999 do
  begin
    CacheDir := AppCacheDir+'cache/people/tmdb/'+RightStr('000'+IntToStr(i),3);
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
  LogEvent('- Cleaning 05/11 Movies/Top');
  CacheDir := AppCacheDir+'cache/movies/top1000';
  CleanPeople := CleanPeople + CleanDir;
  CacheDir := AppCacheDir+'cache/movies/top5000';
  CleanPeople := CleanPeople + CleanDir;
  btRecentProgressClick(nil);

  LogEvent('- Cleaning 06/11 Movies/Actorious');
  for i :=  0 to 999 do
  begin
    CacheDir := AppCacheDir+'cache/movies/actorious/'+RightStr('000'+IntToStr(i),3);
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

  LogEvent('- Cleaning 07/11 Movies/TMDb');
  for i :=  0 to 999 do
  begin
    CacheDir := AppCacheDir+'cache/movies/tmdb/'+RightStr('000'+IntToStr(i),3);
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
  LogEvent('- Cleaning 08/11 TVShows/Top');
  CacheDir := AppCacheDir+'cache/tvshows/top1000';
  Cleantvshows := Cleantvshows + CleanDir;
  CacheDir := AppCacheDir+'cache/tvshows/top5000';
  Cleantvshows := Cleantvshows + CleanDir;
  btRecentProgressClick(nil);

  LogEvent('- Cleaning 09/11 TVShows/Actorious');
  for i :=  0 to 999 do
  begin
    CacheDir := AppCacheDir+'cache/tvshows/actorious/'+RightStr('000'+IntToStr(i),3);
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

  LogEvent('- Cleaning 10/11 TVShows/TMDb');
  for i :=  0 to 999 do
  begin
    CacheDir := AppCacheDir+'cache/tvshows/tmdb/'+RightStr('000'+IntToStr(i),3);
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

  // Clean Lookup Cache
  LogEvent('- Cleaning 11/11 Lookup Cache');
  CacheDir := AppCacheDir+'cache/lookup';
  CleanDir;

  // BirthdayCount is a count of the number of files in the cache/days/actorious-birthdays folder
  // This should be 366 * 4 = 1464. If the regen is slow, then the cleaning will delete files older
  // than 7 days that should have been regenerated after 5 days. We provide a count here so that
  // we can pass this info to Home Assistant and keep an eye on it there.
  BirthDaysCount := IntToStr(Length(TDirectory.GetFiles(AppCacheDir+'cache/days/actorious-births', '*')) div 4)+'d';
  LogEvent('- Cleaning Total Date Count: '+BirthDaysCount);

  LogEvent('Cache Clean Completed: '+FloatToStrF(CleanFiles-CleanNum,ffNumber,8,0)+' Files, '+FloatToStrF((CleanSize - CleanData)/(1024*1024),ffNumber,8,0)+' MB ('+FormatDateTime('HH:nn:ss.zzz',Now-CleanTime)+')');

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
          if d.getValue('TP') <> nil
          then  d2 := FormatDateTime('HH:nn:ss.zzz',(d.getValue('TP') as TJSONNumber).AsDouble-(d.getValue('TS') as TJSONNumber).AsDouble)
          else  d2 := FormatDateTime('HH:nn:ss.zzz',Now-(d.getValue('TS') as TJSONNumber).AsDouble);
          
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
    // let the Clean, Top1000, and Top5000 functions operate without interferenece
    QuietWindow := TimeOf(Now);
    if ((QuietWindow >= EncodeTime( 4, 50, 0, 0)) and (QuietWindow <= EncodeTime( 5, 59, 59, 0))  or
        (QuietWindow >= EncodeTime(17, 00, 0, 0)) and (QuietWindow <= EncodeTime(17, 59, 59, 0))) then
    begin
      CurrentProgressA.Caption := 'Waiting for Top Refresh to Complete (Retry in 60s)';
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
      CacheFile := AppCacheDir+'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
      if not(FileExists(CacheFile+'.working')) then
      begin
        if not(FileExists(CacheFile)) then
        begin
          Update := 'Generating BirthDay';
          AppCacheSkips := 0;
        end
        else
        begin
          if TFile.GetLastWriteTime(CacheFile) < (Now - 6) then
          begin
            Update := 'Refreshing BirthDay';
            AppCacheSkips := 0;
          end;
        end;
      end;
    end;

    // DeathDay Data
    if (update = 'Waiting') then
    begin
      CacheFile := AppCacheDir+'cache/days/actorious-deaths/deathday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
      if not(FileExists(CacheFile+'.working')) then
      begin
        if not(FileExists(CacheFile)) then
        begin
          Update := 'Generating DeathDay'
        end
        else
        begin
          if TFile.GetLastWriteTime(CacheFile) < (Now - 6)
          then Update := 'Refreshing DeathDay';
        end;
      end;
    end;

    // ReleaseDay Data
    if (update = 'Waiting') then
    begin
      CacheFile := AppCacheDir+'cache/days/actorious-releases/releaseday-'+RightStr('000'+IntToStr(CacheIndex),3)+'.json';
      if not(FileExists(CacheFile+'.working')) then
      begin
        if not(FileExists(CacheFile)) then
        begin
          Update := 'Generating Releases'
        end
        else
        begin
          if TFile.GetLastWriteTime(CacheFile) < (Now - 6)
          then Update := 'Refreshing Releases';
        end;
      end;
    end;

    // If we've run around a complete cycle, maybe make a note of that
    AppCacheSkips := AppCacheSkips + 1;
    if AppCacheSkips >= 366 then
    begin
      SendActivityLog('Cache update complete');
      AppCacheSkips := 0;

      // Everything up to date, so we don't need to check as frequently
      CacheTimer.Interval := 15000;
    end;

    // If just woke up from delay, let's shorten the scanning timef
    if CacheTimer.Interval > 15000
    then CacheTimer.Interval := 5000;

    if (Update <> 'Waiting') then
    begin

      // Found something, so don't want so long to find the next something
      CacheTimer.Interval := 5000;

      // Set a timer so we can track how long it is taking
      CurrentProgressA.Caption := Update+': '+TGUID.NewGUID.ToString;

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
      URL := URL+'&Progress='+CurrentProgressA.Caption;

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
        Client.ConnectionTimeout := 5400000;  // 90 minutes
        Client.ResponseTimeout := 5400000;    // 90 minutes
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
//        CacheTimer.Intervalf := 1000;
        CacheTimer.Enabled := True;
        CurrentProgressA.Caption := 'Scanning';
      end
    end
    else
    begin
      // If already processed, let's check the next one
      CacheTimer.Tag := CacheTimer.Tag + 1;
//      CacheTimer.Interval := 1000;
      CacheTimer.Enabled := True;
      CurrentProgressA.Caption := 'Scanning';
    end
  end
  else
  begin
    // Disabled, so wait 30s and check again
    CurrentProgressA.Caption := 'Timer Disabled (Retry in 30s)';
    CacheTimer.Interval := 30000;
    CacheTimer.Enabled := True;

    WaitingMessage := 'Timer Disabled (Retry in %s)';
    tmrWaiting.Tag := 30;
    tmrWaiting.Enabled := True;
  end;

end;

procedure TMainForm.DateTimePickerBirthdayCloseUp(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  if (DateTimePickerBirthDay.Tag <> DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePickerBirthDay.Date),DayOfTheMonth(DateTimePickerBirthDay.Date)))) then
  begin
    DateTimePickerBirthDay.Tag := DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePickerBirthDay.Date),DayOfTheMonth(DateTimePickerBirthDay.Date)));
    CacheTimer.Tag := DateTimePickerBirthDay.Tag;
    LogEvent('');
    LogEvent('Regenerating BirthDay Data [ '+FormatDateTime('mmmdd',DateTimePickerBirthDay.Date)+' / d'+IntToStr(DateTimePickerBirthDay.Tag)+' ]');
    CurrentProgressA.Caption := 'ManReGenBirthDay['+FormatDateTime('mmmdd',DateTimePickerBirthDay.Date)+'/d'+IntToStr(DateTimePickerBirthDay.Tag)+']: '+TGUID.NewGUID.ToString;
    CacheTimer.Enabled := False;

    progMonth.Text := FormatDateTime('mmm', DateTimePickerBirthDay.date);
    progDay.Text := FormatDateTime('dd', DateTimePickerBirthDay.date);
    progMonth.Enabled := True;
    progDay.Enabled := True;
    CacheTimer.Tag := DayOfTheYear(DateTimePickerBirthDay.Date);

    URL := AppURL+'/ActorInfoService/ActorBirthDay';

    // Setup the Request
    URL := URL+'?Secret='+edSecret.Text;
    URL := URL+'&aMonth='+IntToStr(MonthOf(DateTimePickerBirthDay.Date));
    URL := URL+'&aDay='+IntToStr(DayOf(DateTimePickerBirthDay.Date));
    URL := URL+'&Progress='+CurrentProgressA.Caption;

    // Submit the request (asynchronously)
    Client := TFancyNetHTTPClient.Create(nil);
    Client.Tag := DateTimeToUnix(Now);
    Client.Description := 'BirthDay:'+FormatDateTime('mmmdd',DateTimePickerBirthDay.Date)+'/d'+IntToStr(DateTimePickerBirthDay.Tag);
    client.Asynchronous := True;
    Client.ConnectionTimeout := 1800000; // 30 minutes
    Client.ResponseTimeout := 1800000; // 30 minutes
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

procedure TMainForm.DateTimePickerDeathDayCloseUp(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  if (DateTimePickerDeathDay.Tag <> DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePickerDeathDay.Date),DayOfTheMonth(DateTimePickerDeathDay.Date)))) then
  begin
    DateTimePickerDeathDay.Tag := DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePickerDeathDay.Date),DayOfTheMonth(DateTimePickerDeathDay.Date)));
    CacheTimer.Tag := DateTimePickerDeathDay.Tag;
    LogEvent('');
    LogEvent('Regenerating DeathDay Data [ '+FormatDateTime('mmmdd',DateTimePickerDeathDay.Date)+' / d'+IntToStr(DateTimePickerDeathDay.Tag)+' ]');
    CurrentProgressA.Caption := 'ManReGenDeathDay['+FormatDateTime('mmmdd',DateTimePickerDeathDay.Date)+'/d'+IntToStr(DateTimePickerDeathDay.Tag)+']: '+TGUID.NewGUID.ToString;
    CacheTimer.Enabled := False;

    progMonth.Text := FormatDateTime('mmm', DateTimePickerDeathDay.date);
    progDay.Text := FormatDateTime('dd', DateTimePickerDeathDay.date);
    progMonth.Enabled := True;
    progDay.Enabled := True;
    CacheTimer.Tag := DayOfTheYear(DateTimePickerDeathDay.Date);

    URL := AppURL+'/ActorInfoService/ActorDeathDay';

    // Setup the Request
    URL := URL+'?Secret='+edSecret.Text;
    URL := URL+'&aMonth='+IntToStr(MonthOf(DateTimePickerDeathDay.Date));
    URL := URL+'&aDay='+IntToStr(DayOf(DateTimePickerDeathDay.Date));
    URL := URL+'&Progress='+CurrentProgressA.Caption;

    // Submit the request (asynchronously)
    Client := TFancyNetHTTPClient.Create(nil);
    Client.Tag := DateTimeToUnix(Now);
    Client.Description := 'DeathDay:'+FormatDateTime('mmmdd',DateTimePickerDeathDay.Date)+'/d'+IntToStr(DateTimePickerDeathDay.Tag);
    client.Asynchronous := True;
    Client.ConnectionTimeout := 1800000; // 30 minutes
    Client.ResponseTimeout := 1800000; // 30 minutes
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

procedure TMainForm.DateTimePickerReleaseDayCloseUp(Sender: TObject);
var
  URL: String;
  Client: TFancyNetHTTPClient;
begin
  if (DateTimePickerReleaseDay.Tag <> DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePickerReleaseDay.Date),DayOfTheMonth(DateTimePickerReleaseDay.Date)))) then
  begin
    DateTimePickerReleaseDay.Tag := DayOfTheYear(EncodeDate(2020,MonthOfTheYear(DateTimePickerReleaseDay.Date),DayOfTheMonth(DateTimePickerReleaseDay.Date)));
    CacheTimer.Tag := DateTimePickerReleaseDay.Tag;
    LogEvent('');
    LogEvent('Regenerating Releases Data [ '+FormatDateTime('mmmdd',DateTimePickerReleaseDay.Date)+' / d'+IntToStr(DateTimePickerReleaseDay.Tag)+' ]');
    CurrentProgressA.Caption := 'ManReGenReleases['+FormatDateTime('mmmdd',DateTimePickerReleaseDay.Date)+'/d'+IntToStr(DateTimePickerReleaseDay.Tag)+']: '+TGUID.NewGUID.ToString;
    CacheTimer.Enabled := False;

    progMonth.Text := FormatDateTime('mmm', DateTimePickerReleaseDay.date);
    progDay.Text := FormatDateTime('dd', DateTimePickerReleaseDay.date);
    progMonth.Enabled := True;
    progDay.Enabled := True;
    CacheTimer.Tag := DayOfTheYear(DateTimePickerReleaseDay.Date);

    URL := AppURL+'/ActorInfoService/MovieReleaseDay';

    // Setup the Request
    URL := URL+'?Secret='+edSecret.Text;
    URL := URL+'&aMonth='+IntToStr(MonthOf(DateTimePickerReleaseDay.Date));
    URL := URL+'&aDay='+IntToStr(DayOf(DateTimePickerReleaseDay.Date));
    URL := URL+'&Progress='+CurrentProgressA.Caption;

    // Submit the request (asynchronously)
    Client := TFancyNetHTTPClient.Create(nil);
    Client.Tag := DateTimeToUnix(Now);
    Client.Description := 'Releases:'+FormatDateTime('mmmdd',DateTimePickerReleaseDay.Date)+'/d'+IntToStr(DateTimePickerReleaseDay.Tag);
    client.Asynchronous := True;
    Client.ConnectionTimeout := 1800000; // 30 minutes
    Client.ResponseTimeout := 1800000;  // 30 minutes
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
  ClientVer.SaveToFile(AppCacheDir+'clientversion.txt');
  ClientVer.Free;
end;

procedure TMainForm.ManualRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
var
  MainHandle : THandle;
begin
  LogEvent('Manual Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  LogEvent('');

  if (Sender as TFancyNetHTTPClient).Description  = 'Top 1000'
  then GetTopResults('https://www.actorious.com/?top-one-thousand=true','Top-1000-Cache');
  if (Sender as TFancyNetHTTPClient).Description  = 'Top 5000'
  then GetTopResults('https://www.actorious.com/?top-five-thousand=true','Top-5000-Cache');

  CurrentProgressA.Caption := 'Waiting';
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
  LogEvent('| Manual Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('HH:nn:ss',-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  LogEvent('| ERROR: '+AError);
  LogEvent('| COMMS: '+(Sender as TFancyNetHTTPClient).uRL);
  LogEvent('| ');

  CacheTimer.Enabled := True;
  CurrentProgressA.Caption := 'Waiting';
  Sender.Free;
end;

procedure TMainForm.NetHTTPClient1RequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
var
  MainHandle : THandle;
  Handling: String;
begin
  if Pos('BirthDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    Handling := 'Birthday';
    LogEvent('');
    LogEvent('BirthDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
    CurrentProgressA.Caption := 'Short API Delay (Continue in 120s)';
    CacheTimer.Interval := 120000; // 120 seconds
    CacheTimer.Enabled := True;

    WaitingMessage := 'Short API Delay (Continue in %s)';
    tmrWaiting.Tag := 120;
    tmrWaiting.Enabled := True;

    if FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)) <> '00:00:00' then
    begin
      lblBirthDays.Tag := lblBirthDays.Tag + 1;
      lblBirthDays.Caption := IntToStr(lblBirthDays.Tag);
      BirthDaysCount := IntToStr(Length(TDirectory.GetFiles(AppCacheDir+'cache/days/actorious-births', '*')) div 4)+'d';
    end;
  end
  else if Pos('DeathDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    Handling := 'DeathDay';
    LogEvent('DeathDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
    CurrentProgressA.Caption := 'Short API Delay (Continue in 120s)';
    CacheTimer.Interval := 120000; // 120 seconds
    CacheTimer.Enabled := True;

    WaitingMessage := 'Short API Delay (Continue in %s)';
    tmrWaiting.Tag := 120;
    tmrWaiting.Enabled := True;

    if FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)) <> '00:00:00' then
    begin
      lblDeathDays.Tag := lblDeathDays.Tag + 1;
      lblDeathDays.Caption := IntToStr(lblDeathDays.Tag);
    end;
  end
  else if Pos('Releases',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    Handling := 'Releases';
    LogEvent('Releases Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] Complete: '+FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
    CurrentProgressA.Caption := 'Long API Delay (Continue in 300s)';
    CacheTimer.Interval := 300000;  // 5 minutes
    CacheTimer.Enabled := True;

    WaitingMessage := 'Long API Delay (Continue in %s)';
    tmrWaiting.Tag := 300;
    tmrWaiting.Enabled := True;

    if FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)) <> '00:00:00' then
    begin
      lblReleaseDays.Tag := lblReleaseDays.Tag + 1;
      lblReleaseDays.Caption := IntToStr(lblReleaseDays.Tag);
    end;
  end;

  // What if it semi-failed?
  if FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)) = '00:00:00' then
  begin
    LogException('Processing Delay (15m)', 'PROCESSING',Handling, (Sender as TFancyNetHTTPClient).Description);

    CurrentProgressA.Caption := 'ERROR DELAY (Continue in 15m)';
    CacheTimer.Interval := 900000; // 900 seconds = 15m
    CacheTimer.Enabled := False;
    CacheTimer.Enabled := True;

    WaitingMessage := 'ERROR DELAY (Continue in %s)';
    tmrWaiting.Tag := 900;
    tmrWaiting.Enabled := False;
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

  // Update Search Index on disk
  SaveSearchIndexes;

end;

procedure TMainForm.NetHTTPClient1RequestError(const Sender: TObject; const AError: string);
begin
  if Pos('BirthDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('');
    LogEvent('BirthDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  end
  else if Pos('DeathDay',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('DeathDay Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
  end
  else if Pos('Releases',(Sender as TFancyNetHTTPClient).Description) > 0 then
  begin
    LogEvent('Releases Cache Update [ '+(Sender as TFancyNetHTTPClient).Description+' ] FAILED: '+FormatDateTime('HH:nn:ss',Now-UnixToDateTime((Sender as TFancyNetHTTPClient).Tag)));
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

  CurrentProgressA.Caption := 'Short API Delay (Continue in 10s)';
  CacheTimer.Interval := 90000; // 90 seconds
  CacheTimer.Enabled := True;

  WaitingMessage := 'Short API Delay (Continue in %s)';
  tmrWaiting.Tag := 90;
  tmrWaiting.Enabled := True;
end;

procedure TMainForm.SaveSearchIndexes;
var
  i: Integer;
  IndexFileName: String;
  IndexFile: TStringList;
  LookupJSON: TJSONArray;
  LookupItem: TPair<string, string>;
begin
  // Here we're just taking our search arrays and writing them out to disk.

  // people
  if Length(IndexPeople) > 0 then
  begin
    IndexFileName := AppCacheDir+'cache/search/IndexPeople.idx';
    IndexFile := TStringList.Create;

    i := 0;
    while i < Length(IndexPeople) do
    begin
      IndexFile.Add(IndexPeople[i]);
      i := i + 1;
    end;

    try
      IndexFile.SaveToFile(IndexFileName, TEncoding.UTF8);
      IndexFile.Free;
      IndexPeopleSize := FileSizeByName(IndexFileName);
      btRecentProgressClick(nil);
    except on E: Exception do
      begin
        LogException('Search Index Update Failed',E.ClassName, E.Message, IndexFileName);
      end;
    end;
  end;

  // Similarly, we'll save the Lookup Cache at the same time. This is much smaller, just a list of
  // Lookup hashes and filenames, but still a bit tedious to get into a usable text file.
  LookupJSON := TJSONArray.Create;
  IndexFile := TStringList.Create;
  for LookupItem in LookupCache do
  begin
    LookupJSON.Add( LookupItem.Key + ' : '+LookupItem.Value );
  end;
  IndexFile.Text := LookupJSON.toString;
  IndexFile.SaveToFile(AppCacheDir+'cache/lookup/LookupIndex.json');
  IndexFile.free;
  LookupJSON.Free;

end;

procedure TMainForm.LoadSearchIndexes;
var
  i,j: Integer;
  IndexFileName: String;
  IndexFile: TStringList;
  LookupJSON: TJSONArray;
begin
  // Here we're just loading our search arrays from disk

  // People
  IndexFileName := AppCacheDir+'cache/search/IndexPeople.idx';
  if FileExists(IndexFileName) then
  begin
    IndexFile := TStringList.Create;

    IndexFile.LoadFromFile(IndexFileName);

    if IndexFile.Count > 0 then
    begin
      i := 0;
      j := 0;
      while i < IndexFile.Count do
      begin
        if (Pos(' Pending', IndexFile[i]) = 0) and
           (IndexFile[i] <> '') then
        begin
          SetLength(IndexPeople, j + 1);
          IndexPeople[j] := IndexFile[i];
          j := j + 1;
        end;
        i := i + 1;
      end;

      lblSearchPeople.Caption := FloatToStrF(IndexFile.Count,ffNumber,6,0);
      IndexPeopleSize := FileSizeByName(IndexFileName);
      IndexFile.Free;

      btRecentProgressClick(nil);
    end;
  end;

  // Load Lookup Cache
  if FileExists(AppCacheDir+'cache/lookup/LookupIndex.json') then
  begin

    LookupJSON := TJSONArray.Create;
    IndexFile := TStringList.Create;
    IndexFile.LoadFromFile(AppCacheDir+'cache/lookup/LookupIndex.json');
    if IndexFile.Count > 0 then
    begin
      LookupJSON := TJSONObject.ParseJSONValue(IndexFile.Text) as TJSONArray;
      if LookupJSON.Count > 0 then
      begin
        i := 0;
        while i < LookupJSON.Count do
        begin
          if FileExists(Copy(LookupJSON.Items[i].Value,68,Length(LookupJSON.Items[i].Value))) then
          begin
            LookupCache.Add(Copy(LookupJSON.Items[i].Value,1,64), Copy(LookupJSON.Items[i].Value,68,Length(LookupJSON.Items[i].Value)));
          end;
          i := i + 1;
        end;
      end;
    end;
    LookupJSON.Free;
    IndexFile.Free;
  end;

end;

function TMainForm.SearchPeople(ActorID: Integer):Integer;
var
  i: Integer;
  SearchTerm: String;
  SearchEnd: Boolean;

begin
  i := 0;
  Result := -1;
  SearchTerm := RightStr('00000000'+IntToStr(ActorID),8);

  SearchEnd := False;
  while not(SearchEnd) do
  begin
    // Not found, so let's add it
    if i = Length(IndexPeople)  then
    begin
      Result := i;
      SetLength(IndexPeople,i+1);
      lblSearchPeople.Caption := FloatToStrF(i+1,ffNumber,6,0);

      IndexPeople[i] := 'Pending';
      SearchEnd := True;
    end;

    // Found it
    if Copy(IndexPeople[i],2,8) = SearchTerm then
    begin
      Result := i;
      SearchEnd := True;
    end;

    i := i + 1;
  end;

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
        else Msg1.Subject := '['+GetEnvironmentVariable('COMPUTERNAME')+'] '+Subject+': '+MainForm.Caption+' ('+FormatDateTime('HH:nn:ss', Now - AppStartup)+'}';

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
      LogEvent('WARNING: '+Subject+' e-mail to '+MailServerName+' <'+MailServerFrom+'> FAILED');
      LogEvent('WARNING: SMTP Error: '+SMTPResult);
    end;
  end;
end;

procedure TMainForm.StartTimerTimer(Sender: TObject);
var
  i: integer;
  AppConfigFile: String;
  ConfigFile: TStringList;

  oldestdate: Integer;
  oldesttime: TDateTime;
  oldestfile: Integer;
  oldestname: String;
  oldestchks: Integer;
  oldestmesg: String;

begin
  StartTimer.Enabled := False;

  UpdateProgress(1, 'Welcome to Actorious','Server Startup', '0 of 17','');
  LogEvent('');
  LogEvent('______________________________________________________');
  LogEvent('');
  LogEvent('SERVER STARTUP');


  // List of App Parameters
  AppParameters := TStringList.Create;
  AppParameters.QuoteChar := ' ';
  GetAppParameters(AppParameters);

  // Load JSON Configuration
  UpdateProgress(1, 'Loading Configuration','Server Startup', '1 of 17','');
  LogEvent('');
  LogEvent('Loading Configuration');

  AppConfigFile := 'Actorious.json';
  i := 0;
  while i < AppParameters.Count do
  begin
    if Pos('"CONFIG=',UpperCase(AppParameters[i])) = 1
    then AppConfigFile  := Copy(AppParameters[i],9,length(AppParameters[i])-9);
    i := i + 1;
  end;

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

  if Appconfiguration = nil then
  begin
    // Create an empty AppConfiguration
    LogEvent('- Invalid Configuration');
    AppConfiguration := TJSONObject.Create;
  end;

  // Used to access this Actorious REST API
  UpdateProgress(1, 'Retrieving Actorious API Secret','Server Startup', '2 of 17','');

  if AppConfiguration.getValue('Actorious API Secret') <> nil then
  begin
    edSecret.Text := (AppConfiguration.getValue('Actorious API Secret') as TJSONString).Value;
    LogEvent('- Actorious API Secret Loaded');
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [Actorious API Secret]');
  end;
  edSecretChange(nil);

  // Used to access The Movie Database API
  UpdateProgress(1, 'Retrieving TMDb API Key','Server Startup', '3 of 17','');

  if AppConfiguration.getValue('TMDb API Key') <> nil then
  begin
    edTMDbAPI.Text := (AppConfiguration.getValue('TMDb API Key') as TJSONString).Value;
    LogEvent('- TMDb API Key Loaded');
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [TMDb API Key]');
  end;

  // BaseURL
  UpdateProgress(1, 'Retrieving BaseURL','Server Startup', '4 of 17','');

  if AppConfiguration.getValue('BaseURL') <> nil then
  begin
    AppBaseURL := (AppConfiguration.getValue('BaseURL') as TJSONString).Value;
    LogEvent('- BaseURL set to '+AppBaseURL);
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [BaseURL]');
  end;

  // AppURL
  UpdateProgress(1, 'Retrieving AppURL','Server Startup', '5 of 17','');

  if AppConfiguration.getValue('AppURL') <> nil then
  begin
    AppURL := (AppConfiguration.getValue('AppURL') as TJSONString).Value;
    LogEvent('- AppURL set to '+AppURL);
  end
  else
  begin
    LogEvent('- ERROR: Missing Required Entry For [AppURL]');
  end;

  // AppCacheDir
  UpdateProgress(1, 'Retrieving AppCacheDir','Server Startup', '6 of 17','');

  if AppConfiguration.getValue('AppCacheDir') <> nil then
  begin
    AppCacheDir := StringReplace(Trim((AppConfiguration.getValue('AppCacheDir') as TJSONString).Value),'\','/',[rfReplaceAll]);
    if RightSTr(AppCacheDir,1) <> '/' then AppCacheDir := AppCacheDir + '/';
    if AppCacheDir = '/' then AppCacheDir := '';
    LogEvent('- AppCacheDir set to [ '+AppCacheDir+' ]');
  end
  else
  begin
    LogEvent('- WARNING: Missing Entry For [AppCacheDir]');
    AppCacheDir := '';
  end;
  ForceDirectories(AppCacheDir+'cache/days/actorious-births');
  BirthDaysCount := IntToStr(Length(TDirectory.GetFiles(AppCacheDir+'cache/days/actorious-births', '*')) div 4)+'d';
  LogEvent('- Available Cache History: '+BirthDaysCount);

  // Swagger Support
  UpdateProgress(1, 'Configuring SwaggerUI Support','Server Startup', '7 of 17','');

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

  // Redoc Support
  UpdateProgress(1, 'Configuring Redoc Support','Server Startup', '8 of 17','');

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

  // HomeAssistant Support
  UpdateProgress(1, 'Configuring Home Assistant Support','Server Startup', '9 of 17','');

  if AppConfiguration.getValue('HA_URL') <> nil then
  begin
    AppHAURL := (AppConfiguration.getValue('HA_URL') as TJSONString).Value;
    if AppHAURL <> ''
    then LogEvent('- Home Assistant configured at '+AppHAURL)
    else LogEvent('- Home Assistant not configured');
  end
  else
  begin
    AppHAURL := '';
    LogEvent(' - HomeAssistant not configured');
  end;

  // HomeAssistant Token
  AppHAToken := '';
  if AppConfiguration.getValue('HA_Token') <> nil then
  begin
    AppHAToken := (AppConfiguration.getValue('HA_Token') as TJSONString).Value;
    if Trim(AppHAToken) <> ''
    then LogEvent('- Home Assistant Token loaded ');
  end;

  // Get Mail Configuration
  UpdateProgress(1, 'Configuring Mail Services','Server Startup', '10 of 17','');

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

  LogEvent('Configuration Loaded');
  LogEvent('');

  Application.ProcessMessages;

  // Kick off Cache Populator
  UpdateProgress(1, 'Enabling Cache Timer','Server Startup', '11 of 17','');

  CacheTimer.Interval := 30000;
  CacheTimer.Enabled := True;

  WaitingMessage := 'Startup Delay (Continue in %s)';
  tmrWaiting.Tag := 30;
  tmrWaiting.Enabled := True;

  // Change URL of server depending on machine it is running on
  UpdateProgress(1, 'Starting Server','Server Startup', '12 of 17','');

  if AppBaseURL <> '' then
  begin
    ServerContainer.XDataServer.BaseURL := AppBaseURL;
    ServerContainer.SparkleHttpSysDispatcher.Active := True;
  end;

  // Create Cache directory structure
  UpdateProgress(1, 'Creating Cache Directories','Server Startup', '13 of 17','');
  LogEvent('Creating Cache Directories');

  ForceDirectories(AppCacheDir+'cache'); // Cache Root

  CreateDir(AppCacheDir+'cache/people');                  // Data cached by TMDb ID, either Actors or Directors or Writers
  CreateDir(AppCacheDir+'cache/people/tmdb');             // JSON as it originated from TMDb
  CreateDir(AppCacheDir+'cache/people/actorious');        // JSON formatted for Actorious
  CreateDir(AppCacheDir+'cache/people/top1000');          // Top 1000 all ready to go
  CreateDir(AppCacheDir+'cache/people/top5000');          // Top 5000 all ready to go

  CreateDir(AppCacheDir+'cache/days');                    // Data cached by Julian Day
  CreateDir(AppCacheDir+'cache/days/actorious-births');   // People with this birthday
  CreateDir(AppCacheDir+'cache/days/actorious-deaths');   // People with this birthday
  CreateDir(AppCacheDir+'cache/days/actorious-releases'); // People with this birthday
  CreateDir(AppCacheDir+'cache/days/first');              // The first person to appear for this given birthday
  CreateDir(AppCacheDir+'cache/days/wikidata-births');    // Wikidata response to this birthday
  CreateDir(AppCacheDir+'cache/days/wikidata-deaths');    // Wikidata response to this deathday
  CreateDir(AppCacheDir+'cache/days/wikidata-releases');  // Wikidata response to this releaseday
  CreateDir(AppCacheDir+'cache/days/toptoday');

  CreateDir(AppCacheDir+'cache/movies');
  CreateDir(AppCacheDir+'cache/movies/tmdb');
  CreateDir(AppCacheDir+'cache/movies/actorious');
  CreateDir(AppCacheDir+'cache/movies/top1000');
  CreateDir(AppCacheDir+'cache/movies/top5000');

  CreateDir(AppCacheDir+'cache/tvshows');
  CreateDir(AppCacheDir+'cache/tvshows/tmdb');
  CreateDir(AppCacheDir+'cache/tvshows/actorious');
  CreateDir(AppCacheDir+'cache/tvshows/top1000');
  CreateDir(AppCacheDir+'cache/tvshows/top5000');

  CreateDir(AppCacheDir+'cache/search');
  CreateDir(AppCacheDir+'cache/lookup');

  // Loading Search Indexes
  UpdateProgress(1, 'Loading Search Indexes','Server Startup', '14 of 17','');
  LogEvent('Loading Search Indexes');

  LoadSearchIndexes;
  LogEvent('- People Search Index Loaded: '+IntToStr(Length(IndexPeople))+' entries');
  LogEvent('- Lookup Index Loaded: '+IntToStr(LookupCache.Count)+' entries');

  // Find oldest file to start on
  UpdateProgress(1, 'Determining Cache Start Date','Server Startup', '15 of 17','');

  oldestdate := DayOfTheYear(EncodeDate(2020, MonthOf(Now), DayOftheMonth(Now)));
  oldesttime := Now;
  oldestchks := 0;
  oldestname := '';
  oldestmesg := '';

  oldestfile := oldestdate;
  
  while oldestchks < 366 do
  begin

    oldestname := AppCacheDir+'cache/days/actorious-births/birthday-'+RightStr('000'+IntToStr(oldestfile),3)+'.json';
    if not(FileExists(oldestname)) then
    begin
      oldestdate := oldestfile;
      oldestchks := 367;
      oldestmesg := ' (missing)';
    end
    else if TFile.getLAstWriteTime(oldestname) < oldesttime then
    begin
      oldesttime := TFile.getLastWriteTime(oldestname);
      oldestdate := oldestfile;
      oldestmesg := FormatDateTime(' dd"d" hh"h" mm"m" ss"s"',Now - oldesttime);
    end;

    oldestname := AppCacheDir+'cache/days/actorious-deaths/deathday-'+RightStr('000'+IntToStr(oldestfile),3)+'.json';
    if not(FileExists(oldestname)) then
    begin
      oldestdate := oldestfile;
      oldestchks := 367;
      oldestmesg := ' (missing)';
    end
    else if TFile.getLAstWriteTime(oldestname) < oldesttime then
    begin
      oldesttime := TFile.getLastWriteTime(oldestname);
      oldestdate := oldestfile;
      oldestmesg := FormatDateTime(' dd"d" hh"h" mm"m" ss"s"',Now - oldesttime);
    end;

    oldestname := AppCacheDir+'cache/days/actorious-releases/releaseday-'+RightStr('000'+IntToStr(oldestfile),3)+'.json';
    if not(FileExists(oldestname)) then
    begin
      oldestdate := oldestfile;
      oldestchks := 367;
      oldestmesg := ' (missing)';
    end
    else if TFile.getLAstWriteTime(oldestname) < oldesttime then
    begin
      oldesttime := TFile.getLastWriteTime(oldestname);
      oldestdate := oldestfile;
      oldestmesg := FormatDateTime(' dd"d" hh"h" mm"m" ss"s"',Now - oldesttime);
    end;

    oldestfile := oldestfile + 1;
    if oldestfile = 367 then oldestfile := 1;

    oldestchks := oldestchks + 1;

    Application.ProcessMessages;
  end;

  LogEvent('Oldest Data found is for DY: '+IntToStr(oldestdate)+' / DT: '+FormatDateTime('mmmdd',EncodeDate(2020, 1, 1) + (oldestdate - 1))+oldestmesg);
  progMonth.Text := FormatDateTime('mmm',EncodeDate(2020, 1, 1) + (oldestdate - 1));
  progDay.Text   := FormatDateTime('dd',EncodeDate(2020, 1, 1) + (oldestdate - 1));
  CacheTimer.Tag := oldestdate;

  // Check for new ActoriousClient Version right away
  UpdateProgress(1, 'Retrieving Current Client Version','Server Startup', '16 of 17','');
  LogEvent('Updating Client Version');

  btUpdateVersionClick(nil);

  // Display the progress at start
  UpdateProgress(1, 'Updating Statistics','Server Startup', '17 of 17','');
  LogEvent('Updating Statistics');
  btRecentProgressClick(Sender);

  LogEvent('');
  LogEvent('SERVER STARTUP COMPLETE ('+IntToStr(MillisecondsBetween(Now, AppStartup))+'ms)');
  LogEvent('______________________________________________________');
  LogEvent('');

  // Send an email if so configured
  SendActivityLog('Startup Confirmation');

  ProgressDetailA.Caption := 'Startup Complete';

  // Default XData UpdateGUI call
  UpdateGUI;

  UpdateProgress(1, '', '', '','');

end;

procedure TMainForm.UpdateProgress(PNum: Integer; PInfo: String; PDetail:String; PStep:String; PReport: String);
var
  PValue: Integer;
  PMax: Integer;
  PDesc: String;
  PDuration: String;
  PStart: TDateTime;
begin
  PValue := 0;
  PMax := 1;
  PDuration := '';
  PStart := Now;

  if PStep <> '' then
  begin
    if Pos(' of ', PStep) > 0 then
    begin
      if Pos('"TS":', PReport) > 0 then
      begin
        PStart := EncodeDateTime(
          StrToInt(Copy(PReport, Pos('"ST":',PReport)+6, 4)),
          StrToInt(Copy(PReport, Pos('"ST":',PReport)+11,2)),
          StrToInt(Copy(PReport, Pos('"ST":',PReport)+14,2)),
          StrToInt(Copy(PReport, Pos('"ST":',PReport)+17,2)),
          StrToInt(Copy(PReport, Pos('"ST":',PReport)+20,2)),
          StrToInt(Copy(PReport, Pos('"ST":',PReport)+23,2)),
          StrToInt(Copy(PReport, Pos('"ST":',PReport)+26,3))
        );
      end;

      PValue := StrToIntDef(Trim(Copy(PStep,1, Pos(' of ', PStep) - 1)),0);
      PMax := StrToIntDef(Trim(Copy(PStep, Pos(' of ', PStep)+4,Length(PStep))),10001);

      if (PValue = 0) or (PMax = 10001) or (PMax = 0) then
      begin
        PValue := 0;
        PMax := 1;
      end
    end;
  end;

  if PValue > PMax then
  begin
    PValue := 1;
    PMax := 1;
  end;

  if (PValue > 0) and (PMax > 1)  and (PValue < PMax) then
  begin
    PDuration := FormatDateTime('nn:ss" Elapsed / "', Now - PStart)+FormatDateTime('nn:ss" Remaining: "',(Trunc(SecondsBetween(Now, PStart) * PMax / PValue) - SecondsBetween(Now, Pstart))/86400.0);
  end;

  PDesc := StringReplace(PInfo, '"DY":"', 'Day! ', [rfReplaceAll]);
  PDesc := StringReplace(PDesc, '"DT":"', 'Date! ', [rfReplaceAll]);
  PDesc := StringReplace(PDesc,',',' ', [rfReplaceAll]);
  PDesc := StringReplace(PDesc,'"',' ', [rfReplaceAll]);
  PDesc := StringReplace(PDesc,':',' ', [rfReplaceAll]);
  PDesc := StringReplace(PDesc, 'Day!', 'Day:', [rfReplaceAll]);
  PDesc := StringReplace(PDesc, 'Date!', 'Date:', [rfReplaceAll]);

  if PNum = 1 then
  begin
    CurrentProgressA.Caption := PDesc;
    ProgressDetailA.Caption := PDetail;
    ProgressStepA.Caption := PDuration + PStep;
    ShapeProgressFGA.Width := Trunc((PValue / PMax) * (ShapeProgressBG.Width));
  end
  else if PNum = 2 then
  begin
    CurrentProgressB.Caption := PDesc;
    ProgressDetailB.Caption := PDetail;
    ProgressStepB.Caption := PDuration + PStep;
    ShapeProgressFGB.Width := Trunc((PValue / PMax) * (ShapeProgressBG.Width));
  end
  else if PNum = 3 then
  begin
    CurrentProgressC.Caption := PDesc;
    ProgressDetailC.Caption := PDetail;
    ProgressStepC.Caption := PDuration + PStep;
    ShapeProgressFGC.Width := Trunc((PValue / PMax) * (ShapeProgressBG.Width));
  end
  else if PNum = 4 then
  begin
    CurrentProgressD.Caption := PDesc;
    ProgressDetailD.Caption := PDetail;
    ProgressStepD.Caption := PDuration + PStep;
    ShapeProgressFGD.Width := Trunc((PValue / PMax) * (ShapeProgressBG.Width));
  end
  else if PNum = 5 then
  begin
    CurrentProgressE.Caption := PDesc;
    ProgressDetailE.Caption := PDetail;
    ProgressStepE.Caption := PDuration + PStep;
    ShapeProgressFGE.Width := Trunc((PValue / PMax) * (ShapeProgressBG.Width));
  end
  else if PNum = 6 then
  begin
    CurrentProgressF.Caption := PDesc;
    ProgressDetailF.Caption := PDetail;
    ProgressStepF.Caption := PDuration + PStep;
    ShapeProgressFGF.Width := Trunc((PValue / PMax) * (ShapeProgressBG.Width));
  end

end;

procedure TMainForm.tmrProgressTimer(Sender: TObject);
var
  i: Integer;
  PReport: String;
  ProgNum: Integer;
  ProgDetail: String;
  ProgStep: String;
  ProgFunction: String;

begin
  ProgNum := 1;

  // If the server has been running for awhile. Progress.Count will be large
  // So only check the most recent entries.  How far back?  Well, that will
  // depend somewhat on how much activity is going on.

  i := Progress.Count-1;
  while (i >= Max(0, Progress.Count-25)) and (ProgNum < 7) do
  begin
    if (Pos('"PR":"Complete', Progress[i]) = 0)  then
    begin
      ProgNum := ProgNum + 1;

      // Get the PR element
      PReport := Copy(Progress[i], Pos('"PR":', Progress[i])+6, 999);
      PReport := Copy(PReport,1,Pos('"TP":', PReport)-3);
      ProgFunction := Copy(Progress[i], Pos('"RQ":', Progress[i])+6, 999);
      ProgFunction := Copy(ProgFunction,1,Pos('"PR":', ProgFunction)-3);

      // Does this have any detail
      if Pos('(', PReport) > 0 then
      begin
        ProgDetail := Copy(PReport,1,Pos('(',PReport)-2);
        ProgStep := Trim(StringReplace(StringReplace(Copy(PReport,Pos('(',PReport),99),'(','',[rfReplaceAll]),')','',[rfReplaceAll]));
      end
      else
      begin
        ProgDetail := PReport;
        ProgStep := '';
      end;
      UpdateProgress(ProgNum, ProgFunction, ProgDetail, ProgStep, Progress[i]);

    end;

    i := i - 1;
  end;

  // How soon do we want to do this again?
  if ProgNum > 1 then
  begin
    tmrProgress.Interval := 1000;
    tmrProgress.Tag := tmrProgress.Tag + 1;

    // We still want to update the statistics periodically
    if (tmrProgress.tag mod 10) = 0
    then btRecentProgressClick(Sender);
  end
  else
  begin
    tmrProgress.Interval := 10000;
    btRecentProgressClick(Sender);
  end;

  // Clear out any empty slots
  while ProgNum < 7 do
  begin
    ProgNum := ProgNum + 1;
    UpdateProgress(ProgNum, '', '', '', '');
  end;




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
    SaveSearchIndexes;
    exit;
  end;

  // 6PM Run

//  WindowStart := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 00, 0, 0);
//  WindowEnd   := EncodeDateTime(YearOf(now), MonthOf(Now), DayOf(now), 17, 05, 0, 0);
//  if (WindowStart < Now) and (WindowEnd > Now) and (btClean.Tag <> Today) then
//  begin
//    btClean.Tag := Trunc(Today);
//    btCleanClick(Sender);
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
    SaveSearchIndexes;
    exit;
  end;

end;

procedure TMainForm.tmrVersionCheckTimer(Sender: TObject);
begin
  btUpdateVersionClick(Sender)
end;

procedure TMainForm.tmrWaitingTimer(Sender: TObject);
begin
  if (Pos('Waiting', CurrentProgressA.Caption) > 0) or
     (Pos('Delay',   CurrentProgressA.Caption) > 0) or
     (Pos('Retry',   CurrentProgressA.Caption) > 0) or
     ((CurrentProgressA.Caption = '') and (WaitingMessage <> '')) then
  begin
    tmrWaiting.Tag := tmrWaiting.Tag - 1;
    CurrentProgressA.Caption := StringReplace(WaitingMessage, '%s', IntToStr(tmrWaiting.Tag)+'s',[]);
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

  // How many days have we skipped because they were not out of date?
  AppCacheSkips := 0;

  // Initialize Index Sizes
  IndexPeopleSize := 0;
  IndexMoviesSize := 0;
  IndexTVShowSize := 0;

  // Sort out the Server Version
  GetAppVersionString;

  // Initialize lookup cache
  LookupCache := TDictionary<String,String>.Create;

  // Having a dark form in the IDE makes it hard to read component names
  MainForm.Color := clBlack;

  // Initialize Progress History
  Progress := TStringList.Create;

  // Initialize Progress UI
  CurrentProgressA.Caption := '';
  CurrentProgressB.Caption := '';
  CurrentProgressC.Caption := '';
  CurrentProgressD.Caption := '';
  CurrentProgressE.Caption := '';
  CurrentProgressF.Caption := '';

  ShapeProgressFGA.Width := 0;
  ShapeProgressFGB.Width := 0;
  ShapeProgressFGC.Width := 0;
  ShapeProgressFGD.Width := 0;
  ShapeProgressFGE.Width := 0;
  ShapeProgressFGF.Width := 0;

  ProgressDetailA.Caption := '';
  ProgressDetailB.Caption := '';
  ProgressDetailC.Caption := '';
  ProgressDetailD.Caption := '';
  ProgressDetailE.Caption := '';
  ProgressDetailF.Caption := '';

  ProgressStepA.Caption := '';
  ProgressStepB.Caption := '';
  ProgressStepC.Caption := '';
  ProgressStepD.Caption := '';
  ProgressStepE.Caption := '';
  ProgressStepF.Caption := '';

  // Avoid divide by zero errors
  PersonCacheRequests := 0;
  MovieCacheRequests  := 0;
  TVShowCacheRequests := 0;
  CleanRequests       := 0;

  // Do it this way so we don't wait for screen to appear
  StartTimer.Enabled := True;

end;


procedure TMainForm.FormResize(Sender: TObject);
begin
//  Panel1.Width := Max(mmStats.Width + mmInfo.Width + 2,1699);
//  mmInfo.Width := 1920-mmStats.Width - 6;
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
