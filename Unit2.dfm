object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Actorious XData Server'
  ClientHeight = 812
  ClientWidth = 1126
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI Semilight'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMinimized
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    1126
    812)
  PixelsPerInch = 96
  TextHeight = 13
  object mmStats: TMemo
    Left = 8
    Top = 116
    Width = 304
    Height = 691
    Anchors = [akLeft, akTop, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clSilver
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 6
  end
  object mmInfo: TMemo
    Left = 316
    Top = 116
    Width = 802
    Height = 691
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clSilver
    Ctl3D = False
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object sparqlBirthDays: TMemo
    Left = 18
    Top = 130
    Width = 354
    Height = 200
    Lines.Strings = (
      
        'SELECT DISTINCT ?person (max(?DOBs) as ?DOB) (max(?DODs) as ?DOD' +
        ') ?TMDbID ?RTID ?Wikipedia ?ModelID ?MetaCriticID ?Height ?Count' +
        'ryCode WHERE '
      '{'
      '  ?person p:P569  ?statement_1; '
      '          p:P4985 ?statement_3.'
      '  '
      '  ?statement_1 rdf:type wikibase:BestRank.'
      '  ?statement_1 psv:P569 ?statementValue_1.'
      '  ?statementValue_1 wikibase:timeValue ?DOBs.'
      '  FILTER (month(?DOBs) = :MONTH )'
      '  FILTER (day(?DOBs) = :DAY )'
      '  '
      '  ?statement_3 rdf:type wikibase:BestRank.'
      '  ?statement_3 ps:P4985 ?TMDbID.  '
      ''
      '  OPTIONAL {'
      '    ?person p:P570 ?statement_2.'
      '    ?statement_2 rdf:type wikibase:BestRank.    '
      '    ?statement_2 psv:P570 ?statementValue_2.'
      '    ?statementValue_2 wikibase:timeValue ?DODs.'
      '  } '
      '  OPTIONAL {'
      '   ?person p:P2048 ?statement4.'
      '   ?statement4 ps:P2048 ?Height.  '
      '  }'
      '  OPTIONAL {'
      '   ?person wdt:P27 ?citizenship.'
      '   ?citizenship wdt:P17 ?country.'
      '   ?country wdt:P297 ?CountryCode.'
      '  }'
      '  OPTIONAL{'
      '    ?person p:P1258 ?statement_5.'
      '    ?statement_5 ps:P1258 ?RTID.'
      '  }'
      '  OPTIONAL{'
      '    ?person p:P2471 ?statement_6.'
      '    ?statement_6 ps:P2471 ?ModelID.'
      '  }'
      '  OPTIONAL{'
      '    ?person p:P1712 ?statement_7.'
      '    ?statement_7 ps:P1712 ?MetaCriticID.'
      '  }'
      '  OPTIONAL{'
      '    ?Wikipedia schema:about ?person .'
      '    ?Wikipedia schema:inLanguage "en" .'
      '    ?Wikipedia schema:isPartOf <https://en.wikipedia.org/> .'
      '  }'
      '  '
      
        '  SERVICE wikibase:label { bd:serviceParam wikibase:language "en' +
        '". }'
      '}'
      
        'GROUP BY ?person ?TMDbID ?RTID ?Wikipedia ?ModelID ?MetaCriticID' +
        ' ?Height ?CountryCode'
      'LIMIT 1000')
    TabOrder = 1
    Visible = False
    WordWrap = False
  end
  object sparqlDeathDays: TMemo
    Left = 378
    Top = 127
    Width = 354
    Height = 200
    Lines.Strings = (
      
        'SELECT DISTINCT ?person (max(?DOBs) as ?DOB) (max(?DODs) as ?DOD' +
        ') ?TMDbID ?RTID ?Wikipedia ?ModelID ?MetaCriticID WHERE '
      '{'
      '  ?person p:P570  ?statement_1;'
      '          p:P569  ?statement_2;'
      '          p:P4985 ?statement_3.'
      '  '
      '  OPTIONAL{'
      '    ?person p:P1258 ?statement_4.'
      '    ?statement_4 ps:P1258 ?RTID.'
      '  }'
      '  OPTIONAL{'
      '    ?person p:P2471 ?statement_5.'
      '    ?statement_5 ps:P2471 ?ModelID.'
      '  }'
      '  OPTIONAL{'
      '    ?person p:P1712 ?statement_6.'
      '    ?statement_6 ps:P1712 ?MetaCriticID.'
      '  }'
      '  OPTIONAL{'
      '    ?Wikipedia schema:about ?person .'
      '    ?Wikipedia schema:inLanguage "en" .'
      '    ?Wikipedia schema:isPartOf <https://en.wikipedia.org/> .'
      '  }'
      ''
      '  ?statement_3 rdf:type wikibase:BestRank.   '
      '  ?statement_3 ps:P4985 ?TMDbID.'
      '  '
      '  ?statement_2 rdf:type wikibase:BestRank.'
      '  ?statement_2 psv:P569 ?statementValue_2.'
      '  ?statementValue_2 wikibase:timeValue ?DOBs.'
      '  '
      '  ?statement_1 rdf:type wikibase:BestRank.'
      '  ?statement_1 psv:P570 ?statementValue_1.'
      '  ?statementValue_1 wikibase:timeValue ?DODs.'
      '  FILTER (month(?DODs) = :MONTH )'
      '  FILTER (day(?DODs) = :DAY )'
      ''
      
        '  SERVICE wikibase:label { bd:serviceParam wikibase:language "en' +
        '". }'
      '}'
      'GROUP BY ?person ?TMDbID ?RTID ?Wikipedia ?ModelID ?MetaCriticID'
      'LIMIT 1000')
    TabOrder = 2
    Visible = False
    WordWrap = False
  end
  object sparqlReleaseDays: TMemo
    Left = 738
    Top = 130
    Width = 354
    Height = 200
    Lines.Strings = (
      
        'SELECT DISTINCT ?movie ?Releases ?TMDbID  ?RTID ?Wikipedia ?Meta' +
        'CriticID WHERE {'
      
        '  SERVICE wikibase:label { bd:serviceParam wikibase:language "en' +
        '". }'
      '  {'
      
        '    SELECT DISTINCT ?movie ?Releases ?TMDbID ?RTID ?Wikipedia ?M' +
        'etaCriticID WHERE {'
      '      ?movie p:P577  ?statement_1;'
      '             p:P4947 ?statement_2.'
      ''
      '      OPTIONAL{'
      '        ?movie p:P1258 ?statement_3.'
      '        ?statement_3 ps:P1258 ?RTID.'
      '     }'
      '     OPTIONAL{'
      '        ?movie p:P1712 ?statement_5.'
      '        ?statement_5 ps:P1712 ?MetaCriticID.'
      '      }'
      '      OPTIONAL{'
      '        ?Wikipedia schema:about ?movie .'
      '        ?Wikipedia schema:inLanguage "en" .'
      '        ?Wikipedia schema:isPartOf <https://en.wikipedia.org/> .'
      '      }'
      '  '
      '      ?statement_2 rdf:type wikibase:BestRank.'
      '      ?statement_2 ps:P4947 ?TMDbID.'
      '      '
      '      ?statement_1 rdf:type wikibase:BestRank.'
      '      ?statement_1 psv:P577 ?statementValue_1.'
      '      ?statementValue_1 wikibase:timeValue ?Releases.'
      '      FILTER (month(?Releases) = :MONTH )'
      '      FILTER (day(?Releases) = :DAY )'
      ''
      '    }'
      '    LIMIT 1000'
      '  }'
      '}')
    TabOrder = 3
    Visible = False
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 8
    Top = 4
    Width = 889
    Height = 108
    BevelOuter = bvNone
    Color = clSilver
    ParentBackground = False
    TabOrder = 4
    object lbSecret: TLabel
      Left = 80
      Top = 8
      Width = 56
      Height = 25
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Secret'
      Layout = tlCenter
    end
    object Label1: TLabel
      Left = 80
      Top = 33
      Width = 56
      Height = 25
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Base64'
      Layout = tlCenter
    end
    object lbTMDbAPI: TLabel
      Left = 80
      Top = 58
      Width = 56
      Height = 25
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TMDb API'
      Layout = tlCenter
    end
    object CurrentProgress: TLabel
      Left = 0
      Top = 83
      Width = 889
      Height = 25
      Align = alBottom
      Alignment = taCenter
      AutoSize = False
      Caption = 'Waiting'
      Color = clSilver
      ParentColor = False
      Layout = tlCenter
      ExplicitTop = 95
      ExplicitWidth = 623
    end
    object Image1: TImage
      Left = 350
      Top = 10
      Width = 72
      Height = 72
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000480000
        0048080600000055EDB347000005C54944415478DAED9C4B48244718C7CBE7CC
        A47D62341E5454F011148287042FB9E4E421274502212008898820288485C925
        012112140C2A1E22282C0B6B300842040FB9E4204410721051D144F141061CE2
        735ECEB8F9AAD7366DDBAFEAAAA92ADDFD83D44C4F77F5BF7EF37555F7573566
        20C9F40A148944FE5614A556B417AC0CD106F40236F9509CABC640A2FDA83E44
        1BD00B478FF63A168B7D1508047E12ED493A40999999E8E6E6E6B53909A248B8
        014D5AF4604058B790AE8151AE485F520002364550FCABC1C192258A6401742F
        7A34DD425A06466DA2BC4903C808074B8628120EC82A7A34DD42FA12184D8BF0
        2705202B3858A2A3482820A7E8C12A2F2F47C7C7C7F8650E304AF2F6281C5071
        71313A3B3BB3DD4F64140903E4267A34B5B5B5A1A5A5A5370F507777379A9D9D
        75B5BFA828120288247A34EDEEEEA29A9A9A3707D0EAEA2A6A6D6D253A4E4414
        7107E4257A34259349F5B8B7806CC43B8AB80202365128FC5EE1603D754054D1
        A38927246E8080CD2F50B4D3C2C17AAA805C458FD6784DD9D9D90FB6F184C405
        10B0F9088A3F48E1682A2A2A42E7E7E74F071000E987E247E376374FED56B2CB
        17E91482BF1A6016950210808843619B333E3A3A42959595B6F538C1B183841F
        76C3E1B0ED71F0301C86287C372D800E0E0E0215151511DB06A652E81D454189
        44C28B072A40765A595971BC5B874BF7BBC2C2C26F1D019D9C9C3C2B292919B6
        AB6C626202F5F7F77B82400BC72B24D34643B715894490CFE7B3DD0F987C9811
        0A85BE2E2B2BFB41DB787575858028B1712F1A1C1C442323239610CC3CB00064
        A79E9E1E34353575F75E8DA06834FADCEFF77FC1C3805E4E004400C29A9F9F47
        EDEDEDFF03C23A3D3D7D0991F3192F137D7D7D687C7CDC11006F48939393A8B7
        B7F7351CE33D04407A01903EE701C96DC379021A1D1D450303037770D4D2C4D0
        3FF0D97BE986646C783C1E4781404018203CDAE1514F0FC714105632990C6565
        6595A5CB0C49A37900AAAAAA427B7B7B0FE05802C24AA5529760444987219246
        777575A19999997BDB16161650474707132FB5B5B5686767C7148E2D202C88A4
        3388A4029690BC4444BAA2A8AEAE0E6D6D6D59C2710484757D7D7D0A4FD485AC
        4CC902C8EEB2220284A55FF945636C6C6CECC19DB89BFA580382AB027FF18E70
        5C036205C96B435903224995103DCDD342323614778EF5F5F55C0191E69188D3
        1D5E21D134726868080583C17BDB0A0A0AD0E5E5A5EBF3E7E4E4A8F75A24703C
        01F20A89360A8CC7E339B2DC5CF7CB17BD66203D27CC4821191B487A89D000D6
        8EDDD8D8F03535351125AD68338AAE20B1E8431874F099103CAF1C0F6009C82D
        2451805824F69924ED9D20D15E5E5E00B19AF56036AB6135EF75717181144531
        6D185E1F343D4DBF36339D2B64D30E8847EA16CF6A9496969A9D977A8D355340
        66391D1E80B0ACBE18A922C868B2A5A505ADADAD7187F368009144CFF2F2B23A
        9BB0B8B868B98FB1BEC3C343F5A9DC6E5F2900019B6D28EA9C0091DEFD3A01C2
        B21AC9F0F415BEDC6501E4AA8366B52EC86D9D787F6901A523C9F5A801E1A9DC
        BCBC3CE900EDEFEF87ABABAB3D2D5C600A88C7849F88BBE9470548537373335A
        5F5F77DC4F0A40C066038AF7CD1A9F4AA590DE1BCF797FAC582CA68E9AA20131
        59B9AA293F3F5F7D7E6325DA8E5A2A40AC6F0B7475FA8091A7D55D4C00456104
        537423182D18BDBE0906D1F7C3C304B599D61D07407E6180184E0375434366B4
        7AF51F88FAE9821040266012E0DF743D1C2D28A180C0FB9F507CE0D674676727
        9A9B9BBB6FC0A571AFA0440372D541636F78C8F702C6703EBC2427440A8A6624
        4B3B20E3E5C462653C9CF637283ED1DEDBA53D8403B24A6198F43301F018A385
        633CBFFE7D636323DADEDEB6F27208E7AF7459353B402E1E317E05639FB204E3
        048A65FA9519A0CDCDCD070B1178FF00D70A941040E0E567283A1B1A1AEE5669
        8902E3048AC61793B97959C018FCE17F12B74BEB8F092099C098F8FC188ADFBD
        FAA40224331813BF7F2512895C9FCF574172DC7FCDCA72638A72E1A800000000
        49454E44AE426082}
    end
    object ProgressStep: TLabel
      Left = 730
      Top = 83
      Width = 151
      Height = 25
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0 of 0'
      Layout = tlCenter
    end
    object ProgressDetail: TLabel
      Left = 142
      Top = 81
      Width = 199
      Height = 25
      AutoSize = False
      Caption = 'Detail'
      Layout = tlCenter
    end
    object btStart: TButton
      Left = 8
      Top = 8
      Width = 64
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btStartClick
    end
    object btStop: TButton
      Left = 8
      Top = 33
      Width = 64
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = btStopClick
    end
    object btSwagger: TButton
      Left = 8
      Top = 58
      Width = 64
      Height = 25
      Caption = 'Swagger'
      TabOrder = 2
      WordWrap = True
      OnClick = btSwaggerClick
    end
    object edSecret: TEdit
      Left = 140
      Top = 10
      Width = 200
      Height = 21
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      TabOrder = 3
      OnChange = edSecretChange
    end
    object edSecretBase64: TEdit
      Left = 140
      Top = 35
      Width = 200
      Height = 21
      AutoSize = False
      TabOrder = 4
    end
    object edTMDbAPI: TEdit
      Left = 140
      Top = 60
      Width = 200
      Height = 21
      AutoSize = False
      TabOrder = 5
    end
    object btTimer: TButton
      Tag = 1
      Left = 428
      Top = 8
      Width = 94
      Height = 25
      Caption = 'Disable Timer'
      TabOrder = 6
      OnClick = btTimerClick
    end
    object btRecentProgress: TButton
      Left = 428
      Top = 33
      Width = 42
      Height = 25
      Caption = 'Stats'
      TabOrder = 7
      OnClick = btRecentProgressClick
    end
    object btClear: TButton
      Left = 428
      Top = 58
      Width = 94
      Height = 25
      Caption = 'Clear View'
      TabOrder = 8
      OnClick = btClearClick
    end
    object progDay: TEdit
      Left = 528
      Top = 34
      Width = 72
      Height = 47
      Margins.Top = 0
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 9
      Text = '25'
    end
    object progMonth: TEdit
      Left = 528
      Top = 10
      Width = 72
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 10
      Text = 'May'
    end
    object DateTimePicker1: TDateTimePicker
      Tag = 1
      Left = 606
      Top = 10
      Width = 77
      Height = 21
      Hint = 'Birth'
      Date = 43831.000000000000000000
      Time = 43831.000000000000000000
      Checked = False
      MaxDate = 44196.999988425920000000
      MinDate = 43831.000000000000000000
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnCloseUp = DateTimePicker1CloseUp
    end
    object DateTimePicker2: TDateTimePicker
      Tag = 1
      Left = 606
      Top = 35
      Width = 77
      Height = 21
      Hint = 'Death'
      Date = 43831.000000000000000000
      Time = 43831.000000000000000000
      MaxDate = 44196.999988425920000000
      MinDate = 43831.000000000000000000
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnCloseUp = DateTimePicker2CloseUp
    end
    object DateTimePicker3: TDateTimePicker
      Tag = 1
      Left = 606
      Top = 60
      Width = 77
      Height = 21
      Hint = 'Release'
      Date = 43831.000000000000000000
      Time = 43831.000000000000000000
      MaxDate = 44196.999988425920000000
      MinDate = 43831.000000000000000000
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnCloseUp = DateTimePicker3CloseUp
    end
    object btTop1000: TButton
      Tag = 1
      Left = 689
      Top = 33
      Width = 94
      Height = 25
      Caption = 'Top 1000'
      TabOrder = 14
      OnClick = btTop1000Click
    end
    object btTop5000: TButton
      Tag = 1
      Left = 689
      Top = 58
      Width = 94
      Height = 25
      Caption = 'Top 5000'
      TabOrder = 15
      OnClick = btTop5000Click
    end
    object btAll: TButton
      Left = 496
      Top = 33
      Width = 26
      Height = 25
      Caption = 'Ext'
      TabOrder = 16
      OnClick = btAllClick
    end
    object edtClientVersion: TEdit
      Left = 789
      Top = 35
      Width = 90
      Height = 21
      Alignment = taCenter
      AutoSize = False
      TabOrder = 17
      Text = '511'
      OnChange = edtClientVersionChange
    end
    object btUpdateVersion: TButton
      Left = 787
      Top = 8
      Width = 94
      Height = 25
      Caption = 'Client Ver Chk'
      TabOrder = 18
      OnClick = btUpdateVersionClick
    end
    object btInternal: TButton
      Left = 470
      Top = 33
      Width = 26
      Height = 25
      Caption = 'Int'
      TabOrder = 19
      OnClick = btInternalClick
    end
    object btClean: TButton
      Left = 689
      Top = 8
      Width = 94
      Height = 25
      Caption = 'Clean Cache'
      TabOrder = 20
      OnClick = btCleanClick
    end
    object ckRegenerate: TCheckBox
      Left = 789
      Top = 62
      Width = 97
      Height = 17
      Caption = 'Regenerate'
      Checked = True
      State = cbChecked
      TabOrder = 21
    end
    object btRedoc: TButton
      Left = 8
      Top = 83
      Width = 64
      Height = 25
      Caption = 'Redoc'
      TabOrder = 22
      WordWrap = True
      OnClick = btRedocClick
    end
  end
  object sparqlRelatives: TMemo
    Left = 18
    Top = 336
    Width = 354
    Height = 200
    Lines.Strings = (
      'SELECT ?TMDB WHERE'
      '{'
      '  {'
      '    SELECT ?TMDB WHERE '
      '    {'
      '      wd::PERSON wdt:P22/wdt:P26*/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P25/wdt:P26*/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P25/wdt:P22/wdt:P26*/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P25/wdt:P25/wdt:P26*/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P22/wdt:p22/wdt:P26*/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P22/wdt:P25/wdt:P26*/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P26/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P451 ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P3373* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P3373*/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P26* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P26/wdt:P22/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '  UNION'
      '  {'
      '    SELECT ?TMDB WHERE'
      '    {'
      '      wd::PERSON wdt:P26/wdt:P25/wdt:P40* ?person.'
      '      ?person wdt:P4985 ?TMDB.'
      '    }'
      '  }'
      '}'
      'GROUP BY ?TMDB'
      'LIMIT 1000')
    TabOrder = 5
    Visible = False
    WordWrap = False
  end
  object CacheTimer: TTimer
    Tag = 100
    Enabled = False
    Interval = 10000
    OnTimer = CacheTimerTimer
    Left = 896
    Top = 360
  end
  object StartTimer: TTimer
    Tag = 1
    OnTimer = StartTimerTimer
    Left = 1008
    Top = 360
  end
  object tmrVersionCheck: TTimer
    Interval = 60000
    OnTimer = tmrVersionCheckTimer
    Left = 896
    Top = 416
  end
  object tmrTopUpdate: TTimer
    Interval = 60000
    OnTimer = tmrTopUpdateTimer
    Left = 1008
    Top = 416
  end
  object NetHTTPClient1: TNetHTTPClient
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    HandleRedirects = True
    AllowCookies = True
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 776
    Top = 360
  end
  object tmrWaiting: TTimer
    Enabled = False
    OnTimer = tmrWaitingTimer
    Left = 896
    Top = 480
  end
  object tmrProgress: TTimer
    Interval = 10000
    OnTimer = tmrProgressTimer
    Left = 1008
    Top = 480
  end
end
