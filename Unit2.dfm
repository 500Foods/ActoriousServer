object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Actorious XData Server'
  ClientHeight = 812
  ClientWidth = 1092
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
    1092
    812)
  PixelsPerInch = 96
  TextHeight = 13
  object mmInfo: TMemo
    Left = 308
    Top = 153
    Width = 781
    Height = 656
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object mmStats: TMemo
    Left = 2
    Top = 153
    Width = 304
    Height = 656
    Anchors = [akLeft, akTop, akBottom]
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
    TabOrder = 5
  end
  object sparqlBirthDays: TMemo
    Left = 8
    Top = 282
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
    TabOrder = 0
    Visible = False
    WordWrap = False
  end
  object sparqlDeathDays: TMemo
    Left = 368
    Top = 282
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
    TabOrder = 1
    Visible = False
    WordWrap = False
  end
  object sparqlReleaseDays: TMemo
    Left = 368
    Top = 488
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
    TabOrder = 2
    Visible = False
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 2
    Top = 2
    Width = 1087
    Height = 149
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    BorderWidth = 1
    Color = clSilver
    ParentBackground = False
    TabOrder = 3
    DesignSize = (
      1087
      149)
    object Shape1: TShape
      Left = 406
      Top = 3
      Width = 70
      Height = 70
      Brush.Color = clGray
      Enabled = False
      Pen.Color = clGray
    end
    object lbSecret: TLabel
      Left = 135
      Top = 2
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Secret'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label1: TLabel
      Left = 135
      Top = 26
      Width = 41
      Height = 23
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Base64'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object lbTMDbAPI: TLabel
      Left = 135
      Top = 50
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TMDb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object lblBirthDays: TLabel
      Left = 258
      Top = 74
      Width = 30
      Height = 24
      Hint = 'Birth Days Generated Today'
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
    end
    object lblDeathDays: TLabel
      Left = 258
      Top = 98
      Width = 30
      Height = 24
      Hint = 'Death Days Generated Today'
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
    end
    object lblReleaseDays: TLabel
      Left = 258
      Top = 122
      Width = 30
      Height = 24
      Hint = 'Release Days Generated Today'
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
    end
    object lblSearchPeople: TLabel
      Left = 336
      Top = 74
      Width = 64
      Height = 24
      Cursor = crHandPoint
      Hint = 'Search Index: People'
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
      OnClick = lblSearchPeopleClick
    end
    object lblSearchMovies: TLabel
      Left = 336
      Top = 98
      Width = 64
      Height = 24
      Cursor = crHandPoint
      Hint = 'Search Index: Movies'
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
      OnClick = lblSearchMoviesClick
    end
    object lblSearchTVShow: TLabel
      Left = 337
      Top = 121
      Width = 64
      Height = 24
      Cursor = crHandPoint
      Hint = 'Search Index: TV Shows'
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
      OnClick = lblSearchTVShowClick
    end
    object Label2: TLabel
      Left = 135
      Top = 74
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Birth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label5: TLabel
      Left = 135
      Top = 98
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Death'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label6: TLabel
      Left = 135
      Top = 122
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label7: TLabel
      Left = 294
      Top = 74
      Width = 48
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'People'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label8: TLabel
      Left = 294
      Top = 98
      Width = 48
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Movies'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label9: TLabel
      Left = 294
      Top = 122
      Width = 48
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TV Shows'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object progDay: TEdit
      Left = 407
      Top = 31
      Width = 68
      Height = 41
      Margins.Top = 0
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      Ctl3D = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 22
      Text = '25'
    end
    object progMonth: TEdit
      Left = 407
      Top = 4
      Width = 68
      Height = 27
      Alignment = taCenter
      AutoSize = False
      Color = clWhite
      Ctl3D = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 23
      Text = 'May'
    end
    object btStart: TButton
      Left = 2
      Top = 2
      Width = 52
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btStartClick
    end
    object btStop: TButton
      Left = 2
      Top = 26
      Width = 52
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 1
      OnClick = btStopClick
    end
    object btSwagger: TButton
      Left = 2
      Top = 50
      Width = 52
      Height = 25
      Caption = 'Swagger'
      Enabled = False
      TabOrder = 2
      WordWrap = True
      OnClick = btSwaggerClick
    end
    object edSecret: TEdit
      Left = 180
      Top = 4
      Width = 125
      Height = 21
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      TabOrder = 12
      OnChange = edSecretChange
    end
    object edSecretBase64: TEdit
      Left = 180
      Top = 28
      Width = 220
      Height = 21
      AutoSize = False
      TabOrder = 13
    end
    object edTMDbAPI: TEdit
      Left = 180
      Top = 52
      Width = 220
      Height = 21
      AutoSize = False
      TabOrder = 14
    end
    object btTimer: TButton
      Tag = 1
      Left = 54
      Top = 2
      Width = 77
      Height = 25
      Caption = 'Disable Timer'
      TabOrder = 6
      OnClick = btTimerClick
    end
    object btRecentProgress: TButton
      Left = 54
      Top = 26
      Width = 77
      Height = 25
      Caption = 'Statistics'
      TabOrder = 7
      OnClick = btRecentProgressClick
    end
    object btClear: TButton
      Left = 2
      Top = 98
      Width = 52
      Height = 25
      Caption = 'Clear'
      TabOrder = 4
      OnClick = btClearClick
    end
    object DateTimePickerBirthday: TDateTimePicker
      Tag = 1
      Left = 180
      Top = 76
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
      TabOrder = 15
      OnCloseUp = DateTimePickerBirthdayCloseUp
    end
    object DateTimePickerDeathDay: TDateTimePicker
      Tag = 1
      Left = 180
      Top = 100
      Width = 77
      Height = 21
      Hint = 'Death'
      Date = 43831.000000000000000000
      Time = 43831.000000000000000000
      MaxDate = 44196.999988425920000000
      MinDate = 43831.000000000000000000
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnCloseUp = DateTimePickerDeathDayCloseUp
    end
    object DateTimePickerReleaseDay: TDateTimePicker
      Tag = 1
      Left = 180
      Top = 124
      Width = 77
      Height = 21
      Hint = 'Release'
      Date = 43831.000000000000000000
      Time = 43831.000000000000000000
      MaxDate = 44196.999988425920000000
      MinDate = 43831.000000000000000000
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnCloseUp = DateTimePickerReleaseDayCloseUp
    end
    object btTop1000: TButton
      Tag = 1
      Left = 54
      Top = 98
      Width = 77
      Height = 25
      Caption = 'Top 1000'
      TabOrder = 10
      OnClick = btTop1000Click
    end
    object btTop5000: TButton
      Tag = 1
      Left = 54
      Top = 122
      Width = 77
      Height = 25
      Caption = 'Top 5000'
      TabOrder = 11
      OnClick = btTop5000Click
    end
    object btAll: TButton
      Left = 54
      Top = 74
      Width = 77
      Height = 25
      Caption = 'External'
      TabOrder = 9
      OnClick = btAllClick
    end
    object edtClientVersion: TEdit
      AlignWithMargins = True
      Left = 407
      Top = 100
      Width = 68
      Height = 21
      Alignment = taCenter
      AutoSize = False
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 20
      Text = '1.0.3000'
      OnChange = edtClientVersionChange
    end
    object btUpdateVersion: TButton
      Left = 405
      Top = 78
      Width = 70
      Height = 25
      Caption = 'Client Check'
      TabOrder = 19
      OnClick = btUpdateVersionClick
    end
    object btInternal: TButton
      Left = 54
      Top = 50
      Width = 77
      Height = 25
      Caption = 'Internal'
      TabOrder = 8
      OnClick = btInternalClick
    end
    object btClean: TButton
      Left = 2
      Top = 122
      Width = 52
      Height = 25
      Caption = 'Clean'
      TabOrder = 5
      OnClick = btCleanClick
    end
    object ckRegenerate: TCheckBox
      Left = 320
      Top = 6
      Width = 77
      Height = 17
      Caption = 'Regenerate'
      Checked = True
      State = cbChecked
      TabOrder = 18
    end
    object btRedoc: TButton
      Left = 2
      Top = 74
      Width = 52
      Height = 25
      Caption = 'Redoc'
      Enabled = False
      TabOrder = 3
      WordWrap = True
      OnClick = btRedocClick
    end
    object btEmail: TButton
      Left = 406
      Top = 122
      Width = 70
      Height = 25
      Caption = 'E-Mail'
      Enabled = False
      TabOrder = 21
      OnClick = btEmailClick
    end
    object PanelA: TPanel
      Left = 481
      Top = 3
      Width = 602
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'PanelA'
      TabOrder = 24
      DesignSize = (
        602
        23)
      object shapeProgressBG: TShape
        Left = 0
        Top = 0
        Width = 602
        Height = 23
        Align = alClient
        Brush.Color = clBtnFace
        Pen.Color = clGray
        ExplicitLeft = 384
        ExplicitTop = 12
        ExplicitWidth = 297
        ExplicitHeight = 21
      end
      object shapeProgressFGA: TShape
        Left = 1
        Top = 1
        Width = 400
        Height = 22
        Brush.Color = clMoneyGreen
        Pen.Color = clNone
        Pen.Style = psClear
      end
      object ProgressStepA: TLabel
        Left = 100
        Top = 2
        Width = 494
        Height = 19
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = '0 of 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object CurrentProgressA: TLabel
        Left = 50
        Top = 2
        Width = 450
        Height = 19
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Waiting'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object ProgressDetailA: TLabel
        Left = 8
        Top = 2
        Width = 600
        Height = 19
        AutoSize = False
        Caption = 'Detail'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
    end
    object PanelB: TPanel
      Left = 481
      Top = 27
      Width = 602
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'PanelA'
      TabOrder = 25
      DesignSize = (
        602
        23)
      object Shape2: TShape
        Left = 0
        Top = 0
        Width = 602
        Height = 23
        Align = alClient
        Brush.Color = clBtnFace
        Pen.Color = clGray
        ExplicitLeft = 384
        ExplicitTop = 12
        ExplicitWidth = 297
        ExplicitHeight = 21
      end
      object shapeProgressFGB: TShape
        Left = 1
        Top = 1
        Width = 400
        Height = 22
        Brush.Color = clMoneyGreen
        Pen.Color = clNone
        Pen.Style = psClear
      end
      object ProgressStepB: TLabel
        Left = 100
        Top = 2
        Width = 494
        Height = 19
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = '0 of 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object ProgressDetailB: TLabel
        Left = 8
        Top = 2
        Width = 600
        Height = 19
        AutoSize = False
        Caption = 'Detail'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object CurrentProgressB: TLabel
        Left = 50
        Top = 2
        Width = 450
        Height = 19
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Waiting'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
    end
    object PanelC: TPanel
      Left = 481
      Top = 51
      Width = 602
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'PanelA'
      TabOrder = 26
      DesignSize = (
        602
        23)
      object Shape4: TShape
        Left = 0
        Top = 0
        Width = 602
        Height = 23
        Align = alClient
        Brush.Color = clBtnFace
        Pen.Color = clGray
        ExplicitLeft = 384
        ExplicitTop = 12
        ExplicitWidth = 297
        ExplicitHeight = 21
      end
      object shapeProgressFGC: TShape
        Left = 1
        Top = 1
        Width = 400
        Height = 22
        Brush.Color = clMoneyGreen
        Pen.Color = clNone
        Pen.Style = psClear
      end
      object ProgressStepC: TLabel
        Left = 100
        Top = 2
        Width = 494
        Height = 19
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = '0 of 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object ProgressDetailC: TLabel
        Left = 8
        Top = 2
        Width = 600
        Height = 19
        AutoSize = False
        Caption = 'Detail'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object CurrentProgressC: TLabel
        Left = 50
        Top = 2
        Width = 450
        Height = 19
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Waiting'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
    end
    object PanelD: TPanel
      Left = 481
      Top = 75
      Width = 602
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'PanelA'
      TabOrder = 27
      DesignSize = (
        602
        23)
      object Shape6: TShape
        Left = 0
        Top = 0
        Width = 602
        Height = 23
        Align = alClient
        Brush.Color = clBtnFace
        Pen.Color = clGray
        ExplicitLeft = 384
        ExplicitTop = 12
        ExplicitWidth = 297
        ExplicitHeight = 21
      end
      object shapeProgressFGD: TShape
        Left = 1
        Top = 1
        Width = 400
        Height = 22
        Brush.Color = clMoneyGreen
        Pen.Color = clNone
        Pen.Style = psClear
      end
      object ProgressStepD: TLabel
        Left = 100
        Top = 2
        Width = 494
        Height = 19
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = '0 of 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object ProgressDetailD: TLabel
        Left = 8
        Top = 2
        Width = 600
        Height = 19
        AutoSize = False
        Caption = 'Detail'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object CurrentProgressD: TLabel
        Left = 50
        Top = 2
        Width = 450
        Height = 19
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Waiting'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
    end
    object PanelE: TPanel
      Left = 481
      Top = 99
      Width = 602
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'PanelA'
      TabOrder = 28
      DesignSize = (
        602
        23)
      object Shape8: TShape
        Left = 0
        Top = 0
        Width = 602
        Height = 23
        Align = alClient
        Brush.Color = clBtnFace
        Pen.Color = clGray
        ExplicitLeft = 384
        ExplicitTop = 12
        ExplicitWidth = 297
        ExplicitHeight = 21
      end
      object shapeProgressFGE: TShape
        Left = 1
        Top = 1
        Width = 400
        Height = 22
        Brush.Color = clMoneyGreen
        Pen.Color = clNone
        Pen.Style = psClear
      end
      object ProgressStepE: TLabel
        Left = 100
        Top = 2
        Width = 494
        Height = 19
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = '0 of 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object ProgressDetailE: TLabel
        Left = 8
        Top = 2
        Width = 600
        Height = 19
        AutoSize = False
        Caption = 'Detail'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object CurrentProgressE: TLabel
        Left = 50
        Top = 2
        Width = 450
        Height = 19
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Waiting'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
    end
    object PanelF: TPanel
      Left = 481
      Top = 123
      Width = 602
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'PanelA'
      TabOrder = 29
      DesignSize = (
        602
        23)
      object Shape10: TShape
        Left = 0
        Top = 0
        Width = 602
        Height = 23
        Align = alClient
        Brush.Color = clBtnFace
        Pen.Color = clGray
        ExplicitLeft = 384
        ExplicitTop = 12
        ExplicitWidth = 297
        ExplicitHeight = 21
      end
      object shapeProgressFGF: TShape
        Left = 1
        Top = 1
        Width = 400
        Height = 22
        Brush.Color = clMoneyGreen
        Pen.Color = clNone
        Pen.Style = psClear
      end
      object ProgressStepF: TLabel
        Left = 100
        Top = 2
        Width = 494
        Height = 19
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = '0 of 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object ProgressDetailF: TLabel
        Left = 8
        Top = 2
        Width = 600
        Height = 19
        AutoSize = False
        Caption = 'Detail'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object CurrentProgressF: TLabel
        Left = 50
        Top = 2
        Width = 450
        Height = 19
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Waiting'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Inconsolata'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
    end
  end
  object sparqlRelatives: TMemo
    Left = 8
    Top = 488
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
    TabOrder = 4
    Visible = False
    WordWrap = False
  end
  object CacheTimer: TTimer
    Tag = 100
    Enabled = False
    Interval = 5000
    OnTimer = CacheTimerTimer
    Left = 896
    Top = 360
  end
  object StartTimer: TTimer
    Tag = 1
    Enabled = False
    Interval = 2500
    OnTimer = StartTimerTimer
    Left = 1008
    Top = 360
  end
  object tmrVersionCheck: TTimer
    Enabled = False
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
  object tmrWaiting: TTimer
    Enabled = False
    OnTimer = tmrWaitingTimer
    Left = 896
    Top = 480
  end
  object tmrProgress: TTimer
    OnTimer = tmrProgressTimer
    Left = 1008
    Top = 480
  end
end
