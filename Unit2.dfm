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
  object mmInfo: TMemo
    Left = 308
    Top = 119
    Width = 815
    Height = 690
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
    Top = 119
    Width = 304
    Height = 690
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
    TabOrder = 0
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
    TabOrder = 1
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
    TabOrder = 2
    Visible = False
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 8
    Top = 2
    Width = 889
    Height = 115
    BevelOuter = bvNone
    BorderWidth = 1
    Color = clSilver
    ParentBackground = False
    TabOrder = 3
    object shapeProgressBG: TShape
      Left = 80
      Top = 85
      Width = 801
      Height = 21
      Brush.Color = clBtnFace
      Pen.Color = clGray
    end
    object shapeProgressFG: TShape
      Left = 81
      Top = 86
      Width = 400
      Height = 20
      Brush.Color = clMoneyGreen
      Pen.Color = clNone
      Pen.Style = psClear
    end
    object CurrentProgress: TLabel
      Left = 80
      Top = 84
      Width = 801
      Height = 21
      Alignment = taCenter
      AutoSize = False
      Caption = 'Waiting'
      Color = clSilver
      ParentColor = False
      Layout = tlCenter
    end
    object lbSecret: TLabel
      Left = 80
      Top = 8
      Width = 36
      Height = 25
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Secret'
      Layout = tlCenter
    end
    object Label1: TLabel
      Left = 80
      Top = 33
      Width = 36
      Height = 25
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Base64'
      Layout = tlCenter
    end
    object lbTMDbAPI: TLabel
      Left = 80
      Top = 58
      Width = 36
      Height = 25
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TMDb'
      Layout = tlCenter
    end
    object Image1: TImage
      Left = 330
      Top = 10
      Width = 72
      Height = 72
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000900000
        00900806000000E746E2B8000011964944415478DAED9D0B6C17451EC7074A5B
        DA0AB414F1C1230145A42056811E9C80D6781A4579884802DE89870808F8888A
        4420F8E0C48A8F08424E1E92A0018F20DA5C0D9A3B0D9C813B0E440CD21A0B07
        2AD552A4054A1F401FF7FB6DBA7F77E7BFFFDDD9DDD999FDEFEE3769A6BBFFFD
        CFCE6FE6F3FFED63667ED38E4432554B4BCB4FEDDAB5EB89FF43DA4E7679FCA6
        A8422CD40A52FFAFAEAEEE929B9B7B567699FCA4082013D5D7D77F9E919171AB
        765FE485F48A2AC3445AEFA3510A30D422BB6C7E51045002C1E5EAB99C9C9CBF
        D0FB81A996F6EDDBA7C82E9F5F140194405AEF03C0E0CDB4F6E3C80BB52902C8
        40757575D3333333D7A9DB0600350140A9B2CBE90745001988F63EAA28888601
        44FB649755B6228028013BCD90C4A83101287A22231140714AE47D545110F501
        868EC92EB34C45006904EC544392A36E3300147A2F146AE36969BD4F7A7A3AB9
        78F162DC319D3A752267CE9CD1EECA0586AA65975D962280DA04EC7C0E49ECAD
        B391F7511579218DEDB20BE01769BDCF15575C414E9C3891F0D8BCBC3CF2EDB7
        DFC6B62380422E606719240BD46D33EFA32AF2426D76CB2E801FA4F53E43860C
        215F7FFDB5E577EEB9E71E525C5C1CDB8E000AA9809D6190FC57DD66F13EAAA2
        B7D311403AEF3366CC18B27DFB76E6EF3EF3CC33A4A8A828B61D462F143A8369
        59BD38B412E5854E0243DD65DB2452A106480BCFEAD5ABC9DCB9736DE7B17FFF
        7E929F9F1FDB0E9B170A95B15A013B69909C57B79D781F5594172A0386F264DB
        274A610628E67DD08B0C1D3AD4715EB5B5B5242B2B2BB61D262F141A4369B9BD
        F7A14579A10DC0D09F65DB2842A104480B0F7A8F2E5DBAB8CE33AC2F16436124
        2DDEDE471505D10C60689DD3BC9245A10308D86982243628DE438042E185026F
        202DAFBC8F2A0AA2F1C050B1D3BC9241A1028865C0985B85CD0B05DA385A5E7B
        1F551444BD81A19F64DBEE95420310B0730492BEEAB6408002ED85026B182DAD
        F7494B4B234D4D4D9E9D0B5F2AE2EB018D528121EF4E2851A10008D8D9034981
        BAEDA5F75115162F1448A36869BDCFE5975F4EAAAAAA3C3FE7800103C8A14387
        62DB1140492A600703243CA76E8BF03EAAC2E0850267102DADF7292C2C243B77
        EE1476EE071F7C906CD8B021B61D01946402764642F2A5BA2DD2FBA8A2BC5033
        30D44176BDF054D0018A799F679F7D962C5FBE5C7819B66DDB46C68F1F1FDB0E
        9A170A94315A013BF8CEE788BA2DC3FBA8A2BC5003309429AF66F82AC800C5BC
        CFCA952BC9E38F3F2EAD2C7BF7EE55A60BA90A92170A8C21B444755BB08AF242
        E5C0D035B2CBC4438104480BCFBE7DFB484141819BECB828A8C35E0361042D2F
        BCCF840913C89A356B9497908B162D221F7DF491ED3C282FF43E30F447D975E5
        568103483B608CC770D5B56BD792E9D3A727FCDCC54CD64078A1A43780164FEF
        D3DCDC4C58DAD80544EF40FEB384571247050A2060A7119274759BE35C2F4B65
        646490F3E7CF5B1E17342F94D485A7C5CBFBD885C7EE39A9FC4B81A181422AC8
        03252540C009BE885B4C34317D683905A8A2A2420930E5544E620B51DA047F73
        01AA1A3EB5E5AD7C0B10408265C3B76F25F077999DEFA6A4A410E3652ECC75ED
        B5D792D2D25257E53E7CF830B9E61AF3573C78B9ABABAB73923DDECD6F01B8CE
        B92A24474907081ABA1F248BE0EF4F3CF2C3A7A69933673AFAAED34B172D162F
        F4DE7BEF91A953A7BA3E577979F96300EC2A2C3E97C2DB943080009479902C24
        36BD8991BEFFFE7BB270E142F2E1871F722B1F2F785439BD844E9E3C992C5DBA
        945C75D555AECE5F5B5B7BA8AAAAEAA5ABAFBEFA6F5C0DA3C4152080A4109297
        E0EF260E79297D588B172FA6C7177357FFFEFD49595919D73C79779F64676793
        65CB9639F6AEAA4ED7D4D4941F3EBCBCA0A060198F723902081AB77D636363AF
        A6A6A68D999999A3A0B21C83889131B052BEFAEA2B1EF638126FEF83C218D318
        6B5A84B0AB062FDDD75D779DAB7C2E5CB85009F7662B57AC58F1CAF3CF3FCF54
        29960D0F15F12200320F6EDCB2DDBCB298346912F9E4934F0880C7BBFE5CA9BA
        BA5AF9757B21D99DB878B37EEFBDF72AF75B6ED4DCDCDC02601DE8DCB9F33860
        E0B8F6338588FAFAFA11A9A9A95B3A74E8D083B8B8AC2120780D87FCA4569C1D
        D9F13E6AF47AD6EF8C1D3B96949494C836D15070E5201F7FFC31B9EDB6DB5CE5
        D3EEB3CF3E1B74FBEDB71F64FD02BEDEBFF9E69BC9EEDDBB65D7816BD98147EB
        4DFAF4E9438E1C3962FB7BC922840AB860EAC6518E387FFEFCC6B4B4B4B89EE1
        6434DE8E580102D74DCE9D3BE7E8BB41ABC3B8AE18F51FA8A0F7B3B2B2742F26
        9E7AEA29F2E69B6FCA2EB3908A30931104389C63DCB87196DF7DE49147C8BA75
        C10813B47EFD7AF2D0430FE9F6E97C544343C3FA8E1D3BEA42B30515221E1E24
        4C5E08A3D8CE9AA51F3800F7BA17E32E7270B7BD016EB0A669F7E100AA975F7E
        59B60DDCC4DAF0F3E7CF27AFBDF69AEB7C921DA0B7DF7E9B3CFAE8A371FBDB25
        1A4A0097B30D70399BA6DD8751D95F7FFD75D9B67011AF86FFE5975FC8659759
        BF58671DEAE147619B3FF9E493BA7D4D4D4D2DF0D4AE0CDA4B789B0D10AD0788
        7497B3A79F7E9ABCF1C61BB26D72258CCAC1E211060F1EAC5BD2299182EC85B0
        AD9F78E209DD3E2D3C28D3E7B49A9A9A8DD9D9D9BAA733A4F1ADB7DE926D9B63
        F16EF0A002B46AD52A327BF6ECB8FDF455CBF241BFB5B515D777ECACDD874B1D
        E14BC364D3C99327496E6EAEE57137DE78233970E000539E410408AF34AFBEFA
        6ADC7EA35B1EA6B7CE50498DF05D5DC70E5EFBB14192495E34F6850B1748870E
        D6D3DD9305A0442F4913DD2F33775B40E59F863C74531CAEBCF24A52595929DB
        6626AD58B18269319569D3A6918D1B3732E73B7AF468B263C70ECBE3B07BE792
        4B2E915D0DA64A34A0CE6CDCB6AD7EAFE6E6E66AF825E568F7F5E8D143791AF1
        BBBCBCD404E13276C30D37188E88B01AF46FBBE314EEC2AB5252522ED5EE1315
        F5CBA9D0531E3F7EDCF2B84D9B3691071E78C076FEC90E90537894639C9C1020
        3A0110E91656F333445E3770320374FDF5D71BAE11CB3ADDC8F1D08D8B172F56
        C1CD635278229606C641EE9D3A75F22C7F94DF00C2C1FFDF7DF75DDC7E3B73D5
        5C0D69054F540D9E48774F64B5E6BA68D1410D12C94DE3E2B0DB175E78C1F238
        5941AE8CD4BB776F72ECD8B1B8FD76273ABA1E130D10D50044BA217D7EF244A2
        BC03CB79FCF224D6B76F5F65FA112D27B364B90CAA6F6D6D6D8644D7027E70D7
        3D7BF6243FFEF8A3E571A9A9A9CA4039374AA6CB9851599D4EB1E6362BA3D560
        269FECCA12D9A8C902104F7894EFF22C9C11444E6789F2104BA3E2D0DC912347
        0A39174A1640D80E389E9B96DBE00EBC01C277FA71A5C457FD5E4C9D31135616
        569A957835A89F01C24BB4D170121E9141B8CF4C0588B0CF2C6EEE8E684F24BA
        4171D21F3E6559A963C78E4AFF9928E1C2324653A9788595F1646A3380920149
        DCDC1E99CB0C18896779F0E9EAECD9B396C77DFAE9A7E4AEBBEE1252075E7A9E
        585E5E15BE2D044B5C080A11107911598C452CD06283E20845AFE5D53D4F5C7E
        5E1A01107584A481DEEF35442C0DB960C102C3312F5E9F5784FD89CAE2453434
        CFA37300446990C4F951AF2A1147D1E1683A2B89582F55E4B9ADCAE155283D21
        E15D30180324716FEA82D6887E0048243C4ADE9E596220112F1B591AD1AB252F
        6503241A1E257F2F33A705FC6030C9B8A90EBC2A14678AB20400F7AA01E7CC99
        A3C43492717E19F028E7F0FA04B400A2A190ECA5F78BEA4ED8BE7D3B1933668C
        27B66150F39A1AEBD898393939E4CC9933DCCE2B0B1EE53C224E420B20FA3D24
        BBE8FD227AC4FDF00488F3AD70E68357E713197B5A5A904D80681424FFA2F73B
        6D60D69E773F0074F4E851D731104DCED51EF811F6CA5F6A94568008A31BFD23
        AE063C1AD87EE79D772A716FBC94E4F147C2D7A7F743985FDDBAA6AAEC56B01F
        2E5FACE5705B16A373949696A60F1C38505C275B9BA4038402888643F26F7ABF
        9B95708C14048064DFF3C49D5BD689690144F990C44D0F60A96896A11B181350
        4480CF5DBB7691112346581EC7F1323D03F89116C1CA3700A18C20E2B0F60453
        3E1889232F2F4FB8CD7686B918D87906A3E70A2FB446BE0208A57D5B8D6F8BF1
        ADB195AC00C2CF71501B0E6F282A2A8A0B59E207B1FC50E851067E582A4A7A01
        686901C2F9EC568DFDC5175F905B6EB94576B1B9C80AA26FBEF946174C3C02C8
        405A805846EF891E2AEBA5AC00A257F989003290DD45E3C204106D6F049081EC
        0074DF7DF7912D5BB6C82E323739F8C14C0486B6C92C7352031424EFC362AF81
        CDFF0380DCF789B890AF000276D643A204F6549F9CCC1424805867C7FA6DD15E
        BF0184A3C0156A7021105C69269170F981D3A74FCB2EB24E070F1E542629DA1D
        AAA12EE2C222EC30C68E635511401A89BC7C61A4599CC7E5C51C2D91DD191140
        1AF10608B363999DCA5B114092C41B2059F3D05901EAD7AF1FF3B25126F9DF0D
        0C498BB99C9400E1FD112EE66625BF03E4B48C7E7A12F30D40DA2730AB4B0F4B
        03C98C062618A06838070AA0C1C9874ACF29067D1C326448C263FD7CF9622D9F
        2A5C836BC68C19B6F2C7E86FDDBA758B6D470011FDE5CB6CDE163E22B304C34C
        16809C94951EFF1D0144D8EF7F644FDE63D19E3D7BC8B061C3988F777B198B00
        22C10208DB9335E6A2D3C09B1140945800EADFBF3F292B2B63CACF8FB1087996
        93CAFF0FC0D03F65D8E90B80809D85902C55B793DD03A14E9D3AA5CC40F5AA8C
        543D1C0380FAC8B0D32F0061682FE5CEB8BCBC5CF13489942C00A9DAB9732719
        356A946EDFAD85856407EC7723BFF489F90520EE83C8FC029057C2A1AD38C455
        5504509B782D35197480E83A88006A939D86C76095252525BA7D151515A457AF
        5EB24D12A208A036390528ECD202D4D8D858989191B1437419A40304ECCC87A4
        48DDF61B4018C97EF8F0E1A4B8B8984C983041767174D202844B92D22B278990
        1F00C228AE18CD95FCF0C30FCAA2AF7E10AE9DFAEEBBEFC6EDC7CB26C67AF683
        7059AD4B2FFD6DC9361997313F0014BB7C611F9776DE930CD173AF12C9CE3054
        AF74C71D772811D754851EA064E97ED04AF62557F68D740410E133BBC30F658F
        0012DC0818101303639AE802B449BAA6AC1802767CA2837985AEB3230AFE5E50
        5EEBE5A9394A7688BBE990C462DB880268EBD6ADA65386948A31F93543B97130
        4EC2974D6BD7AE253367CE14620B05D06E28F64D424EDC26D900E104AACEF83F
        AEE189AB087B290CFC8401A02CD4171AE12863F94D03FB0C1A340843CF796A53
        656525E9DEFDB715D8455FC66403146B005C08D66815611E627CB21A0B75FF77
        073618AE4AA495976BA54D9E3C996CDEBC39B61D5A804486FFA7F40ED4F92C0E
        B6E025CD34CEB0081B23803809C7545BE5E945658349AB21996D562E96A86B76
        140144F80184F3E571DEBCA9D1022A194CAB862461D702CF7B3ECACBF604F32A
        BCB64F95CC48F55321795FDD760BD0BC79F394F9EEA6C64A784F02766268D8F4
        449F4F9932857CF0C107AECE41012434F0A64C8062155B71FC38E9D5BBB7A37C
        F2F3F3C9FEFDFBAD0EEB06957A4A96AD6DF69ADE45631F20F6053AD1AFBFFE4A
        BA76ED1ADB0ECB5A19B10AC5C76B9C0A6347E8B118D6FC7A0CEAD27AFD257136
        E3745BD3423BF1C40F3FFC3059B3664D6C3B740079B0ACC10EA8C34259B631D8
        6EB85A91566EEA2402288170596D8B3954E7A0EEACA7ADFA4450057F8524E12B
        EB9F7FFE593770DE4C114026329AD91067880F2296BAA80B7C119999E8739645
        F2420510BDF465228070513686A5B93B437DD5CAB0C3837A31BDD1BEFFFEFB95
        7E3C236901AAAFAFDF939595355C44996501149B07761C9EC07A534F603858AB
        A1A1C12A9BC100CE4119E5F7B86EB04D4C6FF28C7E70D5D5D5243BFBB7A7F7A0
        2F7919FBA54D9C3851B7502EC30DF28B50374B64945BA4A08A709C896914512D
        484B962C51FE54850620B51218C029833A11BF9C8E644155DD0D896927AF511D
        86062006E9067585555065F8A2AC80F5F808208195904C82AAFB091296677B21
        EBA7CAE81BFA1D24FFB138AC2B186FBD007B88C5F023FC12EA70B4D7E5900110
        46F64E4DF0F114307AB39DFCC2AEBABABAD2CCCCCC01061FB5405D7A1E245B06
        4046BF9CD560EC1CD16509928CEA55C42D8070801A1B1B5BD2D3D3D5F39E041B
        BBBBCA30524CC0103E6CC456160E24406DBF94AD60DB24D1E70E8BA08A71AA78
        830880FE0FCDF7E8C3BCE9A0010000000049454E44AE426082}
      Stretch = True
      Transparent = True
    end
    object ProgressStep: TLabel
      Left = 730
      Top = 84
      Width = 145
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0 of 0'
      Layout = tlCenter
    end
    object ProgressDetail: TLabel
      Left = 85
      Top = 84
      Width = 263
      Height = 21
      AutoSize = False
      Caption = 'Detail'
      Layout = tlCenter
    end
    object lblBirthDays: TLabel
      Left = 685
      Top = 8
      Width = 25
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Layout = tlCenter
    end
    object lblDeathDays: TLabel
      Left = 685
      Top = 33
      Width = 25
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Layout = tlCenter
    end
    object lblReleaseDays: TLabel
      Left = 685
      Top = 58
      Width = 25
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Layout = tlCenter
    end
    object lblSearchPeople: TLabel
      Left = 578
      Top = 8
      Width = 29
      Height = 25
      Cursor = crHandPoint
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Layout = tlCenter
      OnClick = lblSearchPeopleClick
    end
    object Label3: TLabel
      Left = 578
      Top = 33
      Width = 29
      Height = 25
      Cursor = crHandPoint
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Layout = tlCenter
    end
    object Label4: TLabel
      Left = 578
      Top = 58
      Width = 29
      Height = 25
      Cursor = crHandPoint
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Layout = tlCenter
    end
    object progMonth: TEdit
      Left = 508
      Top = 10
      Width = 68
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Color = clWhite
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
      Enabled = False
      TabOrder = 1
      OnClick = btStopClick
    end
    object btSwagger: TButton
      Left = 8
      Top = 58
      Width = 64
      Height = 25
      Caption = 'Swagger'
      Enabled = False
      TabOrder = 2
      WordWrap = True
      OnClick = btSwaggerClick
    end
    object edSecret: TEdit
      Left = 120
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
      Left = 120
      Top = 35
      Width = 200
      Height = 21
      AutoSize = False
      TabOrder = 4
    end
    object edTMDbAPI: TEdit
      Left = 120
      Top = 60
      Width = 200
      Height = 21
      AutoSize = False
      TabOrder = 5
    end
    object btTimer: TButton
      Tag = 1
      Left = 408
      Top = 8
      Width = 94
      Height = 25
      Caption = 'Disable Timer'
      TabOrder = 6
      OnClick = btTimerClick
    end
    object btRecentProgress: TButton
      Left = 408
      Top = 33
      Width = 42
      Height = 25
      Caption = 'Stats'
      TabOrder = 7
      OnClick = btRecentProgressClick
    end
    object btClear: TButton
      Left = 408
      Top = 58
      Width = 42
      Height = 25
      Caption = 'Clear'
      TabOrder = 8
      OnClick = btClearClick
    end
    object progDay: TEdit
      Left = 508
      Top = 34
      Width = 68
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
    object DateTimePickerBirthday: TDateTimePicker
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
      OnCloseUp = DateTimePickerBirthdayCloseUp
    end
    object DateTimePickerDeathDay: TDateTimePicker
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
      OnCloseUp = DateTimePickerDeathDayCloseUp
    end
    object DateTimePickerReleaseDay: TDateTimePicker
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
      OnCloseUp = DateTimePickerReleaseDayCloseUp
    end
    object btTop1000: TButton
      Tag = 1
      Left = 709
      Top = 33
      Width = 74
      Height = 25
      Caption = 'Top 1000'
      TabOrder = 14
      OnClick = btTop1000Click
    end
    object btTop5000: TButton
      Tag = 1
      Left = 709
      Top = 58
      Width = 74
      Height = 25
      Caption = 'Top 5000'
      TabOrder = 15
      OnClick = btTop5000Click
    end
    object btAll: TButton
      Left = 476
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
      Text = '1.0.3000'
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
      Left = 450
      Top = 33
      Width = 26
      Height = 25
      Caption = 'Int'
      TabOrder = 19
      OnClick = btInternalClick
    end
    object btClean: TButton
      Left = 709
      Top = 8
      Width = 74
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
      Enabled = False
      TabOrder = 22
      WordWrap = True
      OnClick = btRedocClick
    end
    object btEmail: TButton
      Left = 450
      Top = 58
      Width = 52
      Height = 25
      Caption = 'E-Mail'
      Enabled = False
      TabOrder = 23
      OnClick = btEmailClick
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
    Interval = 10000
    OnTimer = tmrProgressTimer
    Left = 1008
    Top = 480
  end
end
