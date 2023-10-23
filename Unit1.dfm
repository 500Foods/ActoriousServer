object ServerContainer: TServerContainer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 210
  Width = 431
  object XDataServer: TXDataServer
    BaseUrl = 'https://+:10999/actorious'
    Dispatcher = SparkleHttpSysDispatcher
    Pool = XDataConnectionPool
    ModelName = 'Actorious'
    UnknownMemberHandling = Error
    DefaultEntitySetPermissions = [List, Get]
    EntitySetPermissions = <>
    SwaggerOptions.Enabled = True
    SwaggerUIOptions.Enabled = True
    SwaggerUIOptions.ShowFilter = True
    RedocOptions.Enabled = True
    Left = 216
    Top = 16
    object XDataServerCORS: TSparkleCorsMiddleware
      Origin = '*'
    end
    object XDataServerCompress: TSparkleCompressMiddleware
    end
  end
  object XDataConnectionPool: TXDataConnectionPool
    Connection = AureliusConnection
    Size = 50
    Left = 216
    Top = 72
  end
  object AureliusConnection: TAureliusConnection
    Left = 216
    Top = 128
  end
  object SparkleHttpSysDispatcher: TSparkleHttpSysDispatcher
    Left = 64
    Top = 64
  end
end
