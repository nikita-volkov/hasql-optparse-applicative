module Hasql.OptparseApplicative
  ( poolConfig,
    poolSettings,
    connectionSettings,
  )
where

import qualified Attoparsec.Time.Text as AttoparsecTime
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Hasql.Connection as Connection
import Hasql.OptparseApplicative.Prelude
import qualified Hasql.Pool.Config as Pool.Config
import Options.Applicative

-- * Pool

-- | Given a function, which updates the long names, produces a parser of
-- a compiled config.
poolConfig ::
  -- | Option long name modifier.
  --
  -- You can use this function to prefix the name or you can just specify 'id',
  -- if you don't want it changed.
  (String -> String) ->
  Parser Pool.Config.Config
poolConfig modifyName =
  Pool.Config.settings <$> poolSettings modifyName

-- | Given a function, which updates the long names, produces a parser of
-- a list of settings, which you can extend upon or override, and compile to 'Pool.Config.Config' on your own.
poolSettings ::
  -- | Option long name modifier.
  --
  -- You can use this function to prefix the name or you can just specify 'id',
  -- if you don't want it changed.
  (String -> String) ->
  Parser [Pool.Config.Setting]
poolSettings modifyName =
  sequenceA
    [ Pool.Config.size <$> poolSize modifyName,
      Pool.Config.acquisitionTimeout <$> acquisitionTimeout modifyName,
      Pool.Config.agingTimeout <$> connectionLifetime modifyName,
      Pool.Config.idlenessTimeout <$> connectionIdleTime modifyName,
      Pool.Config.staticConnectionSettings <$> connectionSettings modifyName
    ]

poolSize :: (String -> String) -> Parser Int
poolSize modifyName =
  option auto
    . mconcat
    $ [ long (modifyName "pool-size"),
        value 1,
        showDefault,
        help "Amount of connections in the pool"
      ]

acquisitionTimeout :: (String -> String) -> Parser DiffTime
acquisitionTimeout modifyName =
  attoparsedOption AttoparsecTime.diffTime
    . mconcat
    $ [ long (modifyName "pool-acquisition-timeout"),
        value 10,
        showDefault,
        help "How long it takes until the attempt to connect is considered timed out"
      ]

connectionLifetime :: (String -> String) -> Parser DiffTime
connectionLifetime modifyName =
  attoparsedOption AttoparsecTime.diffTime
    . mconcat
    $ [ long (modifyName "pool-connection-lifetime"),
        value (fromIntegral (24 * 60 * 60)),
        showDefault,
        help "Maximal lifetime for connections. Allows to periodically clean up the connection resources to avoid server-side leaks"
      ]

connectionIdleTime :: (String -> String) -> Parser DiffTime
connectionIdleTime modifyName =
  attoparsedOption AttoparsecTime.diffTime
    . mconcat
    $ [ long (modifyName "pool-connection-idle-time"),
        value (fromIntegral (5 * 60)),
        showDefault,
        help "Maximal connection idle time"
      ]

-- * Connection

-- | Given a function, which updates the long names produces a parser
-- of @Hasql.Connection.'Connection.Settings'@.
connectionSettings ::
  -- | Option long name modifier.
  --
  -- You can use this function to prefix the name or you can just specify 'id',
  -- if you don't want it changed.
  (String -> String) ->
  Parser Connection.Settings
connectionSettings modifyName =
  Connection.settings
    <$> host modifyName
    <*> port modifyName
    <*> user modifyName
    <*> password modifyName
    <*> database modifyName

host :: (String -> String) -> Parser ByteString
host modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "host"),
        value "127.0.0.1",
        showDefault,
        help "Server host"
      ]

port :: (String -> String) -> Parser Word16
port modifyName =
  option auto
    $ mconcat
      [ long (modifyName "port"),
        value 5432,
        showDefault,
        help "Server port"
      ]

user :: (String -> String) -> Parser ByteString
user modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "user"),
        value "postgres",
        showDefault,
        help "Username"
      ]

password :: (String -> String) -> Parser ByteString
password modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "password"),
        value "",
        showDefault,
        help "Password"
      ]

database :: (String -> String) -> Parser ByteString
database modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "database"),
        help "Database name"
      ]

-- * Helpers

attoparsedOption :: Attoparsec.Parser a -> Mod OptionFields a -> Parser a
attoparsedOption parser =
  option $ eitherReader $ Attoparsec.parseOnly (parser <* Attoparsec.endOfInput) . fromString
