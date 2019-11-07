module BookLibrary where

import Prelude

type BookLibraryStartArgs = {
  connectionString :: ConnectionString
}

type State = {
  connection :: RedisConnection
}

serverName :: ServerName State
serverName = ServerName "book_library"

startLink :: BookLibraryStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: BookLibraryStartArgs -> Effect State
init args = do
  connection <- Redis.open args.connectionString
  pure $ { connection }

create :: Book -> Effect (Either String Book)
create book = 
  Gen.doCall serverName \state@{ connection } -> do
    existing <- Redis.get (dbId book.isbn) connection
    case existing of
         Nothing -> do
           Redis.put (dbId book.isbn) book connection
           pure $ CallReply (Right book) state
         Just (gasp :: Book) -> 
           pure $ CallReply (Left "Book with this ISBN already exists") state

dbPrefix :: String
dbPrefix = "books:"

dbId :: String -> DbId
dbId isbn = wrap $ dbPrefix <> isbn
