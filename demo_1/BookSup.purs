module BookSup where

import Effect
import Erl.Data.List
import Prelude

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "book_sup" init

init :: Effect SupervisorSpec
init = do
  connectionString <- BookConfig.connectionString
  webPort <- BookConfig.webPort
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren ( ( buildChild
                                       # childType Worker
                                       # childId "book_web"
                                       # childStart BookWeb.startLink  { webPort } )
                                       : 
                                       ( buildChild
                                       # childType Worker
                                       # childId "book_library"
                                       # childStart BookLibrary.startLink { connectionString } )
                                        : nil)


