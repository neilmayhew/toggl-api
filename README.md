# toggl-api #

_A Haskell library and utilities for using the Toggl API_

Copyright 2019 - Neil Mayhew \<neil@neil.mayhew.name\>

This is a partial implementation of a client library for the [Toggl](http://www.toggl.com) [API](https://github.com/toggl/toggl_api_docs). So far, it handles only the details endpoint of the `reports` API which is used to fetch time entries. The goal is to allow offline processing of time entries from a local file. The `update-toggl-entries` utility allows entries to be synced to a file using the CLI, and the time entries can then be processed separately according to need. The `Toggl.Main` and `Toggl.Types` modules are available to help accomplish this.

```haskell
import System.Environment (getArgs)
import Toggl.Main (mainFilesWith)
import Toggl.Types (TimeEntry(..))

main :: IO ()
main = do
    files <- getArgs
    flip mainFilesWith files $ \es -> do
      print . sum . map teDur $ es
      return True
```

[hoggl](http://hackage.haskell.org/package/hoggl) is another Toggl API client library for Haskell. I didn't know about it when I started writing this package, and I haven't switched to it because it doesn't preserve timezone information in entry start times, and knowing the _local_ day is important to my workflow.
