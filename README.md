This is a little library that can visually show progress on a terminal that
various threads are doing in a Haskell process.

It's designed for batch-style programs, e.g. applications that run, compute
lots of things and exit, without being interactive.

```
withMonitoring "Computing big thing" $
  computeBigThing
```

This library takes over the terminal and displays all text messages supplied in
`withMonitoring` function calls until they exit. `withMonitoring` can be nested
in which case the parts will be shown on the same row in the terminal output.

`withMonitoringProgress` can additionally display a progress bar for threads.

